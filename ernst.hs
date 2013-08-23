import Paths_ernst
import Control.Monad
import Data.List
import Numeric
import qualified Data.Sequence as S
import qualified Data.Foldable as Foldable (toList) 
import System.Random
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.GC
import Control.Concurrent

-- NB: temp refers to temperature, not temporary
main :: IO ()
main = do
    --Initialise GUI elements
    initGUI
    gladePath <- getDataFileName "ernst.glade"
    --gladePath <- return "ernst.glade"
    gui <- loadGlade gladePath 
    --Create the MVars that the event and physics threads use to communicate. The values are just place holders.
    temp <- newMVar 1.0
    couplings <- newMVar []
    size <- newMVar (1,0)
    reqRedraw <- newMVar True
    let cv = CommVars temp couplings size reqRedraw
    --Create listeners that adjust the MVars as needed, and initialise the MVars.
    connectGui gui cv
    widgetShowAll $ mainWin gui
    initCommVars gui cv
    --dw and gcUp/gcDown are passed to the physics loop so that it can call the redraws it needs.
    dw <- widgetGetDrawWindow $ canvas gui
    gcUp <- gcNew dw 
    gcDown <- gcNew dw
    gcSetValues gcUp newGCValues{ foreground = red }
    --Initialise the lattice l
    let lWidth = 39 --TODO Make these readable from somewhere
    let lHeight = 39
    let l = downLattice lWidth lHeight
    --Three infinite lists of random numbers. Ints risW and risH for the coordinates on the lattice,
    --Doubles rds for the probabilities in the physics simulation.
    gen <- newStdGen
    let risW = randomRs (0,lWidth-1) gen :: [Int]
    gen <- newStdGen
    let risH = randomRs (0,lHeight-1) gen :: [Int]
    gen <- newStdGen
    let rds = randomRs (0.0,1.0) gen :: [Double]
    --Start the physics thread and begin listening for events in the GUI.
    forkIO $ updateAndDraw l (risW,risH) rds dw cv gcUp gcDown
    mainGUI
    where red = Color 65535 0 0

{-updateAndDraw picks a new random lattice site and a new candidate spin for it
- (simple, because these are spin half particles so we can only flip or not flip the spin,
- but for richer degrees of freedom the new candidate would be picked at random too).
- It then asks updateSpin whether the new candidate or the old spin should be kept.
- If the spin is changed or the GUI has requested completely redrawing the lattice,
- the appropirate draw requested is posted to the main thread with postGUISync.
- Finally it recurses.
- Note that postGUISync waits for the redraw to return before continuing and that
- if no redrawn is requested by the GUI, only the updated spin is redrawn.
-}
updateAndDraw :: Lattice Spin -> ([Int],[Int]) -> [Double] -> DrawWindow -> CommVars -> GC -> GC -> IO ()
updateAndDraw l (risW,risH) rds dw cv gcUp gcDown = do
    let x:risW' = risW
    let y:risH' = risH
    let r:rds' = rds
    t <- withMVar (temp cv) return
    coups <- withMVar (couplings cv) return
    let beta = 1/t --t cannot be zero because it's 10**(slider position)
    let neigh = neighbourhood l (x,y)
    let s = l `at` (x,y)
    let cand = if s == Up then Down else Up --Candidate for the new state
    let s' = updateSpin neigh cand beta coups r
    let l' = if s' == s then l else S.update x (S.update y s' (S.index l x)) l
    redrawRequested <- modifyMVar (requestRedraw cv) (\ a -> return (False, a))
    if redrawRequested --If the window has been resized redraw the whole lattice
        then postGUISync $ drawLattice dw cv gcUp gcDown l' --TODO why doesn't this redraw the whole lattice on the first go?
        else if s' /= s --If the spin has changed, redraw that spin
                then do (width, height) <- withMVar (canvasSize cv) $ return
                        let size = minimum [div width (latticeWidth l), div height (latticeHeight l)]
                        let gc = if s' == Up then gcUp else gcDown
                        postGUISync $ drawRectangle dw gc True (x*size) (y*size) size size
                else return () --No redrawing necessary
    updateAndDraw l' (risW',risH') rds' dw cv gcUp gcDown


-------- PHYSICS --------

{-updateSpin takes a spin s1 with its neighbourhood and the new candidate spin s2 and gives back one or the other:
- If the s2 is energetically favourable, s2 is returned.
- Otherwise, the probability to return s2 is the boltzmannFactor, which depends on inverse temperature and the 
- energy difference between the s1 and s2 configurations.
-}
updateSpin :: (Spin,[Spin],[Spin]) -> Spin -> Double -> [Double] -> Double -> Spin
updateSpin (s1,nearest,diagonal) s2 beta coups r = if s2E < s1E || r < boltzmannFactor then s2 else s1
    where s1E = neighbourhoodE (s1,nearest,diagonal) coups
          s2E = neighbourhoodE (s2,nearest,diagonal) coups
          boltzmannFactor = exp $ (-1) * beta * (s2E - s1E)

{-neighbourhood of a spin s is a triple that has the following elements:
- First s itself
- second a list of the spins immediately neighbouring s,
- third a list of the spins diagonally neihbouring s.
-}
neighbourhood :: Lattice Spin -> (Int,Int) -> (Spin,[Spin],[Spin])
neighbourhood l (x,y) = (atDir l here (x,y), nearest, diagonal)
    where nearest = map (\dir -> atDir l dir (x,y)) [east, south, west, north]
          diagonal = map (\dir -> atDir l dir (x,y)) [nE, sE, sW, nW]

{-The following set of functions named somethingE give the energies or different spin configurations,
- such as the energy of a single spin by itself, the interaction energy between neighbouring spins, etc.
- The last one, neighbourhoodE, uses the others to calculate the total energy of a spin living in its neighbourhood.
-
- The various functions use different coupling constants to determine the energies.
- These couplings are passed as a list [Double] and can be used here in whatever way suitable.
- Note that at the moment there's no check to make sure that the given coupling constant exists in the list,
- so calling coups!!213214231 will cause a runtime error. TODO fix this.
- If more couplings are needed, see the documentation for the function loadGlade for instructions on how to do this.
-}

--External field
singleE :: Spin -> [Double] -> Double
singleE s coups = (if s==Down then -1 else 1)*coups!!0

--Ising model
nearPairE :: Spin -> Spin -> [Double] -> Double
nearPairE s1 s2 coups = (if s1==s2 then -1 else 1)*coups!!1

--Energy of diagonally neighbouring spins
diagPairE :: Spin -> Spin -> [Double] -> Double
diagPairE s1 s2 coups = (if s1==s2 then -1 else 1)*coups!!2

neighbourhoodE :: (Spin,[Spin],[Spin]) -> [Double] -> Double
neighbourhoodE (x,nearest,diagonal) coups = singleE x coups + nearestE + diagonalE
    where nearestE = sum $ map (\s -> nearPairE x s coups) nearest
          diagonalE = sum $ map (\s -> diagPairE x s coups) diagonal


-------- DATA TYPES --------

data Spin = Up | Down deriving (Eq, Show, Read)

type Lattice a = S.Seq (S.Seq a)

latticeWidth :: Lattice a -> Int
latticeWidth = S.length

latticeHeight :: Lattice a -> Int
latticeHeight l = S.length $ S.index l 0 --Lattices are assumed rectangular. TODO make a check for this somewhere.

--Returns the value (spin) of the lattice point at (x,y).
at :: Lattice a -> (Int,Int) -> a
l `at` (x,y) = S.index (S.index l x) y

--As above, but takes a direction to move to from (x,y).
--Used to implement periodic boundary conditions and make it easy to construct neighbourhoods of points.
atDir :: Lattice a -> (Lattice a -> (Int,Int) -> (Int,Int)) -> (Int,Int) -> a
atDir l dir (x,y) = S.index (S.index l x') y'
   where (x',y') = dir l (x,y) 

--A pretty useless function just to make the collection of directions consistent and complete
here :: Lattice a -> (Int,Int) -> (Int,Int)
here l = id

--Cardinal directions, periodic boundary conditions
east :: Lattice a -> (Int,Int) -> (Int,Int)
east l (x,y) = (x',y)
    where x' = if x == S.length l - 1 then 0 else x+1
   
west :: Lattice a -> (Int,Int) -> (Int,Int)
west l (x,y) = (x',y)
    where x' = if x == 0 then S.length l - 1 else x-1

north :: Lattice a -> (Int,Int) -> (Int,Int)
north l (x,y) = (x,y')
    where y' = if y == 0 then S.length (S.index l x) - 1 else y-1

south :: Lattice a -> (Int,Int) -> (Int,Int)
south l (x,y) = (x,y')
    where y' = if y == S.length (S.index l x) - 1 then 0 else y+1

--Ordinal directions
nE :: Lattice a -> (Int,Int) -> (Int,Int)
nE l = (north l).(east l)

nW :: Lattice a -> (Int,Int) -> (Int,Int)
nW l = (north l).(west l)

sE :: Lattice a -> (Int,Int) -> (Int,Int)
sE l = (south l).(east l)

sW :: Lattice a -> (Int,Int) -> (Int,Int)
sW l = (south l).(west l)

--Functions to create lattices initialised to different states
downLattice :: Int -> Int -> Lattice Spin
downLattice w h = S.replicate w $ S.replicate h Down

upLattice :: Int -> Int -> Lattice Spin
upLattice w h = S.replicate w $ S.replicate h Up

randomLattice :: [Double] -> Int -> Int -> Lattice Spin
randomLattice rands w h = S.fromList $ [S.fromList $ map randToSpin $ take h $ drop (h*i) rands | i <- [0..w]]
    where randToSpin rand
            | rand<0.5  = Up
            | otherwise = Down


-------- GUI STUFF -------- 

--MVars for communication between the physics and GUI threads.
--requestRedraw is for there for the GUI to be able to tell the physics loop that the
--canvas size has changed and the whole lattice needs to be redrawn once.
data CommVars = CommVars {
    temp :: MVar Double,
    couplings :: MVar [Double],
    canvasSize :: MVar (Int, Int),
    requestRedraw :: MVar Bool}

data GUI = GUI {
    mainWin :: Window,
    canvas :: DrawingArea,
    tempScl :: VScale,
    couplingScls :: [HScale],
    quitBt :: Button,
    tempLbl :: Label}

--Redraws the whole lattice
drawLattice :: DrawWindow -> CommVars -> GC -> GC -> Lattice Spin -> IO ()
drawLattice dw cv gcUp gcDown l = do
    (width, height) <- withMVar (canvasSize cv) $ return
    let size = minimum [div width (latticeWidth l), div height (latticeHeight l)] --The dimension of the square that represents a single spin
    let spinToGC s
            | s == Up   = gcUp
            | otherwise = gcDown
    let gcMat = Foldable.toList . fmap (Foldable.toList . (fmap spinToGC)) $ l --Map the lattice to a matrix of GCs
    let coordinateList = iterate (+ size) 0 --[0, size, 2*size, ..]
    let posMat = [[(x,y) | y <- coordinateList] | x <- coordinateList] --Infinite matrix of tuples with elements posMat_ij = (i*size, j*size)
    --Each GC in gcMat corresponds to a spin. The corresponding matrix element in postMat holds the coordinates
    --of this spin on the drawing canvas. zip these to obtain a matrix of rectangles to draw.
    let rectangleMat = zipWith (zipWith (\ gc (x,y) -> drawRectangle dw gc True x y size size)) gcMat posMat
    drawWindowBeginPaintRect dw $ Rectangle 0 0 width height
    sequence_ $ concat $ rectangleMat --Draw all the rectangles in the matrix.
    drawWindowEndPaint dw

--loadGlade loads the Glade XML file that holds the description of the GUI elements and maps those to a GUI data type.
--If new couplings are needed for the physics it is sufficient to add a new couplingScl to the Glade file and
--xmlGetWidget it here so that it is included in the returned GUI. Only two lines of code need to be changed!
loadGlade :: String -> IO GUI
loadGlade gladepath = do
    Just xml <- xmlNew gladepath
    mw <- xmlGetWidget xml castToWindow "mainWindow"
    [quitBt] <- mapM (xmlGetWidget xml castToButton) ["quitBt"]
    tempScl <- xmlGetWidget xml castToVScale "tempScl"
    tempLbl <- xmlGetWidget xml castToLabel "tempLbl"
    couplingScl1 <- xmlGetWidget xml castToHScale "couplingScl1"
    couplingScl2 <- xmlGetWidget xml castToHScale "couplingScl2"
    couplingScl3 <- xmlGetWidget xml castToHScale "couplingScl3"
    canvas <- xmlGetWidget xml castToDrawingArea "canvas"
    return $ GUI mw canvas tempScl [couplingScl1, couplingScl2, couplingScl3] quitBt tempLbl

--Set up event listeners that update the MVars in CommVars when for example sliders are moved or the window is resized.
connectGui :: GUI -> CommVars -> IO ()
connectGui gui cv = do
    onDestroy (mainWin gui) mainQuit
    onClicked (quitBt gui) mainQuit
    onRangeValueChanged (tempScl gui) $ updateTempVar gui cv
    sequence $ map (\scl -> onRangeValueChanged scl $ updateCouplingsVar gui cv) $ couplingScls gui
    afterSizeAllocate (canvas gui) $ (\a -> do updateSizeVar gui cv
                                               scheduleRedraw gui cv)
    return ()

--Temp means temperature, not temporary!
--Note that the temperature is 10**val, so that the slider has a logarithmic scale.
--This also guarantees the temperature can not be 0, so the inverse temperature is always well defined.
updateTempVar :: GUI -> CommVars -> IO()
updateTempVar gui cv = do modifyMVar_ (temp cv) (\a -> do 
                          val <- rangeGetValue $ tempScl gui
                          let newTemp = 10**val
                          labelSetLabel (tempLbl gui) $ showEFloat (Just 2) newTemp ""
                          return newTemp)

updateCouplingsVar :: GUI -> CommVars -> IO()
updateCouplingsVar gui cv = do modifyMVar_ (couplings cv) (\a -> sequence $ map rangeGetValue (couplingScls gui))
    
updateSizeVar :: GUI -> CommVars -> IO()
updateSizeVar gui cv = do modifyMVar_ (canvasSize cv) (\a -> widgetGetSize $ canvas gui)

scheduleRedraw :: GUI -> CommVars -> IO()
scheduleRedraw gui cv = do modifyMVar_ (requestRedraw cv) (\a -> return True)

initCommVars :: GUI -> CommVars -> IO ()
initCommVars gui cv = do
    updateTempVar gui cv
    updateCouplingsVar gui cv
    updateSizeVar gui cv --This actually appears needless, but playing safe
    --This should cause a redraw of the whole lattice at startup but doesn't work, TODO.
    modifyMVar_ (requestRedraw cv) (\a -> return True)
    return ()

