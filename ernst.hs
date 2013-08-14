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
    initGUI
    gui <- loadGlade "lattice.glade"
    temp <- newMVar 0.0
    couplings <- newMVar []
    size <- newMVar (0,0)
    reqRedraw <- newMVar True
    let cv = CommVars temp couplings size reqRedraw
    connectGui gui cv
    widgetShowAll $ mainWin gui
    initCommVars gui cv
    dw <- widgetGetDrawWindow $ canvas gui
    gcUp <- gcNew dw 
    gcDown <- gcNew dw
    gcSetValues gcUp newGCValues{ foreground = red }
    let latticeWidth = 39 --TODO Make these readable from somewhere
    let latticeHeight = 39
    let l = downLattice latticeWidth latticeHeight
    gen <- newStdGen
    let risW = randomRs (0,latticeWidth-1) gen :: [Int]
    gen <- newStdGen
    let risH = randomRs (0,latticeHeight-1) gen :: [Int]
    gen <- newStdGen
    let rds = randomRs (0.0,1.0) gen :: [Double]
    --timeoutAdd (yield >> return True) 30 --Unnecessary as we are using -threaded
    forkIO $ updateAndDraw l (risW,risH) rds dw cv gcUp gcDown
    mainGUI
    where red = Color 65535 0 0

updateAndDraw :: Lattice Spin -> ([Int],[Int]) -> [Double] -> DrawWindow -> CommVars -> GC -> GC -> IO ()
updateAndDraw l (risW,risH) rds dw cv gcUp gcDown = do
    let ([x],risW') = splitAt 1 risW
    let ([y],risH') = splitAt 1 risH
    let ([r],rds') = splitAt 1 rds
    t <- withMVar (temp cv) $ return
    coups <- withMVar (couplings cv) $ return
    let t' = if t==0 then 10^^(-5) else t --"absolute zero"
    let beta = 1/t'
    let neigh = neighbourhood l (x,y)
    let cand = if S.index (S.index l x) y == Up then Down else Up --Candidate for the new state
    let s' = updateSpin neigh cand beta coups r
    let l' = S.update x (S.update y s' (S.index l x)) l --Can this be done without two calls to x l?
    let gc = if s'==Up then gcUp else gcDown
    (width, height) <- withMVar (canvasSize cv) $ return
    let size = minimum [div width (S.length l), div height (S.length (S.index l 0))]
    reqRedraw <- modifyMVar (requestRedraw cv) (\a -> return (False, a))
    --If the window has been resized redraw the whole lattice, otherwise just the new spin
    let drawAction = if reqRedraw --TODO why doesn't this redraw the whole lattice at first go?
                        then drawLattice dw cv gcUp gcDown l'
                        else drawRectangle dw gc True (x*size) (y*size) size size
    postGUISync drawAction  --Would like this to be async, but that breaks stuff.
                            --Now the physics halts to wait for a call that everything got drawn ok,
                            --which is an unnecessarily strict requirement for helping drawing keep up with physics.
    updateAndDraw l' (risW',risH') rds' dw cv gcUp gcDown


-------- PHYSICS --------

updateSpin :: (Spin,[Spin],[Spin]) -> Spin -> Double -> [Double] -> Double -> Spin
updateSpin (s1,nearest,diagonal) s2 beta coups r = if s2E < s1E || r < boltzmannFactor then s2 else s1
    where s1E = neighbourhoodE (s1,nearest,diagonal) coups
          s2E = neighbourhoodE (s2,nearest,diagonal) coups
          boltzmannFactor = exp $ (-1) * beta * (s2E - s1E)

neighbourhood :: Lattice Spin -> (Int,Int) -> (Spin,[Spin],[Spin])
neighbourhood l (x,y) = (at l here (x,y), nearest, diagonal)
    where nearest = map (\dir -> at l dir (x,y)) [east, south, west, north]
          diagonal = map (\dir -> at l dir (x,y)) [nE, sE, sW, nW]

--The couplings are passed as a list, feel free to assign each of them to whatever purpose
--you like, but be careful not to request couplings out of range. TODO could I check against that?
--Add sliders as needed, a new coupling will be added to the list automatically.

--External field
singleE :: Spin -> [Double] -> Double
singleE s coups = (if s==Down then -1 else 1)*coups!!0

--Ising model
nearPairE :: Spin -> Spin -> [Double] -> Double
nearPairE s1 s2 coups = (if s1==s2 then -1 else 1)*coups!!1

--Energy of diagonally connected spins
diagPairE :: Spin -> Spin -> [Double] -> Double
diagPairE s1 s2 coups = (if s1==s2 then -1 else 1)*coups!!2

neighbourhoodE :: (Spin,[Spin],[Spin]) -> [Double] -> Double
neighbourhoodE (x,nearest,diagonal) coups = singleE x coups + nearestE + diagonalE
    where nearestE = sum $ map (\s -> nearPairE x s coups) nearest
          diagonalE = sum $ map (\s -> diagPairE x s coups) diagonal

--------

data Spin = Up | Down deriving (Eq, Show, Read)

downLattice :: Int -> Int -> Lattice Spin
downLattice w h = S.replicate w $ S.replicate h Down

upLattice :: Int -> Int -> Lattice Spin
upLattice w h = S.replicate w $ S.replicate h Up

randomLattice :: [Double] -> Int -> Int -> Lattice Spin
randomLattice rands w h = S.fromList $ [S.fromList $ map (\d -> if d<0.5 then Up else Down) $ take h $ drop (h*i) rands | i <- [0..w]]

--------

type Lattice a = S.Seq (S.Seq a)

at :: Lattice a -> (Lattice a -> (Int,Int) -> (Int,Int)) -> (Int,Int) -> a
at l dir (x,y) = S.index (S.index l x') y'
   where (x',y') = dir l (x,y) 

--A pretty useless function just to make the collection of directions consistent and complete
here :: Lattice a -> (Int,Int) -> (Int,Int)
here l = id

--Periodic boundary conditions
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

nE :: Lattice a -> (Int,Int) -> (Int,Int)
nE l = (north l).(east l)

nW :: Lattice a -> (Int,Int) -> (Int,Int)
nW l = (north l).(west l)

sE :: Lattice a -> (Int,Int) -> (Int,Int)
sE l = (south l).(east l)

sW :: Lattice a -> (Int,Int) -> (Int,Int)
sW l = (south l).(west l)


-------- GUI STUFF -------- 

--MVars for communication between physics and GUI
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

drawLattice :: DrawWindow -> CommVars -> GC -> GC -> Lattice Spin -> IO ()
drawLattice dw cv gcUp gcDown l = do
    (width, height) <- withMVar (canvasSize cv) $ return
    let size = minimum [div width (S.length l), div height (S.length (S.index l 0))] --Assuming rectangular lattice
    let gcMat = Foldable.toList $ fmap (Foldable.toList . (fmap (\s -> if s==Up then gcUp else gcDown))) l
    let posMat = [iterate (\(a,b) -> (a,b+size)) (x,0) | x <- iterate (+ size) 0]
    drawWindowBeginPaintRect dw $ Rectangle 0 0 width height
    sequence_ $ concat $ zipWith (zipWith (\gc (x,y) -> drawRectangle dw gc True x y size size)) gcMat posMat
    drawWindowEndPaint dw

loadGlade :: String -> IO GUI
loadGlade gladepath = do
    Just xml <- xmlNew gladepath
    mw <- xmlGetWidget xml castToWindow "mainWindow"
    [quitBt] <- mapM (xmlGetWidget xml castToButton) ["quitBt"]
    tempScl <- xmlGetWidget xml castToVScale "tempScl"
    tempLbl <- xmlGetWidget xml castToLabel "tempLbl"
    --Add new a new couplingScl here if needed and a new coupling will be available for the energy functions
    couplingScl1 <- xmlGetWidget xml castToHScale "couplingScl1"
    couplingScl2 <- xmlGetWidget xml castToHScale "couplingScl2"
    couplingScl3 <- xmlGetWidget xml castToHScale "couplingScl3"
    canvas <- xmlGetWidget xml castToDrawingArea "canvas"
    return $ GUI mw canvas tempScl [couplingScl1, couplingScl2, couplingScl3] quitBt tempLbl

connectGui :: GUI -> CommVars -> IO ()
connectGui gui cv = do
    onDestroy (mainWin gui) mainQuit
    onClicked (quitBt gui) mainQuit
    onRangeValueChanged (tempScl gui) $ updateTempVar gui cv
    sequence $ map (\scl -> onRangeValueChanged scl $ updateCouplingsVar gui cv) $ couplingScls gui
    afterSizeAllocate (canvas gui) $ (\a -> do updateSizeVar gui cv
                                               scheduleRedraw gui cv)
    return ()

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
    modifyMVar_ (requestRedraw cv) (\a -> return True)
    return ()
