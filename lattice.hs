import Control.Monad
import Data.List
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
    let latticeWidth = 29 --TODO Make these readable from somewhere
    let latticeHeight = 50
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

updateAndDraw :: S.Seq (S.Seq Spin) -> ([Int],[Int]) -> [Double] -> DrawWindow -> CommVars -> GC -> GC -> IO ()
updateAndDraw l (risW,risH) rds dw cv gcUp gcDown = do
    let ([x],risW') = splitAt 1 risW
    let ([y],risH') = splitAt 1 risH
    let ([r],rds') = splitAt 1 rds
    t <- withMVar (temp cv) $ return
    coups <- withMVar (couplings cv) $ return
    let t' = if t==0 then 10^^(-3) else t --"absolute zero"
    let beta = 1/t'
    let neigh = neighbourhood l x y
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

--------

updateSpin :: (Spin,[Spin]) -> Spin -> Double -> [Double] -> Double -> Spin
updateSpin (s1,neigh) s2 beta coups r = if s2E < s1E || r < boltzmannFactor then s2 else s1
    where s1E = neighbourhoodE (s1,neigh) coups
          s2E = neighbourhoodE (s2,neigh) coups
          boltzmannFactor = exp $ (-1) * beta * (s2E - s1E)

neighbourhood :: S.Seq (S.Seq Spin) -> Int -> Int -> (Spin,[Spin])
neighbourhood l x y = (S.index (S.index l x) y, S.index (S.index l (right x)) y : S.index (S.index l x) (down y) : S.index (S.index l (left x)) y : S.index (S.index l x) (up y) : [])
    where    --Periodic boundaries
    right x = if x == S.length l - 1 then 0 else x+1
    left x = if x == 0 then S.length l - 1 else x-1
    up y = if y == 0 then S.length (S.index l x) - 1 else y-1
    down y = if y == S.length (S.index l x) - 1 then 0 else y+1

--The couplings are passed as a list, feel free to assign each of them to whatever purpose
--you like, but be careful not to request couplings out of range.
--Add sliders as needed, a new coupling will be added automatically.

--Ising model
pairE :: Spin -> Spin -> [Double] -> Double
pairE s1 s2 coups = (if s1/=s2 then -0.01 else 0.01)*coups!!0 --Bogus units

--External field
singleE :: Spin -> [Double] -> Double
singleE s coups = (if s==Down then -0.01 else 0.01)*coups!!1

neighbourhoodE :: (Spin,[Spin]) -> [Double] -> Double
neighbourhoodE (x,xs) coups = (sum $ map (\s -> pairE x s coups) xs) + singleE x coups

--------

data Spin = Up | Down deriving (Eq, Show, Read)

downLattice :: Int -> Int -> S.Seq (S.Seq Spin)
downLattice w h = S.replicate w $ S.replicate h Down

upLattice :: Int -> Int -> S.Seq (S.Seq Spin)
upLattice w h = S.replicate w $ S.replicate h Up

randomLattice :: [Double] -> Int -> Int -> S.Seq (S.Seq Spin)
randomLattice rands w h = S.fromList $ [S.fromList $ map (\d -> if d<0.5 then Up else Down) $ take h $ drop (h*i) rands | i <- [0..w]]

--------

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
    whaatBt :: Button}

drawLattice :: DrawWindow -> CommVars -> GC -> GC -> S.Seq (S.Seq Spin) -> IO ()
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
    [whaatBt] <- mapM (xmlGetWidget xml castToButton) ["whaatBt"]
    tempScl <- xmlGetWidget xml castToVScale "tempScl"
    --Add new couplings here as needed
    couplingScl1 <- xmlGetWidget xml castToHScale "couplingScl1"
    couplingScl2 <- xmlGetWidget xml castToHScale "couplingScl2"
    couplingScl3 <- xmlGetWidget xml castToHScale "couplingScl3"
    canvas <- xmlGetWidget xml castToDrawingArea "canvas"
    return $ GUI mw canvas tempScl [couplingScl1, couplingScl2, couplingScl3] whaatBt

connectGui :: GUI -> CommVars -> IO ()
connectGui gui cv = do
    onDestroy (mainWin gui) mainQuit
    onClicked (whaatBt gui) mainQuit -- doWhaat
    onRangeValueChanged (tempScl gui) $ updateTempVar gui cv
    sequence $ map (\scl -> onRangeValueChanged scl $ updateCouplingsVar gui cv) $ couplingScls gui
    afterSizeAllocate (canvas gui) $ (\a -> do updateSizeVar gui cv
                                               scheduleRedraw gui cv)
    return ()

updateTempVar :: GUI -> CommVars -> IO()
updateTempVar gui cv = do modifyMVar_ (temp cv) (\a -> rangeGetValue $ tempScl gui)

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
