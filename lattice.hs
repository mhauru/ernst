import Control.Monad
import Data.List
import Data.Char
import System.Random
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.GC
import Control.Concurrent
import Network.Socket.Internal

main :: IO ()
main = withSocketsDo $ 
    do initGUI
       gui <- loadGlade "lattice.glade"
       connectGui gui
       widgetShowAll $ mainWin gui
       timeoutAdd (yield >> return True) 30
       dw <- widgetGetDrawWindow $ canvas gui
       gcUp <- gcNew dw 
       gcDown <- gcNew dw
       gcSetValues gcDown newGCValues { foreground = red }
       forkIO $ run initLattice gui gcUp gcDown
       mainGUI
    where
        red = Color 65535 0 0

run :: [[Spin]] -> GUI -> GC -> GC -> IO ()
run l gui gcUp gcDown = do
    drawLattice (canvas gui) gcUp gcDown l
    gen <- newStdGen
    let rands = randomRs (0.0,1.0) gen :: [Double]
    t <- postGUISync $ rangeGetValue $ tempScl gui 
    let t2 = if t==0 then 10^^(-10) else t --10^^-10 counts here as absolute zero
    run (update l (1/t2) rands) gui gcUp gcDown--Bogus units

initLattice :: [[Spin]]
initLattice = replicate 100 $ replicate 100 Down --TODO make lattice size adjustable

update :: [[Spin]] -> Double -> [Double] -> [[Spin]]
update l beta rands = map (map (\x -> updateSpin x beta)) $ gather (0,0) l rands

--A somewhat uninformatively named function, this one takes a lattice of spins, a starting position and a list of random numbers,
--and groups them together to a lattice of tuplets that has first a random number and then the neighbourhood of the lattice site
--in question as a list.
gather :: (Int,Int) -> [[Spin]] -> [Double] -> [[([Spin], Double)]]
gather (x,y) l (r:rs)
    | bottom && rborder = [[(neighbours, r)]] 
	| bottom = [(neighbours, r)] : (gather (x+1,0) l rs)
	| otherwise = ([(neighbours, r)] ++ (head (gather (x,y+1) l rs))) : (tail (gather (x,y+1) l rs))
	where
        neighbours = l!!x!!y : l!!(right x)!!y : l!!x!!(down y) : l!!(left x)!!y : l!!x!!(up y) : []
        rborder = x == length l - 1
        bottom = y == length (l!!x) - 1 
        right x = if rborder then 0 else x+1    --Periodic boundaries
        left x = if x == 0 then length l - 1 else x-1
        up y = if y == 0 then length (l!!x) - 1 else y-1
        down y = if bottom then 0 else y+1

-- The order in which the neighbourhood comes in the list can be seen from neighbours.
-- Additional next-neighbours etc. could be added to the end of the list, following the same ordering convention.
updateSpin :: ([Spin], Double) -> Double -> Spin
updateSpin (x, r) beta = if snd ((boltzmannFactors x beta)!!1) > r then Down else Up -- ad hoc, TODO generalise

--spinSum :: [Spin] -> Spin
--spinSum x = if (foldl (\a s -> if s==Up then a+1 else a-1) 0 x > 0) then Up else Down

partF :: [Spin] -> Double -> Double
partF (x:xs) beta = sum $ map (\y -> exp ((-1)*y*beta)) $ [neighbourhoodE (s:xs) | s <- [Up,Down]]

pairE :: Spin -> Spin -> Double
pairE s1 s2 = if s1==s2 then -1 else 1 --Bogus units

neighbourhoodE :: [Spin] -> Double
neighbourhoodE (x:xs) = sum $ map (pairE x) xs

boltzmannFactors :: [Spin] -> Double -> [(Spin, Double)]
boltzmannFactors (x:xs) beta = map (\s -> (s, (exp ((-1)*neighbourhoodE(s:xs)*beta))/(partF (x:xs) beta))) $ [Up,Down]

data Spin = Up | Down deriving (Eq, Show, Read)

latticeToStrings :: [[Spin]] -> [String]
latticeToStrings l = map (unwords.(map show)) (transpose l)

stringsToIO :: [String] -> IO [()]
stringsToIO l = sequence $ map putStrLn l

data GUI = GUI {
      mainWin :: Window,
      canvas :: DrawingArea,
      tempScl :: VScale,
      whaatBt :: Button}

drawLattice :: DrawingArea -> GC -> GC -> [[Spin]] -> IO ()
drawLattice can gcUp gcDown l =
    do
        (width, height) <- postGUISync $ widgetGetSize can
        let size = minimum [div width (length l), div height (length (l!!0))] --Assuming rectangular lattice
        dw <- postGUISync $ widgetGetDrawWindow can
        let gcMat = map (map (\s -> if s==Up then gcUp else gcDown)) l
        let posMat = [iterate (\(a,b) -> (a,b+size)) (x,0) | x <- iterate (+ size) 0]
        postGUIAsync $ drawWindowBeginPaintRect dw $ Rectangle 0 0 width height
        postGUIAsync $ sequence_ $ concat $ zipWith (zipWith (\gc (x,y) -> drawRectangle dw gc True x y size size)) gcMat posMat
        postGUIAsync $ drawWindowEndPaint dw

loadGlade :: String -> IO GUI
loadGlade gladepath =
    do
        Just xml <- xmlNew gladepath
        mw <- xmlGetWidget xml castToWindow "mainWindow"
        [whaatBt] <- mapM (xmlGetWidget xml castToButton) ["whaatBt"]
        tempScl <- xmlGetWidget xml castToVScale "tempScl"
        canvas <- xmlGetWidget xml castToDrawingArea "canvas"
        return $ GUI mw canvas tempScl whaatBt

connectGui gui =
    do
        onDestroy (mainWin gui) mainQuit
        onClicked (whaatBt gui) mainQuit -- doWhaat

