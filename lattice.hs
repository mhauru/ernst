import Control.Monad
import Data.List
import Data.Char
import System.Random

main = run initLattice

initLattice :: [[Spin]]
--initLattice = [[U, D, U, U, U], [U,U,U,U,U], [U,U,U,U,U], [U,U,U,U,U], [U,U,U,U,U]]
initLattice = replicate 30 $ replicate 30 D

run :: [[Spin]] -> IO ()
run l = do
    stringsToIO . latticeToStrings $ l
    putStrLn "Quit?"
    quit <- getLine
    if (map toLower quit == "y" || map toLower quit == "yes")
        then return ()
        else do
            gen <- newStdGen
            let rands = randomRs (0.0,1.0) gen :: [Double]
            run $ update l rands 

update :: [[Spin]] -> [Double] -> [[Spin]]
update l rands = map (map (updateSpin)) $ gather (0,0) l rands

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
updateSpin :: ([Spin], Double) -> Spin
updateSpin (x, r) = if snd ((boltzmannFactors x (10^^(-10) :: Double))!!1) > r then D else U -- ad hoc, TODO generalise

--spinSum :: [Spin] -> Spin
--spinSum x = if (foldl (\a s -> if s==U then a+1 else a-1) 0 x > 0) then U else D

partF :: [Spin] -> Double -> Double
partF (x:xs) beta = sum $ map (\y -> exp ((-1)*y*beta)) $ [neighbourhoodE (s:xs) | s <- [U,D]]

pairE :: Spin -> Spin -> Double
pairE s1 s2 = if s1==s2 then -1 else 1

neighbourhoodE :: [Spin] -> Double
neighbourhoodE (x:xs) = sum $ map (pairE x) xs

boltzmannFactors :: [Spin] -> Double -> [(Spin, Double)]
boltzmannFactors (x:xs) beta = map (\s -> (s, (exp ((-1)*neighbourhoodE(s:xs)*beta))/(partF (x:xs) beta))) $ [U,D]

data Spin = U | D deriving (Eq, Show, Read)

latticeToStrings :: [[Spin]] -> [String]
latticeToStrings l = map (unwords.(map show)) (transpose l)

stringsToIO :: [String] -> IO [()]
stringsToIO l = sequence $ map putStrLn l
