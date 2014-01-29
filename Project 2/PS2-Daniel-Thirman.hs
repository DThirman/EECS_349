import System.IO  
import Debug.Trace
import Control.Monad
import Data.List.Split 
import Data.Char
import Data.Function
import Data.List
import Data.Ord (comparing)

data DecisionNode = Value Int | NodeNom Int [([Char], DecisionNode)] | NodeNum Int [([Int], DecisionNode)]  deriving (Show)

data NomNum = Nominal | Numeric | NomData [Char] | NumData [Int] deriving (Show)

metadata = [Numeric, Numeric, Numeric, Nominal, Nominal, Nominal, Nominal, Nominal, Numeric, Numeric, Numeric, Numeric]

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile 

concatList :: [[Char]] -> [Char]
concatList = (foldl (\acc x -> acc ++ x) "")

main  = do  
	putStrLn ("Do you want to prune? (Y/N)")
	pruneChoice <- getLine
	
	putStrLn ("Training file?")
	trainFile <- getLine
        content <- readFile trainFile
	let fields = (fst (splitFile content))
	let decisionData = (snd (splitFile content))
	putStrLn ("Validation file?")
	valFile <- getLine
	content2 <- readFile valFile
	putStrLn ("Test file?")
	testFile <- getLine
	content3 <- readFile testFile
	let tree = (dTL decisionData [0..11] 0)
	let validationData = (snd (splitFile content2))
	let pruneTree = prune tree validationData
	if pruneChoice =="Y" then (print (treeToDNF pruneTree fields)) else print (treeToDNF tree fields)
	let newFile = (reverse (tail (foldl (\x y -> (x ++"\n" ++ y)) "" (map (\x -> foldl (\x y ->( x++"," ++ y)) "" (reverse (show (traverseTree (if pruneChoice =="Y" then pruneTree else tree) x)): (tail (reverse x)))) ( snd file))))) where file =  (splitFile content3)
	writeFile ("new"++testFile) newFile
	
	putStrLn("Accuracy: ")
	print (findPercent validationData (if pruneChoice =="Y" then pruneTree else tree))
	return 1


prune :: DecisionNode -> [[[Char]]] -> DecisionNode
prune (Value a) validationData = Value a
prune (NodeNum num a) validationData = 	if percent < 0.05 then (Value 0) else if percent > 0.95 then (Value 1) else (NodeNum num (map (\(value, node) -> (value, (prune node (filter (\x -> head value <= (goodRead (x!! num)) && (goodRead (x!!num)) <= last value ) validationData)))) a)) where percent = lastColumnPercent validationData

prune (NodeNom num a) validationData = 	if percent < 0.05 then (Value 0) else if percent > 0.95 then (Value 1) else (NodeNom num (map (\(value, node) -> (value, (prune node (filter (\x -> value  == x !! num) validationData)))) a)) where percent = lastColumnPercent validationData

	
findPercent :: [[[Char]]] -> DecisionNode -> Double
findPercent decisionData tree = (fromIntegral (foldl (+) 0  (map (\x -> ( if show (traverseTree tree x) == (last x) then 1 else 0)) decisionData))) / (fromIntegral (length decisionData))
	

splitFile content = (head linesOfFile, tail linesOfFile) where linesOfFile = map (splitOn ",") (lines content)
	
node = NodeNom 3 [("m", (Value 0)), ("f" , (Value 1)), ("r", node2)]
node2 = NodeNum 2 [([1..10], (Value 1)), ([11..20], (Value 0)), ([21..30], (Value 1))]
	
traverseTree :: DecisionNode -> [[Char]] -> Int
traverseTree (Value a) _ = a
traverseTree node@(NodeNom num a) values = (traverseTree (findCorrectBin node (values !! num)) values)
traverseTree node@(NodeNum num a) values = traverseTree (findCorrectBin node (values !! num)) values
	
findCorrectBin :: DecisionNode -> [Char] -> DecisionNode
findCorrectBin (NodeNom n [x]) a = (snd x)
findCorrectBin (NodeNom n (x:xs)) a = if a == (fst x) then snd x else  findCorrectBin (NodeNom n xs) a
findCorrectBin (NodeNum n [x]) a = (snd x)
findCorrectBin (NodeNum n (x:xs)) a = if  (head (fst x)) <= (goodRead a) && (goodRead a) <= (last (fst x)) then snd x else  findCorrectBin (NodeNum n xs) a

sec :: (a, b, c)  -> b	
sec (_, x, _) = x	

thrd :: (a, b, c)  -> c	
thrd (_, _,x) = x	
	
findPositive :: [Char] -> [[Char]] -> DecisionNode  -> [Char]
findPositive a fields (Value 0) = ""
findPositive a fields(Value 1) = a
findPositive a fields x = a ++ " && " ++ (treeToDNF x fields) 
  
findBest :: [Int] -> [[[Char]]]  -> (Int, Double)
findBest [] _ = (0, 0)
findBest _ [] = (0, 0)
findBest attributes decisionData =  head (sortBy (\ x y -> compare (snd x) (snd y)) (map (\x -> (x , (numBits (findTotal x (metadata!!x) decisionData) (fromIntegral (length decisionData))))) attributes))
  
treeToDNF :: DecisionNode -> [[Char]] -> [Char]
treeToDNF (Value a) fields = (show a)
treeToDNF (NodeNom x a) fields = "(" ++ (reverse (drop 5 ((reverse (concatList [ y ++ ") || (" | y <- (map (\(value, node) -> (findPositive ("(" ++ (fields !! x) ++ " == " ++ value ++ ")") fields node)) a), y /= ""])))))
treeToDNF (NodeNum x a) fields = "(" ++ (reverse (drop 5 ((reverse (concatList [ y ++ ") || (" | y <- (map (\(value, node) -> (findPositive ("(" ++ (show (head value)) ++ " < " ++ (fields !! x) ++ " < " ++ (show (last value)) ++ ")") fields node)) a), y /= ""])))))



rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

quadcect :: [Int]  ->[[Int]]
quadcect [] = [[0]]
quadcect x = let diff = floor (fromIntegral ((last x) - (head x)) /4) in 
	[[head x, (head x) + diff], [(head x) + diff + 1, (head x) + 2 * diff], [(head x) + 2 * diff + 1, (head x) + 3 * diff], [(head x) + 3 * diff + 1, last x]]
			
goodRead :: [Char] -> Int
goodRead a = if a =="?" then -1 else (read a)
		
findTotal :: Int -> NomNum  -> [[[Char]]] -> [(NomNum, Int, Int)]
findTotal x Nominal decisionData =(map (\y -> let pini = [goodRead (last z)| z <- decisionData, z !! x == y] in (NomData y, foldl (+) 0 pini, length pini)) (findNomData decisionData x))
findTotal x Numeric decisionData = (map (\y -> if y == [] then (NumData [0], 0, 0)else let pini = [(goodRead (last z)) | z <- decisionData, head y <= (goodRead (z !! x)) && (goodRead (z !! x)) <= last y] in (NumData y, foldl (+) 0 pini, length pini)) (findNumData decisionData x))


h :: [Double] -> Double
h a = foldl (\x y  ->  (-y * log(y) / log(2)) + x) 0 a 

numBits :: [(NomNum, Int, Int)]-> Double -> Double
numBits a x = (foldl (\y (_, p, t) -> ((fromIntegral t)/x) * h([(fromIntegral p)/(fromIntegral t), (fromIntegral (t - p))/(fromIntegral t)]) + y) 0 a)

newNode :: NomNum -> Int -> [Int] -> [[[Char]]] -> DecisionNode
newNode Numeric a attributes decisionData =  NodeNum a (map (\x-> if x == [] then (x, (Value 0)) else (x, (dTL (filter  (\y -> (head x) <(goodRead (y!!a)) && (goodRead (y !! a)) < (last x)) decisionData) (filter (\y -> (y /= a)) attributes) (modeLastColumn decisionData)))) (findNumData decisionData a))

newNode Nominal a attributes decisionData = NodeNom a (map (\x-> (x, (dTL (filter  (\y ->  x ==  y !! a) decisionData) (filter (\y -> (y /= a)) attributes) (modeLastColumn decisionData)))) (findNomData decisionData a))

findNomData :: [[[Char]]] -> Int -> [[Char]]
findNomData decisionData a = (rmdups (filter (\y -> y /= "?") rowData)) where rowData =  ((transpose decisionData) !! a)

findNumData :: [[[Char]]] -> Int -> [[Int]]
findNumData [] _ = [[-1, -1]]
findNumData decisionData a = let range = (map (\x -> (goodRead x) +0) (filter (\y -> y/= "?") ((transpose decisionData) !! a))) in if range == [] then [[-1,-1]] else (quadcect [minimum range .. maximum range])

dTL :: [[[Char]]] -> [Int] -> Int -> DecisionNode
dTL [] attributes defaultTree = (Value defaultTree)
dTL examples [] defaultTree = if examples == [] then (Value defaultTree) else (Value (modeLastColumn examples))
dTL examples attributes defaultTree = if (length (rmdups (last (transpose examples)))) == 1 then (Value (goodRead (last (examples !! 0)))) else let best = (findBest  attributes examples) in (newNode (metadata !! (fst best)) (fst best) attributes examples)

mode :: [[Char]] -> [Char]
mode [] = ""
mode xs =snd $ head m
    where m = filter (\(a,b) -> a > 1) (modes xs)
	
modes :: [[Char]] -> [(Int, [Char])]
modes [] = [(0, "")]
modes xs = sortBy (comparing $ negate.fst) $ map (\x-> if x ==[] then (0, "") else (length x, head x)) $ (group.sort) xs
 
modeLastColumn :: [[[Char]]] -> Int
modeLastColumn [] = -1
modeLastColumn x =  (round (lastColumnPercent x))

lastColumnPercent :: [[[Char]]] -> Double
lastColumnPercent [] = -1
lastColumnPercent x =  (fromIntegral ((foldl (\x y -> x + (goodRead y)) 0 values)) / (fromIntegral (length values))) where values = (last (transpose x))


