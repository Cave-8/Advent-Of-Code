import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Maybe (fromJust, isJust)
import Data.Sequence qualified as S

-- Data type and functions
data Monkey = Monkey1 String Integer | Monkey2 String String (Integer -> Integer -> Integer) String

isMonkey1 :: Monkey -> Bool
isMonkey1 (Monkey1 _ _) = True
isMonkey1 (Monkey2{}) = False

getName :: Monkey -> String
getName (Monkey1 x _) = x
getName (Monkey2 x _ _ _) = x

getM1 :: Monkey -> String
getM1 (Monkey2 _ x _ _) = x

getOp :: Monkey -> (Integer -> Integer -> Integer)
getOp (Monkey2 _ _ x _) = x

getM2 :: Monkey -> String
getM2 (Monkey2 _ _ _ x) = x

-- Parsing
parseMonkey :: Integer -> String -> String -> [String]
parseMonkey 1 x _ = concatMap (splitOn ": ") $ words x
parseMonkey 2 x sym =
    concatMap
        (filter (/= "") . splitOn sym)
        (concatMap (splitOn ":") $ words x)

parseMonkeys :: [String] -> [Monkey]
parseMonkeys [] = []
parseMonkeys (x : xs)
    | "+" `isInfixOf` x = let m = parseMonkey 2 x "+" in Monkey2 (head m) (m !! 1) (+) (m !! 2) : parseMonkeys xs
    | "-" `isInfixOf` x = let m = parseMonkey 2 x "-" in Monkey2 (head m) (m !! 1) (-) (m !! 2) : parseMonkeys xs
    | "*" `isInfixOf` x = let m = parseMonkey 2 x "*" in Monkey2 (head m) (m !! 1) (*) (m !! 2) : parseMonkeys xs
    | "/" `isInfixOf` x = let m = parseMonkey 2 x "/" in Monkey2 (head m) (m !! 1) div (m !! 2) : parseMonkeys xs
    | otherwise = let m = parseMonkey 1 x " " in Monkey1 (take 4 $ head m) (read (m !! 1)) : parseMonkeys xs

-- Problem part 1
problem1 :: S.Seq Monkey -> Integer -> M.Map String Integer -> M.Map String Integer
problem1 mToFind currIndex mKnown
    | S.length mToFind == 0 = mKnown
    | currIndex == fromIntegral (S.length mToFind) = problem1 mToFind 0 mKnown
    | otherwise =
        let currM = mToFind `S.index` fromIntegral currIndex
            (el1, el2) = (getM1 currM, getM2 currM)
            (num1, num2) = (el1 `M.lookup` mKnown, el2 `M.lookup` mKnown)
            (mToFind', index') = if isJust num1 && isJust num2 then (S.deleteAt (fromIntegral currIndex) mToFind, currIndex) else (mToFind, currIndex + 1)
            mKnown' = if isJust num1 && isJust num2 then M.insert (getName currM) (getOp currM (fromJust num1) (fromJust num2)) mKnown else mKnown
         in problem1 mToFind' index' mKnown'

-- Problem part 2
findValToMatch :: S.Seq Monkey -> M.Map String Integer -> Monkey -> Integer
findValToMatch mToFind mKnown rootM =
    let iter = problem1 mToFind 0 mKnown
        num = getM2 rootM `M.lookup` iter
     in fromJust num

findPath :: String -> [Monkey] -> [[String]]
findPath start monkeys =
    let (currM, stop) = head $ map (\x -> if isMonkey1 x then (x, 1) else (x, 2)) (filter (\x -> getName x == start) monkeys)
     in if stop == 1
            then [[getName currM]]
            else map (getName currM :) $ findPath (getM1 currM) monkeys ++ findPath (getM2 currM) monkeys

getPoint :: Integer -> S.Seq Monkey -> M.Map String Integer -> String -> Integer
getPoint x mToFind mKnown idM =
    let mKnown' = M.insert "humn" x mKnown
        test = problem1 mToFind 0 mKnown'
        resTest = idM `M.lookup` test
     in fromJust resTest

binSearch :: Integer -> Integer -> Integer -> S.Seq Monkey -> M.Map String Integer -> [String] -> Integer
binSearch obj low high mToFind mNum path
    | getPoint mid mToFind mNum (path !! 1) > obj = binSearch obj (mid + 1) high mToFind mNum path
    | getPoint mid mToFind mNum (path !! 1) < obj = binSearch obj low (mid - 1) mToFind mNum path
    | otherwise = mid
  where
    mid = low + ((high - low) `div` 2)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let monkeys = parseMonkeys (lines contents)
    -- Monkeys with known number
    let monkeyNum = M.fromList (map (\(Monkey1 id num) -> (id, num)) (filter isMonkey1 monkeys))
    -- Monkeys with unknwon number
    let monkeyToFind = S.fromList (filter (not . isMonkey1) monkeys)
    -- Root monkey
    let monkeyRoot = head $ filter (\(Monkey2 x y w z) -> x == "root") (filter (not . isMonkey1) monkeys)
    let finalMap = problem1 monkeyToFind 0 monkeyNum
    let part1 = fromJust ("root" `M.lookup` finalMap)
    print part1

    -- Path from root to "humn"
    let path = head $ filter (\x -> head x == "root" && last x == "humn") (findPath "root" monkeys)
    -- Value to match in root
    let valToMatch = findValToMatch monkeyToFind monkeyNum monkeyRoot
    -- If we sample some values we observe that function decreases with higher inputs in a monotonic way
    -- we can exploit binary search (the interval can be much smaller, I kept the general case)
    let minB = 0
    let maxB = 1000000000000000
    let start = minB + (maxB - minB) `div` 2
    -- Due to the way I applied Haskell `div` it rounds down possible results (we can see it from sampling),
    -- this generates equal results from three values in three values.
    -- To avoid wrong results I built a window from (res - 3, ..., res + 3) and picked the first result that is equal to valToMatch
    let res = binSearch valToMatch minB maxB monkeyToFind monkeyNum path
    let part2 = head $ filter (\x -> getPoint x monkeyToFind monkeyNum (path !! 1) == valToMatch) [res - 3 .. res + 3]
    print part2