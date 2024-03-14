import Data.Char (isDigit)
import Data.List (group)
import Data.List.Split (splitOn)
import Data.Set qualified as Set
import GHC.Base (maxInt, minInt)
import GHC.OldList (sort)

fillCoords :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
fillCoords (x, y) (a, b)
    | x == a && y > b = [(q, w) | q <- [x .. x], w <- [b .. y]]
    | x == a && y < b = [(q, w) | q <- [x .. x], w <- [y .. b]]
    | y == b && x > a = [(q, w) | q <- [a .. x], w <- [y .. y]]
    | y == b && x < a = [(q, w) | q <- [x .. a], w <- [y .. y]]

completeCoords :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
completeCoords [x, y] toReturn = toReturn ++ fillCoords x y
completeCoords (x : xs) toReturn = completeCoords xs (toReturn ++ fillCoords x (head xs))

toCoords :: String -> String -> [Int] -> (Int, Int)
toCoords [] currNum toReturn = (head toReturn, read currNum)
toCoords (x : xs) currNum toReturn
    | isDigit x = toCoords xs (currNum ++ [x]) toReturn
    | otherwise = toCoords xs [] (toReturn ++ [read currNum])

clean :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
clean [] toReturn = toReturn
clean (x : xs) toReturn = if x `elem` xs then clean xs toReturn else clean xs (toReturn ++ [x])

routine1 :: Set.Set (Int, Int) -> (Int, Int) -> Int -> Set.Set (Int, Int)
routine1 rocks (x, y) steps
    | steps >= 1000 = rocks
    | (x + 1, y) `Set.notMember` rocks = routine1 rocks (x + 1, y) (steps + 1)
    | (x + 1, y - 1) `Set.notMember` rocks = routine1 rocks (x + 1, y - 1) (steps + 1)
    | (x + 1, y + 1) `Set.notMember` rocks = routine1 rocks (x + 1, y + 1) (steps + 1)
    | otherwise = Set.insert (x, y) rocks

routine2 :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
routine2 rocks (x, y)
    | (x + 1, y) `Set.notMember` rocks = routine2 rocks (x + 1, y)
    | (x + 1, y - 1) `Set.notMember` rocks = routine2 rocks (x + 1, y - 1)
    | (x + 1, y + 1) `Set.notMember` rocks = routine2 rocks (x + 1, y + 1)
    | otherwise = Set.insert (x, y) rocks

sandFall1 :: Set.Set (Int, Int) -> Set.Set (Int, Int)
sandFall1 rocks =
    let rocks' = routine1 rocks (0, 500) 0
     in if rocks' == rocks then rocks' else sandFall1 rocks'

sandFall2 :: Set.Set (Int, Int) -> Int -> Set.Set (Int, Int)
sandFall2 rocks floorH =
    let rocks' = routine2 rocks (0, 500)
     in if (0, 500) `Set.member` rocks' then rocks' else sandFall2 rocks' floorH

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let split = map (splitOn " -> ") (lines contents)
    let coords = map (map (\x -> toCoords x [] [])) split
    let completedCoords = map (`completeCoords` []) coords
    let rocks = Set.fromList (clean (map (\(x, y) -> (y, x)) (concat completedCoords)) [])

    let part1 = Set.size (sandFall1 rocks) - length rocks
    print part1

    let floorH = maximum (map fst (Set.toList rocks)) + 2
    let rocks' = Set.fromList (Set.toList rocks ++ [(floorH, x) | x <- [-1000 .. 1000]])

    let part2 = Set.size (sandFall2 rocks' floorH) - length rocks'
    print part2