import Data.Char (ord)
import Data.Function (on)
import Data.List (sortBy)
import Data.Matrix (Matrix (..), fromLists, setElem, toLists, (!))
import Data.Maybe (fromJust, mapMaybe)
import GHC.Base (maxInt)

type Coord = (Int, Int)

isValid :: Coord -> Matrix Int -> Int -> Bool
isValid c hill val = (hill ! c) <= val + 1

inbound :: Coord -> Matrix Int -> Bool
inbound (x, y) hill = x >= 1 && y >= 1 && x <= nrows hill && y <= ncols hill

neighbours :: Coord -> Matrix Int -> [Coord]
neighbours (x, y) hill = filter (`inbound` hill) [(x - 1, y), (x, y + 1), (x + 1, y), (x, y - 1)]

findM :: Int -> Matrix Char -> [(Int, Int)] -> (Int, Int)
findM 1 hill (x : xs) = if hill ! x == 'S' then x else findM 1 hill xs
findM 2 hill (x : xs) = if hill ! x == 'E' then x else findM 2 hill xs

findA :: Matrix Char -> [(Int, Int)] -> [(Int, Int)]
findA hill [] = []
findA hill (x : xs) = if hill ! x == 'a' then x : findA hill xs else findA hill xs

execBfs :: Matrix Int -> Coord -> Coord -> [Coord] -> [Coord] -> Int
execBfs hillInt start end queue visited =
    let hillDist = fromLists $ map (map (const 0)) (toLists hillInt)
     in bfs hillInt hillDist start end [] []

bfs :: Matrix Int -> Matrix Int -> Coord -> Coord -> [Coord] -> [Coord] -> Int
bfs hillInt hillDist start end queue visited =
    let hillInt' = setElem 0 start hillInt
     in bfs' hillInt' hillDist start end [start] [start]

bfs' :: Matrix Int -> Matrix Int -> Coord -> Coord -> [Coord] -> [Coord] -> Int
bfs' hillInt hillDist start end queue visited =
    if null queue || head queue == end
        then hillDist ! end
        else
            let curr = head queue
                toVisit = filter (`notElem` visited) (filter (\c -> isValid c hillInt (hillInt ! curr)) (neighbours curr hillInt))
                visited' = visited ++ toVisit
                queue' = tail queue ++ toVisit
                hillDist' = updateDist hillDist ((hillDist ! curr) + 1) toVisit
             in bfs' hillInt hillDist' start end queue' visited'

updateDist :: Matrix Int -> Int -> [Coord] -> Matrix Int
updateDist m _ [] = m
updateDist m val (x : xs) =
    let m' = setElem val x m
     in updateDist m' val xs

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let hill = fromLists $ lines contents

    let start = findM 1 hill [(x, y) | x <- [1 .. nrows hill], y <- [1 .. ncols hill]]
    let end = findM 2 hill [(x, y) | x <- [1 .. nrows hill], y <- [1 .. ncols hill]]

    let matrixS = setElem 'a' start hill
    let matrixE = setElem 'z' end matrixS

    let hillInt = fromLists $ map (map (\x -> ord x - 97)) (toLists matrixE)
    let hillDist = fromLists $ map (map (const 0)) (toLists matrixE)
    let dist1 = bfs hillInt hillDist start end [] []

    let startingPoints = findA matrixE [(x, y) | x <- [1 .. nrows hill], y <- [1 .. ncols hill]]
    let dist2 = minimum $ filter (/= 0) $ map (\x -> execBfs hillInt x end [] []) startingPoints

    print dist1
    print dist2