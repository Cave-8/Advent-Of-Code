import Data.Function (on)
import Data.List (groupBy)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Matrix (Matrix, matrix, setElem, (!))
import GHC.OldList (intersect, sortBy)

type ValveID = String
data Valve' = Valve' ValveID Int (M.Map ValveID Int) deriving (Show)
data Valve = Valve Int Int (M.Map Int Int)

-- Pretty print
instance Show Valve where
    show (Valve x y z) = "V: id: " ++ show x ++ ", flow: " ++ show y ++ ", neighbours: " ++ show (M.toList z)

getID :: Valve' -> ValveID
getID (Valve' x _ _) = x

-- Setup functions
createValve :: String -> Valve'
createValve text =
    let txt = words text
        currID = txt !! 1
        currFlow = read (reverse (drop 1 (reverse (drop 5 (txt !! 4)))))
        currIDs = splitOn "," (concat $ drop 9 txt)
        mapD = M.fromList (map (,1) currIDs)
     in Valve' currID currFlow mapD

relabel :: [Valve'] -> M.Map ValveID Int -> [Valve] -> [Valve]
relabel [] _ toReturn = toReturn
relabel ((Valve' id flow neigh) : xs) idMap toReturn =
    let newId = M.findWithDefault 0 id idMap
        newNeigh = M.fromList (map (\(x, y) -> (M.findWithDefault 0 x idMap, y)) (M.toList neigh))
        newV = Valve newId flow newNeigh
     in relabel xs idMap (newV : toReturn)

updateMap :: M.Map (Int, Int) Int -> [(Int, (Int, Int))] -> M.Map (Int, Int) Int
updateMap dists [] = dists
updateMap dists ((idF, (idT, d)) : xs) = updateMap (M.insert (idF, idT) d dists) xs

initialize :: M.Map (Int, Int) Int -> [Valve] -> M.Map (Int, Int) Int
initialize dists [] = dists
initialize dists ((Valve id _ neigh) : xs) =
    let d = [(id, x) | x <- M.toList neigh] ++ [(id, (id, 0))]
        newDists = updateMap dists d
     in initialize newDists xs

clean :: [((Int, Int), Int)] -> Int -> [((Int, Int), Int)]
clean [] _ = []
clean (x@((s, f), d) : xs) id =
    if s == id || f == id || d == 0
        then clean xs id
        else x : clean xs id

cleanup :: M.Map (Int, Int) Int -> Int -> [Valve] -> M.Map (Int, Int) Int
cleanup dists _ [] = dists
cleanup dists idAA ((Valve id flow neigh) : xs) =
    if id /= idAA && flow == 0
        then cleanup (M.fromList $ clean (M.toList dists) id) idAA xs
        else cleanup dists idAA xs

-- Actual problem
-- Naive implementation to find minimum distances between all couples of nodes
floydWarshall :: M.Map (Int, Int) Int -> [(Int, Int, Int)] -> M.Map (Int, Int) Int
floydWarshall dists [] = dists
floydWarshall dists ((k, i, j) : xs) =
    let newD = min (M.findWithDefault 1 (i, j) dists) (M.findWithDefault 1 (i, k) dists + M.findWithDefault 1 (k, j) dists)
        newDists = M.insert (i, j) newD dists
     in floydWarshall newDists xs

paths :: Int -> Int -> Int -> M.Map (Int, Int) Int -> [Int] -> [[Int]]
paths limit curr currDist dists visited =
    let neigh = map (\((_, dest), _) -> dest) (filter (\((s, _), _) -> s == curr) (M.toList dists))
        distances = map (\((_, _), d) -> d) (filter (\((s, _), _) -> s == curr) (M.toList dists))
        toVisitNeigh = filter (\(dest, dist) -> dest `notElem` visited && currDist + 1 + dist <= limit) (zip neigh distances)
        step = zip toVisitNeigh
     in if null toVisitNeigh
            then [[curr]]
            else map (curr :) $ concatMap (\(dest, dist) -> paths limit dest (currDist + 1 + dist) dists (curr : visited)) toVisitNeigh

simulate :: [Int] -> [Valve] -> M.Map (Int, Int) Int -> Int -> Int -> Int -> Int
simulate [x] valves dists remTime currFlow accFlow =
    let currFlow' = currFlow + head (map (\(Valve _ f _) -> f) (filter (\(Valve id _ _) -> id == x) valves))
     in currFlow + remTime * currFlow' + accFlow
simulate (x : xs) valves dists remTime currFlow accFlow =
    let currFlow' = currFlow + head (map (\(Valve _ f _) -> f) (filter (\(Valve id _ _) -> id == x) valves))
        distance = M.findWithDefault 0 (x, head xs) dists
        accFlow' = accFlow + currFlow + distance * currFlow'
        remTime' = remTime - 1 - distance
     in simulate xs valves dists remTime' currFlow' accFlow'

getPair :: [([Int], Int)] -> Int -> Int -> Int -> Int
getPair l@((path, humanScore) : xs) curr len iter
    | curr == len = getPair xs iter (len - 1) (iter + 1)
    | otherwise =
        let (currPath, elephantScore) = l !! curr
         in if null $ path `intersect` currPath then humanScore + elephantScore else getPair l (curr + 1) len iter

-- It does not work on the example due to the way the path search is done (all possible paths <= limit)
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let valves' = map createValve (lines contents)
    let idMap = M.fromList $ zip (map getID valves') [1 .. length valves']
    let idAA = M.findWithDefault 1 "AA" idMap

    -- Valves
    let valves = relabel valves' idMap []
    let initDists = initialize (M.fromList [((x, y), 1000) | x <- [1 .. length valves], y <- [1 .. length valves]]) valves
    -- Distance between valves
    let dists' = floydWarshall initDists [(x, y, z) | x <- [1 .. length valves], y <- [1 .. length valves], z <- [1 .. length valves]]
    -- Cleaned up without 0-flow valves
    let dists = cleanup dists' idAA valves
    -- All non zero valves
    let nonZeroValves = length (filter (\(Valve id f _) -> id /= idAA && f /= 0) valves)

    -- All viable paths for part 1
    let allPaths1 = paths 30 idAA 0 dists []
    let part1 = maximum $ map (\x -> simulate x valves dists 30 0 0) allPaths1

    -- All viable paths for part 2
    let allPaths2 = paths 26 idAA 0 dists []
    let pathScore = sortBy (flip compare `on` snd) (zip (map tail allPaths2) (map (\x -> simulate x valves dists 26 0 0) allPaths2))
    let part2 = getPair pathScore 1 (length pathScore) 0

    print part2