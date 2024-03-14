import Data.Set qualified as S
import Data.Map qualified as M
import Data.Maybe (fromJust)

type Coord = (Int, Int)
type Blizzard = (Coord, Char)
type Timestamp = (Coord, Int)

-- Parsing
parseBlizzards :: (String, Int) -> [Blizzard]
parseBlizzards (str, row) =
    let strIndexed = zip str [0 ..]
        onlyBlizzards = filter (\(c, col) -> c == '^' || c == '>' || c == 'v' || c == '<') strIndexed
        finalCoords = map (\(c, col) -> ((row, col), c)) onlyBlizzards
     in finalCoords

-- Blizzards management
moveBlizzard :: Blizzard -> Int -> Int -> Blizzard
moveBlizzard ((currR, currC), dir) numRows numCols =
    let (newR, newC) = execMovement (currR, currC) dir numRows numCols
     in ((newR, newC), dir)
  where
    execMovement (r, c) d numR numC
        | d == '^' = if (r - 1) == 0 then (numR - 2, c) else (r - 1, c)
        | d == 'v' = if (r + 1) == numR - 1 then (1, c) else (r + 1, c)
        | d == '<' = if (c - 1) == 0 then (r, numC - 2) else (r, c - 1)
        | d == '>' = if (c + 1) == numC - 1 then (r, 1) else (r, c + 1)

precompute :: [Blizzard] -> Int -> Int -> Int -> Int -> M.Map Int (S.Set Coord) -> M.Map Int (S.Set Coord)
precompute blizzards rows cols currI numIter blizIndex
    | currI == numIter = let blizzards' = S.fromList $ map (fst . (\b -> moveBlizzard b rows cols)) blizzards
                         in M.insert currI blizzards' blizIndex
    | otherwise = let blizzards' = map (\b -> moveBlizzard b rows cols) blizzards
                      blizIndex' = M.insert currI (S.fromList $ map fst blizzards') blizIndex
                  in precompute blizzards' rows cols (currI + 1) numIter blizIndex'

-- Parts 1 and 2
pathfinding :: S.Set Coord -> M.Map Int (S.Set Coord) -> Timestamp -> Coord -> Coord -> Int -> Int -> [Timestamp] -> S.Set Timestamp -> Int -> [Int]
pathfinding innerBox blizzards curr@((currR, currC), currT) start end rows cols toVisit visited numTrips
    | (currR, currC) == end && numTrips == 0 = currT : pathfinding innerBox blizzards (end, currT) start end rows cols [] S.empty 1
    | (currR, currC) == start && numTrips == 1 = currT : pathfinding innerBox blizzards (start, currT) start end rows cols [] S.empty 2
    | (currR, currC) == end && numTrips == 2 = [currT] 
    | otherwise =
        let nextPos = map (\(r, c) -> ((currR + r, currC + c), currT + 1)) [(0, 1), (1, 0), (0, -1), (-1, 0), (0, 0)]
            validPos = filter (\n@(pos, time) -> pos `S.member` innerBox && n `S.notMember` visited && n `notElem` toVisit && pos `S.notMember` fromJust (M.lookup currT blizzards)) nextPos
            toVisit' = toVisit ++ validPos
            visited' = S.fromList (curr : validPos) `S.union` visited
         in pathfinding innerBox blizzards (head toVisit') start end rows cols (tail toVisit') visited' numTrips

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let blizzards = concat $ zipWith (curry parseBlizzards) (words contents) [0 ..]
    let (rows, cols) = (length (words contents), length (head (words contents)))
    let (start, end) = ((0, 1), (rows - 1, cols - 2))
    let innerBox = S.fromList ([(x, y) | x <- [1 .. rows - 2], y <- [1 .. cols - 2]] ++ [start] ++ [end])
    let precBlizzards = precompute blizzards rows cols 0 1000 M.empty

    let problem = pathfinding innerBox precBlizzards (start, 0) start end rows cols [] S.empty 0

    -- Part 1
    print (head problem)

    -- Part 2
    print (last problem)