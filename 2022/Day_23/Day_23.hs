import Data.Map qualified as M
import Data.Set qualified as S

type Coord = (Int, Int)

-- Parsing
getElves :: (String, Int) -> [Coord]
getElves (str, row) =
    let strIndexed = zip str [1 ..]
        onlyElves = filter (\(c, col) -> c == '#') strIndexed
        finalCoords = map (\(_, col) -> (row, col)) onlyElves
     in finalCoords

-- Directions management
-- Check directions (0 = North, 1 = South, 2 = West, 3 = East)
checkDir :: Coord -> [Coord] -> Int -> Bool
checkDir (r, c) elves dir
    | dir == 0 = not ((r - 1, c - 1) `S.member` elves') && not ((r - 1, c) `S.member` elves') && not ((r - 1, c + 1) `S.member` elves')
    | dir == 1 = not ((r + 1, c - 1) `S.member` elves') && not ((r + 1, c) `S.member` elves') && not ((r + 1, c + 1) `S.member` elves')
    | dir == 2 = not ((r - 1, c - 1) `S.member` elves') && not ((r, c - 1) `S.member` elves') && not ((r + 1, c - 1) `S.member` elves')
    | dir == 3 = not ((r - 1, c + 1) `S.member` elves') && not ((r, c + 1) `S.member` elves') && not ((r + 1, c + 1) `S.member` elves')
  where
    elves' = S.fromList elves

-- Calculate destination coordinates
moveTo :: Coord -> Int -> Coord
moveTo (r, c) dir
    | dir == 0 = (r - 1, c)
    | dir == 1 = (r + 1, c)
    | dir == 2 = (r, c - 1)
    | dir == 3 = (r, c + 1)
    | otherwise = (r, c)

rotate :: [Int] -> Int -> [Int]
rotate xs n = take (length xs) $ drop (length xs + n) $ cycle xs

whereToMove :: Coord -> [Coord] -> [Int] -> (Coord, Int)
whereToMove (r, c) elves dirs =
    let whereTo = map (\d -> (d, checkDir (r, c) elves d)) dirs
        notMoving = length (filter (\(_, dir) -> not dir) whereTo) == 4 || length (filter snd whereTo) == 4
     in if notMoving then ((r, c), -1) else ((r, c), fst (head (filter snd whereTo)))

-- Part 1
problem1 :: [Coord] -> Int -> Int -> Int
problem1 elves currRound numRound
    | currRound == numRound = rectangle elves - length elves
    | otherwise =
        let currDef = currRound `mod` 4
            orderOfDir = rotate [0, 1, 2, 3] currDef
            movingElves = map (\(r, c) -> whereToMove (r, c) elves orderOfDir) elves
            startAndDest = map (\((r, c), dir) -> ((r, c), moveTo (r, c) dir)) movingElves
            destsFreq = map fst (filter (\(_, freq) -> freq >= 2) (M.toList (foldr ((\key -> M.insertWith (+) key 1) . snd) M.empty startAndDest)))
            (notMoving, effectivelyMoving) = (map fst (filter (\(start, dest) -> start == dest || dest `elem` destsFreq) startAndDest), map snd (filter (\(start, dest) -> start /= dest && dest `notElem` destsFreq) startAndDest))
            elves' = notMoving ++ effectivelyMoving
         in problem1 elves' (currRound + 1) numRound

rectangle :: [Coord] -> Int
rectangle elves =
    let elvesRows = map fst elves
        elvesCols = map snd elves
        heigth = maximum elvesRows - minimum elvesRows
        width = maximum elvesCols - minimum elvesCols
    in (heigth + 1) * (width + 1)

-- Part 2
problem2 :: [Coord] -> Int -> Int
problem2 elves currRound =
        let currDef = currRound `mod` 4
            orderOfDir = rotate [0, 1, 2, 3] currDef
            movingElves = map (\(r, c) -> whereToMove (r, c) elves orderOfDir) elves
            startAndDest = map (\((r, c), dir) -> ((r, c), moveTo (r, c) dir)) movingElves
            destsFreq = map fst (filter (\(_, freq) -> freq >= 2) (M.toList (foldr ((\key -> M.insertWith (+) key 1) . snd) M.empty startAndDest)))
            (notMoving, effectivelyMoving) = (map fst (filter (\(start, dest) -> start == dest || dest `elem` destsFreq) startAndDest), map snd (filter (\(start, dest) -> start /= dest && dest `notElem` destsFreq) startAndDest))
            elves' = notMoving ++ effectivelyMoving
         in if null effectivelyMoving then currRound + 1 else problem2 elves' (currRound + 1)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let elves = concatMap getElves (zip (words contents) [1 ..])

    let part1 = problem1 elves 0 10
    print part1

    let part2 = problem2 elves 0
    print part2