import Data.Char (isDigit)
import Data.List (maximumBy, minimumBy)
import Data.List.Split (splitOn)
import Data.Matrix as M (Matrix (..), fromLists, (!))
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import GHC.Unicode (isAlpha)
import Text.Read (readMaybe)
import Utils (getTransition, realign, realign')

findStart :: Matrix Char -> [(Int, Int)] -> (Int, Int)
findStart mat [] = (-1, -1)
findStart mat (x : xs) = if mat ! x == '.' then x else findStart mat xs

-- Conversion functions
translateDir :: Char -> (Int, Int)
translateDir myDir
    | myDir == '>' = (0, 1)
    | myDir == 'v' = (1, 0)
    | myDir == '<' = (0, -1)
    | myDir == '^' = (-1, 0)

translateDir' :: (Int, Int) -> Char
translateDir' myDir
    | myDir == (0, 1) = '>'
    | myDir == (1, 0) = 'v'
    | myDir == (0, -1) = '<'
    | myDir == (-1, 0) = '^'

dirToNum :: Char -> Int
dirToNum myDir
    | myDir == '^' = 0
    | myDir == '>' = 1
    | myDir == 'v' = 2
    | myDir == '<' = 3

numToDir :: Int -> Char
numToDir n
    | n == 0 = '^'
    | n == 1 = '>'
    | n == 2 = 'v'
    | n == 3 = '<'

-- Wrappers
wrapValue :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> (Int, Int)
wrapValue (r, c) (dr, dc) coords
    | (dr, dc) == (0, 1) = minimumBy (comparing snd) (filter (\(x, y) -> x == r) coords)
    | (dr, dc) == (0, -1) = maximumBy (comparing snd) (filter (\(x, y) -> x == r) coords)
    | (dr, dc) == (1, 0) = minimumBy (comparing fst) (filter (\(x, y) -> y == c) coords)
    | (dr, dc) == (-1, 0) = maximumBy (comparing fst) (filter (\(x, y) -> y == c) coords)

wrapper1 :: Matrix Char -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> (Int, Int)
wrapper1 grid (oldR, oldC) (r, c) (dr, dc) coords =
    let outOfBound = r > nrows grid || r <= 0 || c > ncols grid || c <= 0
        foundASpace = not outOfBound && (grid ! (r, c) == ' ')
        (wr, wc) = if outOfBound || foundASpace then wrapValue (r, c) (dr, dc) coords else (r, c)
        (r', c') = if grid ! (wr, wc) == '#' then (oldR, oldC) else (wr, wc)
     in (r', c')

wrapper2 :: Matrix Char -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> (Int, Int, (Int, Int))
wrapper2 grid (oldR, oldC) (r, c) (dr, dc) coords =
    let outOfBound = r > nrows grid || r <= 0 || c > ncols grid || c <= 0
        foundASpace = not outOfBound && (grid ! (r, c) == ' ')
        n@(wr, wc, (dr', dc')) = if outOfBound || foundASpace then getNewPos (oldR, oldC) (dr, dc) else (r, c, (dr, dc))
        (r', c', dir) = if grid ! (wr, wc) == '#' then (oldR, oldC, (dr, dc)) else n
     in (r', c', dir)

-- Transitions are hardcoded for my input
getNewPos :: (Int, Int) -> (Int, Int) -> (Int, Int, (Int, Int))
getNewPos (r, c) (dr, dc) =
    let (r', c', dir') = ((r - 1) `div` 50, (c - 1) `div` 50, dirToNum $ translateDir' (dr, dc))
        (normR, normC) = ((r - 1) `rem` 50, (c - 1) `rem` 50)
        -- Move to new face
        (newR, newC, newD) = getTransition (r', c', dir')
        -- Realign old position into new face
        i = realign normC normR dir'
        (uR, uC) = realign' i newD
     in (newR * 50 + uR + 1, newC * 50 + uC + 1, translateDir $ numToDir newD)

execRot :: Char -> Char -> Char
execRot myDir x
    | myDir == '>' = if x == 'R' && x /= 'T' then 'v' else if x == 'L' then '^' else '>'
    | myDir == 'v' = if x == 'R' && x /= 'T' then '<' else if x == 'L' then '>' else 'v'
    | myDir == '<' = if x == 'R' && x /= 'T' then '^' else if x == 'L' then 'v' else '<'
    | myDir == '^' = if x == 'R' && x /= 'T' then '>' else if x == 'L' then '<' else '^'

execSteps1 :: Matrix Char -> (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> (Int, Int)
execSteps1 grid (currR, currC) (dr, dc) steps coords
    | steps == 0 = (currR, currC)
    | otherwise =
        let (newR, newC) = (currR + dr, currC + dc)
            (currR', currC') = wrapper1 grid (currR, currC) (newR, newC) (dr, dc) coords
         in execSteps1 grid (currR', currC') (dr, dc) (steps - 1) coords

routine1 :: Matrix Char -> (Int, Int) -> Char -> [Int] -> [Char] -> [(Int, Int)] -> Int
routine1 grid (currR, currC) myDir [] [] coords
    | myDir == '>' = 1000 * currR + 4 * currC + 0
    | myDir == 'v' = 1000 * currR + 4 * currC + 1
    | myDir == '<' = 1000 * currR + 4 * currC + 2
    | myDir == '^' = 1000 * currR + 4 * currC + 3
    | otherwise = -1
routine1 grid (currR, currC) myDir (x : xs) (y : ys) coords =
    let (currR', currC') = execSteps1 grid (currR, currC) (translateDir myDir) x coords
        myDir' = execRot myDir y
     in routine1 grid (currR', currC') myDir' xs ys coords

-- Part 2
execSteps2 :: Matrix Char -> (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> (Int, Int, (Int, Int))
execSteps2 grid (currR, currC) (dr, dc) steps coords
    | steps == 0 = (currR, currC, (dr, dc))
    | otherwise =
        let (newR, newC) = (currR + dr, currC + dc)
            (currR', currC', (dr', dc')) = wrapper2 grid (currR, currC) (newR, newC) (dr, dc) coords
         in execSteps2 grid (currR', currC') (dr', dc') (steps - 1) coords

routine2 :: Matrix Char -> (Int, Int) -> Char -> [Int] -> [Char] -> [(Int, Int)] -> Int
routine2 grid (currR, currC) myDir [] [] coords
    | myDir == '>' = 1000 * currR + 4 * currC + 0
    | myDir == 'v' = 1000 * currR + 4 * currC + 1
    | myDir == '<' = 1000 * currR + 4 * currC + 2
    | myDir == '^' = 1000 * currR + 4 * currC + 3
    | otherwise = -1
routine2 grid (currR, currC) myDir (x : xs) (y : ys) coords =
    let (currR', currC', myDir') = execSteps2 grid (currR, currC) (translateDir myDir) x coords
        myDir'' = execRot (translateDir' myDir') y
     in routine2 grid (currR', currC') myDir'' xs ys coords

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let parsed = init $ init $ lines contents
    -- Grid
    let grid = M.fromLists parsed
    -- Number of steps
    let steps = mapMaybe (\x -> readMaybe x :: Maybe Int) (splitOn "-" (map (\c -> if c == 'R' || c == 'L' then '-' else c) (last $ lines contents)))
    -- Directions
    let directions = filter isAlpha (last $ lines contents) ++ "T"
    let coordinates = [(x, y) | x <- [1 .. nrows grid], y <- [1 .. ncols grid]]
    let startPos = findStart grid coordinates

    let part1 = routine1 grid startPos '>' steps directions (filter (\(x, y) -> grid ! (x, y) == '#' || grid ! (x, y) == '.') coordinates)
    print part1

    let part2 = routine2 grid startPos '>' steps directions (filter (\(x, y) -> grid ! (x, y) == '#' || grid ! (x, y) == '.') coordinates)
    print part2