import Data.Set qualified as S

type Coord = (Int, Int)
type Coordinates = S.Set Coord
type Board = (Int, Int, Int, Int, Int, Int, Int)

-- Board, Rock, JetIndex, Height
type State = (Board, Int, Int, Int)

-- Rocks
getRocks :: Int -> Int -> Coordinates
getRocks index top
    | index == 0 = S.fromList [(x, top + 4) | x <- [2 .. 5]]
    | index == 1 = S.fromList ([(3, top + 4)] ++ [(x, top + 5) | x <- [2 .. 4]] ++ [(3, top + 6)])
    | index == 2 = S.fromList ([(x, top + 4) | x <- [2 .. 4]] ++ [(4, top + 5)] ++ [(4, top + 6)])
    | index == 3 = S.fromList ([(2, top + x) | x <- [4 .. 7]])
    | index == 4 = S.fromList [(2, top + 4), (3, top + 4), (2, top + 5), (3, top + 5)]
    | otherwise = S.empty

-- Top finder
findTop :: Coordinates -> Int
findTop table
    | S.size table > 0 = maximum $ S.map snd table
    | otherwise = 0

-- Routine with print (debug)
routineWithPrint :: String -> Coordinates -> Int -> Int -> Int -> [String]
routineWithPrint jets rocks iter jetIndex limit
    | iter == limit = showTable rocks
    | otherwise =
        let currTop = findTop rocks
            currRock = getRocks (iter `mod` 5) currTop
            (newRock, newJetIndex) = simulateFall jets rocks currRock jetIndex
            newRocks = rocks `S.union` newRock
         in routineWithPrint jets newRocks (iter + 1) newJetIndex limit

-- Routine with state memoization
routineWithStates :: String -> Coordinates -> [State] -> Int -> Int -> Int -> Int
routineWithStates jets rocks states iter jetIndex limit =
        let currTop = findTop rocks
            currRock = getRocks (iter `mod` 5) currTop
            (newRock, newJetIndex) = simulateFall jets rocks currRock jetIndex
            newRocks = rocks `S.union` newRock
            currState = (snapshot newRocks (findTop newRocks), iter `mod` 5, jetIndex `mod` length jets, findTop newRocks)
         in if (\(b, r, jI, _) -> (b, r, jI)) currState `elem` map (\(b, r, jI, _) -> (b, r, jI)) states
            then cycleManager currState (reverse $ currState : states) limit
            else routineWithStates jets newRocks (currState : states) (iter + 1) newJetIndex limit

-- Capture current state board
snapshot :: Coordinates -> Int -> Board
snapshot rocks top =
    let skyline = map (\a -> top - maximum (S.fromList (0 : S.toList (S.map snd (S.filter (\(x, y) -> x == a) rocks))))) [0 .. 6]
     in (head skyline, skyline !! 1, skyline !! 2, skyline !! 3, skyline !! 4, skyline !! 5, skyline !! 6)

-- Cycle finder routine
cycleManager :: State -> [State] -> Int -> Int
cycleManager cycleStart@(s, r, j, _) states limit =
    let stonesBeforeCycle = headFinder cycleStart states
        heightBeforeCycle = (\(_, _, _, h) -> h) (states !! stonesBeforeCycle)
        cycleLength = length (drop stonesBeforeCycle states) - 1
        [h1, h2] = map (\(_, _, _, h) -> h) (filter (\(s', r', j', _) -> s' == s && r' == r && j == j') states)
        delta = h2 - h1
        deltas = map (\(_, _, _, h) -> h - h1) (drop stonesBeforeCycle states)
        remCycles = (limit - stonesBeforeCycle - 1) `div` cycleLength
        remSteps = (limit - stonesBeforeCycle - 1) `mod` cycleLength
    in h1 + remCycles * delta + deltas !! remSteps

headFinder :: State -> [State] -> Int
headFinder curr@(skyline, stone, jetIndex, _) ((skyline', stone', jetIndex', _) : xs) =
    if skyline == skyline' && stone == stone' && jetIndex == jetIndex' then 0 else 1 + headFinder curr xs

-- Normal routine
routine :: String -> Coordinates -> Int -> Int -> Int -> Int
routine jets rocks iter jetIndex limit
    | iter == limit = findTop rocks
    | otherwise =
        let currTop = findTop rocks
            currRock = getRocks (iter `mod` 5) currTop
            (newRock, newJetIndex) = simulateFall jets rocks currRock jetIndex
            newRocks = rocks `S.union` newRock
         in routine jets newRocks (iter + 1) newJetIndex limit

-- Single rock fall simulation
simulateFall :: String -> Coordinates -> Coordinates -> Int -> (Coordinates, Int)
simulateFall jets rocks currRock jetIndex =
    let rockAfterBlow = blow currRock rocks (jets !! (jetIndex `mod` length jets))
        rockAfterDrop = dropRock rockAfterBlow rocks
        end = S.size (rockAfterBlow `S.intersection` rockAfterDrop) == S.size rockAfterBlow
     in if end then (rockAfterDrop, jetIndex + 1) else simulateFall jets rocks rockAfterDrop (jetIndex + 1)

-- Movements functions
moveLeft :: Coordinates -> Coordinates
moveLeft = S.map (\(x, y) -> (x - 1, y))

moveRight :: Coordinates -> Coordinates
moveRight = S.map (\(x, y) -> (x + 1, y))

moveDown :: Coordinates -> Coordinates
moveDown = S.map (\(x, y) -> (x, y - 1))

-- Simulate lateral movements
blow :: Coordinates -> Coordinates -> Char -> Coordinates
blow currRock rocks dir
    | dir == '<' =
        if S.size (S.filter (\(x, y) -> x == -1) (moveLeft currRock)) > 0 || S.size (moveLeft currRock `S.intersection` rocks) > 0
            then currRock
            else moveLeft currRock
    | dir == '>' =
        if S.size (S.filter (\(x, y) -> x == 7) (moveRight currRock)) > 0 || S.size (moveRight currRock `S.intersection` rocks) > 0
            then currRock
            else moveRight currRock

-- Simulate rock drop
dropRock :: Coordinates -> Coordinates -> Coordinates
dropRock currRock rocks =
    let rocksWithFloor = rocks `S.union` S.fromList [(x, 0) | x <- [0 .. 6]]
     in if S.size (moveDown currRock `S.intersection` rocksWithFloor) > 0 then currRock else moveDown currRock

-- Print functions
showTable :: Coordinates -> [String]
showTable table =
    let top = maximum (S.map snd table) + 1
        base = [(x, 1) | x <- [0 .. 6]]
        rows = completeRows (replicate top base) 0
        prettyRows = map (map (\x -> if S.member x table then '#' else '.')) rows
     in reverse $ "+-------+" : map (\x -> ['|'] ++ x ++ ['|']) prettyRows

completeRows :: [[Coord]] -> Int -> [[Coord]]
completeRows [] _ = []
completeRows (x : xs) inc = map (\(x, y) -> (x, y + inc)) x : completeRows xs (inc + 1)

-- State table prettifier
prettifier :: [State] -> Int -> [String]
prettifier [] _ = []
prettifier ((board, rock, jetIndex, height) : xs) index =
    ("After stone " ++ show index ++ ": skyline: " ++ show board ++ ", rock index: " ++ show rock ++ ", jet index: " ++ show jetIndex ++ ", height: " ++ show height) : prettifier xs (index + 1)

main :: IO ()
main = do
    jets <- readFile "input.txt"

    -- Useful if you want to visualize the table
    -- let table = routineWithPrint jets S.empty 0 0 2022
    -- mapM_ putStrLn table

    let part1 = routine jets S.empty 0 0 2022
    print part1

    let part2 = routineWithStates jets S.empty [] 0 0 1000000000000
    print part2