import Data.List (intersect)
import Data.Maybe (isJust)
import Data.Set qualified as S
import GHC.OldList (tails)

type Cube = (Int, Int, Int)
type Sides = ((Int, Int), (Int, Int), (Int, Int))

toCube :: String -> Cube
toCube cube = read ("(" ++ cube ++ ")")

isAdjacent :: Cube -> Cube -> Bool
isAdjacent (x, y, z) (x', y', z') = ((x == x' + 1 || x == x' - 1) && y == y' && z == z') || (x == x' && (y == y' + 1 || y == y' - 1) && z == z') || (x == x' && y == y' && (z == z' + 1 || z == z' - 1))

sharedFaces :: [Cube] -> Int -> Int
sharedFaces [x] shared = shared
sharedFaces lst shared =
    let pairs = [(x, y) | (x : ys) <- tails lst, y <- ys]
        validPairs = filter id (map (uncurry isAdjacent) pairs)
     in 2 * length validPairs

adjacents :: Cube -> [Cube]
adjacents (x, y, z) = [(x - 1, y, z), (x, y - 1, z), (x, y, z - 1), (x + 1, y, z), (x, y + 1, z), (x, y, z + 1)]

airFinder :: [Cube] -> [Cube] -> Int
airFinder [] _ = 0
airFinder (x : xs) cubes =
    let borderingCubes = filter (`elem` cubes) (adjacents x)
     in length borderingCubes + airFinder xs cubes

-- If we can reach (-1, -1, -1) then we will not consider the starting block as air
-- We could cache visited nodes to avoid revisiting them again
lookingForExternal :: Cube -> [Cube] -> S.Set Cube -> S.Set Cube -> S.Set Cube -> Maybe Cube
lookingForExternal startNode lst visited cubes traversableCells
    | null lst = Just startNode
    | head lst == (-1, -1, -1) = Nothing
    | otherwise =
        let visited' = head lst `S.insert` visited
            toVisit = filter (\x -> x `S.member` traversableCells && x `S.notMember` visited') (adjacents $ head lst)
            nextNodes = toVisit ++ tail lst
         in lookingForExternal startNode nextNodes visited' cubes traversableCells

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let cubes = map toCube $ lines contents

    let part1 = 6 * length cubes - sharedFaces cubes 0
    print part1

    let cubesSet = S.fromList cubes
    let traversableCells = S.fromList (filter (`S.notMember` cubesSet) [(x, y, z) | x <- [-1 .. 22], y <- [-1 .. 22], z <- [-1 .. 22]])
    let air = S.delete (-1, -1, -1) $ S.filter (\x -> isJust (lookingForExternal x [x] S.empty cubesSet traversableCells)) traversableCells
    let part2 = part1 - (6 * length (S.toList air) - sharedFaces (S.toList air) 0)
    print part2