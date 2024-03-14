import Data.Sequence qualified as S
import Data.Foldable (Foldable(toList))

updateIndex :: (Int, Int) -> Int -> (Int, Int) -> (Int, Int)
updateIndex (x, y) oldIndex modified@(val, newIndex)
    | y == oldIndex = (x, newIndex)
    | y > oldIndex && y <= newIndex = (x, y-1)
    | y >= newIndex && y < oldIndex = (x, y+1)
    | otherwise = (x, y)

problem :: S.Seq (Int, Int) -> Int -> Int -> Int -> S.Seq (Int, Int)
problem numSequence currIndex currIter numIter
    | currIndex == length numSequence = if currIter == numIter then numSequence else problem numSequence 0 (currIter + 1) numIter
    | otherwise =
        let (val, oldIndex) = numSequence `S.index` currIndex
            newIndex = (val + oldIndex) `mod` (S.length numSequence - 1)
            newSequence = (\(x, y) -> updateIndex (x, y) oldIndex (val, newIndex)) <$> numSequence
        in problem newSequence (currIndex + 1) currIter numIter

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let numbers :: [Int] = map read (lines contents)
    let indexedNums = S.fromList (zip numbers [0 ..])

    let listPart1 = S.sortBy (\(a, b) (x, y) -> compare b y) (problem indexedNums 0 1 1)
    let index0 = head $ map snd (toList $ S.filter (\(x, y) -> x == 0) listPart1)
    let part1 = sum $ map (fst . (\x -> listPart1 `S.index` ((index0 + x) `mod` S.length indexedNums))) [1000, 2000, 3000]
    print part1

    let listPart2 = S.sortBy (\(a, b) (x, y) -> compare b y) (problem ((\(x, y) -> (x * 811589153, y)) <$> indexedNums) 0 1 10)
    let index0' = head $ map snd (toList $ S.filter (\(x, y) -> x == 0) listPart2)
    let part2 = sum $ map (fst . (\x -> listPart2 `S.index` ((index0' + x) `mod` S.length indexedNums))) [1000, 2000, 3000]
    print part2