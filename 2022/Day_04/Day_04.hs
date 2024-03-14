import Data.List
import Data.List.Split (splitOn)

createPairs :: String -> (Int, Int)
createPairs s = let [a, b] = map read $ splitOn "-" s in (a, b)

pairsPart1 :: String -> Int
pairsPart1 p = do
    let couples = splitOn "," p
    let (a, b) = createPairs $ head couples
    let (c, d) = createPairs $ last couples
    if (a <= c && b >= d) || (c <= a && d >= b)
        then 1
        else 0

pairsPart2 :: String -> Int
pairsPart2 p = do
    let couples = splitOn "," p
    let (a, b) = createPairs $ head couples
    let (c, d) = createPairs $ last couples
    if (a <= c && b >= d) || (c <= a && d >= b) || (c <= b && c >= a) || (a <= d && a >= c)
        then 1
        else 0

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let listOfPairs = lines contents
    let valuesPart1 = map pairsPart1 listOfPairs
    let valuesPart2 = map pairsPart2 listOfPairs
    print $ sum valuesPart1
    print $ sum valuesPart2