import Data.Maybe (mapMaybe)
import Utils (crates)

collectMoves :: [String] -> [String]
collectMoves lst = parseMoves' lst [] False
  where
    parseMoves' lst moves found
        | null lst = moves
        | head lst == "" = parseMoves' (tail lst) moves True
        | found = parseMoves' (tail lst) (moves ++ [head lst]) True
        | otherwise = parseMoves' (tail lst) moves False

extractNumbers :: String -> [Int]
extractNumbers = mapMaybe readMaybe . words
  where
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

executeMove :: Int -> [Int] -> [[Char]] -> [[Char]]
executeMove variant [howMany, from, to] stacks = do
    let newFromRow = take (length (stacks !! (from - 1)) - howMany) (stacks !! (from - 1))
    let cratesToAdd =
            if variant == 1
                then take howMany (reverse (stacks !! (from - 1)))
                else reverse $ take howMany (reverse (stacks !! (from - 1)))
    let newToRow = (stacks !! (to - 1)) ++ cratesToAdd

    let (x, _ : xs) = splitAt (from - 1) stacks
    let firstStep = x ++ [newFromRow] ++ xs

    let (x, _ : ys) = splitAt (to - 1) firstStep
    x ++ [newToRow] ++ ys

executeMoves :: Int -> [[Int]] -> [[Char]] -> [[Char]]
executeMoves variant [] stacks = stacks
executeMoves variant moves stacks = executeMoves variant (tail moves) (executeMove variant (head moves) stacks)

lastElements :: [[Char]] -> [Char]
lastElements [] = []
lastElements lst = last (head lst) : lastElements (tail lst)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let rows = lines contents
    let movesInInt = map extractNumbers $ collectMoves rows
    print $ lastElements (executeMoves 1 movesInInt crates)
    print $ lastElements (executeMoves 2 movesInInt crates)
