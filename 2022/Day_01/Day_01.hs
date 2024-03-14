import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Ord (Down (..), comparing)
import GHC.OldList (sortBy)

parseInt :: [[String]] -> [[Int]]
parseInt = map (map read)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let subLists = map (splitOn "\n") (splitOn "\n\n" contents)
    let intLists = parseInt subLists
    let sumLists = sortBy (comparing Data.Ord.Down) (sum <$> intLists)
    -- First part
    print $ head sumLists
    -- Second part
    print $ sum $ take 3 sumLists
