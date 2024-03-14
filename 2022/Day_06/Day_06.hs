import Data.List (sort)
import GHC.OldList (group)

marker :: [Char] -> Int -> Int
marker s num = marker' s num num
 where
  marker' s num pos = if countUniques (take num s) == num then pos else marker' (tail s) num (pos + 1)

countUniques :: [Char] -> Int
countUniques s = length (group $ sort s)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ marker contents 4
  print $ marker contents 14