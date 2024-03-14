import Data.Char (ord)
import Data.List

-- From external modules
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
 where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n = l `c` splitter (drop i l) c n

priority :: Char -> Int
priority c
  | ord c >= 97 = ord c - 96
  | ord c >= 65 = ord c - 38

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let listOfWords = lines contents
  -- Part 1
  let dividedWords = map (\x -> chunksOf (length x `div` 2) x) listOfWords
  let commonChar = map (\x -> head x `intersect` last x) dividedWords
  let onlyOneChar = map head commonChar
  print $ sum (map priority onlyOneChar)
  -- Part 2
  let groupOfThree = chunksOf 3 listOfWords
  let commonCharThree = map (\x -> head x `intersect` (x !! 1 `intersect` last x)) groupOfThree
  let onlyOneCharThree = map head commonCharThree
  print $ sum (map priority onlyOneCharThree)
