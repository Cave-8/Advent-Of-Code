import Data.Char (isDigit)
import Data.List (isInfixOf, sort)
import Data.List.Split (splitOn)

data Folder = Folder Int [Folder]
  deriving (Show)

onlySize :: Folder -> [Int]
onlySize (Folder size subFolders) = size : concatMap onlySize subFolders

findFolder :: Int -> [Int] -> Int
findFolder main (x : xs) =
  if 70000000 - main + x >= 30000000
    then x
    else findFolder main xs

num :: (Read a) => String -> a
num x = read (head (words x))

-- When it meets a "$ cd name" command it explores and return the folder
-- This works under the hypothesis that we do not go multiple times in the same folder
parse :: Int -> [Folder] -> [String] -> (Folder, [String])
parse size subFolders cmd@(x : xs)
  | "$ cd .." `isInfixOf` x = (Folder size subFolders, xs)
  | "$ cd" `isInfixOf` x =
      let (f@(Folder size' _), rest') = parse 0 [] xs
       in parse (size + size') (f : subFolders) rest'
  | isDigit (head x) = parse (size + num x) subFolders xs
  | otherwise = parse size subFolders xs
parse size subFolders [] = (Folder size subFolders, [])

createTree :: [String] -> (Folder, [String])
createTree = parse 0 []

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let fileSystem = fst . createTree $ drop 2 (lines contents)
  let sizeOnly = onlySize fileSystem
  let part1 = sum $ filter (<= 100000) sizeOnly
  let part2 = findFolder (head sizeOnly) (sort sizeOnly)
  print part1
  print part2