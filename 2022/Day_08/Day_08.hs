import Data.Char (digitToInt)
import Data.Matrix (Matrix (..), fromLists, (!))

stringToList :: String -> [Int]
stringToList = map digitToInt

visible :: Matrix Int -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> [(Int, Int)]
visible forest left right up down cr cc =
  [ (cr, cc)
  | maximum (map (forest !) left) < forest ! (cr, cc)
      || maximum (map (forest !) right) < forest ! (cr, cc)
      || maximum (map (forest !) up) < forest ! (cr, cc)
      || maximum (map (forest !) down) < forest ! (cr, cc)
  ]

visibleFromHere :: [Int] -> Int -> Int -> Int
visibleFromHere [] _ vis = vis
visibleFromHere (x : xs) here vis = if x >= here then vis + 1 else visibleFromHere xs here (vis + 1)

visibleTrees :: Int -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int
visibleTrees here left right up down = max (foldr ((*) . (\x -> visibleFromHere x here 0)) 1 [left, right, up, down])

part1 :: Matrix Int -> [(Int, Int)] -> [(Int, Int)] -> Int -> Int -> [(Int, Int)]
part1 forest vt [] r c = vt
part1 forest vt (x@(1, _) : xs) r c = part1 forest (vt ++ [x]) xs r c
part1 forest vt (x@(_, 1) : xs) r c = part1 forest (vt ++ [x]) xs r c
part1 forest vt (x@(cr, cc) : xs) r c =
  if cr == r || cc == c
    then part1 forest (vt ++ [x]) xs r c
    else
      part1
        forest
        ( vt
            ++ visible
              forest
              [(cr, y) | y <- [1 .. cc - 1]]
              [(cr, y) | y <- [cc + 1 .. c]]
              [(y, cc) | y <- [1 .. cr - 1]]
              [(y, cc) | y <- [cr + 1 .. r]]
              cr
              cc
        )
        xs
        r
        c

part2 :: Matrix Int -> [(Int, Int)] -> Int -> Int -> Int -> Int
part2 forest [] r c currMax = currMax
part2 forest (x@(1, _) : xs) r c currMax = part2 forest xs r c currMax
part2 forest (x@(_, 1) : xs) r c currMax = part2 forest xs r c currMax
part2 forest (x@(cr, cc) : xs) r c currMax =
  if cr == r || cc == c
    then part2 forest xs r c currMax
    else
      part2
        forest
        xs
        r
        c
        ( visibleTrees
            (forest ! (cr, cc))
            (reverse [forest ! (cr, y) | y <- [1 .. cc - 1]])
            [forest ! (cr, y) | y <- [cc + 1 .. c]]
            (reverse [forest ! (y, cc) | y <- [1 .. cr - 1]])
            [forest ! (y, cc) | y <- [cr + 1 .. r]]
            currMax
        )

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let list = map stringToList $ lines contents
  let forest = fromLists list
  let p1 =
        length $
          part1
            forest
            []
            [(x, y) | x <- [1 .. nrows forest], y <- [1 .. ncols forest]]
            (nrows forest)
            (ncols forest)
  let p2 =
        part2
          forest
          [(x, y) | x <- [1 .. nrows forest], y <- [1 .. ncols forest]]
          (nrows forest)
          (ncols forest)
          0
  print p1
  print p2
