substitute :: [Char] -> (Int, Int)
substitute dir
  | dir == "R" = (0, 1)
  | dir == "D" = (-1, 0)
  | dir == "L" = (0, -1)
  | dir == "U" = (1, 0)

parse :: String -> ((Int, Int), Int)
parse coord = (substitute (head $ words coord), read $ last $ words coord)

extend :: ((Int, Int), Int) -> [(Int, Int)]
extend ((rd, cd), steps) = [(rd, cd) | x <- [1 .. steps]]

addPos :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
addPos (r, c) vis = [(r, c) | (r, c) `notElem` vis]

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move h@(hr, hc) t@(tr, tc) =
  if t `notElem` ([(x, y) | x <- [hr - 1, hr + 1], y <- [hc - 1, hc, hc + 1]] ++ [(hr, hc - 1), (hr, hc + 1)])
    then moveTail t h
    else t

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail (tr, tc) (hr, hc)
  | hr == tr && hc > tc = (tr, tc + 1)
  | hr == tr && hc < tc = (tr, tc - 1)
  | hr > tr && hc == tc = (tr + 1, tc)
  | hr < tr && hc == tc = (tr - 1, tc)
  | hr > tr && hc > tc = (tr + 1, tc + 1)
  | hr > tr && hc < tc = (tr + 1, tc - 1)
  | hr < tr && hc < tc = (tr - 1, tc - 1)
  | hr < tr && hc > tc = (tr - 1, tc + 1)
  | otherwise = (tr, tc)

problem :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Int
problem [] _ vis = length vis
problem ((dr, dc) : xs) ((hr, hc) : rest) vis =
  problem xs pos' vis'
 where
  h' = (hr + dr, hc + dc)
  pos' = scanl1 move (h' : rest)
  vis' = vis ++ addPos (last pos') vis

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let parsedCoords = map parse $ lines contents
  let extendedCoords = concatMap extend parsedCoords
  print $ problem extendedCoords [(0, 0), (0, 0)] [(0, 0)]
  print $ problem extendedCoords [(0, 0) | x <- [1 .. 10]] [(0, 0)]