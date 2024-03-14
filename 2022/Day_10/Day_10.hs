import Data.List (isInfixOf)
import Data.List.Split (chunksOf)

parseOp :: String -> (String, Int)
parseOp op =
  if "addx" `isInfixOf` op
    then (head $ words op, read $ last $ words op)
    else ("noop", 0)

routine :: [String] -> [(Int, Int)] -> [(Int, Int)]
routine [] sigs = tail sigs
routine (x : xs) sigs = routine xs sigs'
 where
  sigs' = sigs ++ exec (op, n)
   where
    exec (op, n)
      | op == "addx" = (snd $ last sigs, snd $ last sigs) : [(snd $ last sigs, n + snd (last sigs))]
      | op == "noop" = [(snd $ last sigs, snd $ last sigs)]
    (op, n) = parseOp x

sigStrength :: [(Int, Int)] -> Int -> Int
sigStrength [] _ = 0
sigStrength (x : xs) curr
  | curr `rem` 40 == 20 = (curr * fst x) + sigStrength xs (curr + 1)
  | otherwise = sigStrength xs (curr + 1)

updateMonitor :: String -> [Int] -> [(Int, Int)] -> Int -> String
updateMonitor monitor span [] curr = monitor
updateMonitor monitor span (x : xs) curr =
  if (curr `rem` 40) `elem` span
    then updateMonitor (monitor ++ ['#']) [snd x - 1, snd x, snd x + 1] xs (curr + 1)
    else updateMonitor (monitor ++ ['.']) [snd x - 1, snd x, snd x + 1] xs (curr + 1)

main :: IO ()
main = do
  operations <- readFile "input.txt"
  let signals = routine (lines operations) [(1, 1)]
  let updatedMonitor = updateMonitor "" [0, 1, 2] signals 0
  print $ sigStrength signals 1
  mapM_ putStrLn (chunksOf 40 updatedMonitor)
