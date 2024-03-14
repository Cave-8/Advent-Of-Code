import Data.Char (isDigit)
import Data.List (sort)
import Data.List.Split (chunksOf)

data Packet = Number Int | List [Packet] deriving (Show, Eq, Read)

instance Ord Packet where
    compare :: Packet -> Packet -> Ordering
    compare (Number x) (Number y) = compare x y
    compare (Number x) y = compare (List [Number x]) y
    compare x (Number y) = compare x (List [Number y])
    compare (List x) (List y) = compare x y

comparePackets :: [[Packet]] -> [Ordering]
comparePackets = map (\x -> compare (head x) (last x))

stringToPacket :: String -> String -> Bool -> Packet
stringToPacket [] parsed _ = read parsed
stringToPacket (x : xs) parsed readNum
    | x == '[' = stringToPacket xs (parsed ++ "List [") False
    | isDigit x && not readNum = stringToPacket xs (parsed ++ "Number " ++ [x]) True
    | isDigit x && readNum = stringToPacket xs (parsed ++ [x]) True
    | otherwise = stringToPacket xs (parsed ++ [x]) False

indexers :: (Ord a) => [a] -> Int -> [(a, Int)]
indexers [] _ = []
indexers (x : xs) currI = (x, currI) : indexers xs (currI + 1)

main :: IO ()
main = do
    contents <- readFile "input.txt"

    let lists = chunksOf 2 (filter (/= "") $ lines contents)
    let packets = map (map (\x -> stringToPacket x [] False)) lists
    let part1 = sum $ map snd $ filter (\(x, y) -> x == LT) $ indexers (comparePackets packets) 1

    let singlePackets = map (\x -> stringToPacket x [] False) (filter (/= "") $ lines contents)
    let part2 = product $ map snd $ filter (\(x, _) -> x == List [List [Number 2]] || x == List [List [Number 6]]) (indexers (sort singlePackets) 1)

    print part1
    print part2