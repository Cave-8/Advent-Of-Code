splitPlays :: String -> (String, String)
splitPlays s = case words s of
    [a, b] -> (a, b)

match :: (String, String) -> Integer
match (x, y)
    | x == "A" && y == "Y" = 8
    | x == "B" && y == "Y" = 5
    | x == "C" && y == "Y" = 2
    | x == "A" && y == "X" = 4
    | x == "B" && y == "X" = 1
    | x == "C" && y == "X" = 7
    | x == "A" && y == "Z" = 3
    | x == "B" && y == "Z" = 9
    | x == "C" && y == "Z" = 6

matchIIpart :: (String, String) -> Integer
matchIIpart (x, y)
    | x == "A" && y == "Y" = 4
    | x == "B" && y == "Y" = 5
    | x == "C" && y == "Y" = 6
    | x == "A" && y == "X" = 3
    | x == "B" && y == "X" = 1
    | x == "C" && y == "X" = 2
    | x == "A" && y == "Z" = 8
    | x == "B" && y == "Z" = 9
    | x == "C" && y == "Z" = 7

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let matches = map splitPlays $ lines contents
    -- First part
    print (sum $ map match matches)
    -- Second part
    print (sum $ map matchIIpart matches)
