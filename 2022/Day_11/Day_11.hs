import Data.Char (isDigit)
import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust, isNothing)
import Text.Read (readMaybe)

data Monkey = Monkey
    { mId :: Int
    , objects :: [Int]
    , op :: Int -> Int
    , mDiv :: Int
    , true :: Int
    , false :: Int
    , mAnalyzed :: Int
    }

-- "Setter" functions
pushItem :: Monkey -> Int -> Monkey
pushItem (Monkey a b c d e f g) item =
    Monkey
        { mId = a
        , objects = b ++ [item]
        , op = c
        , mDiv = d
        , true = e
        , false = f
        , mAnalyzed = g
        }

popItem :: Monkey -> (Monkey, Int)
popItem (Monkey a (b : bs) c d e f g) =
    ( Monkey
        { mId = a
        , objects = bs
        , op = c
        , mDiv = d
        , true = e
        , false = f
        , mAnalyzed = g + 1
        }
    , b
    )

updateMonkeys :: [Monkey] -> Monkey -> Monkey -> Int -> Int -> Int -> [Monkey]
updateMonkeys [] _ _ _ _ _ = []
updateMonkeys (x : xs) oM nM iO iN curr
    | curr == iO = oM : updateMonkeys xs oM nM iO iN (curr + 1)
    | curr == iN = nM : updateMonkeys xs oM nM iO iN (curr + 1)
    | otherwise = x : updateMonkeys xs oM nM iO iN (curr + 1)

-- Parsers
toMonkey :: [String] -> Monkey
toMonkey x =
    Monkey
        { mId = last (extractNumbers (head x))
        , objects = extractNumbers (x !! 1)
        , op = parseOp $ reverse $ words (x !! 2)
        , mDiv = last (extractNumbers (x !! 3))
        , true = last (extractNumbers (x !! 4))
        , false = last (extractNumbers (x !! 5))
        , mAnalyzed = 0
        }

parseOp :: [String] -> (Int -> Int)
parseOp x
    | (head (tail x) == "*") && isNothing (readMaybe $ head x :: Maybe Int) = (^ 2)
    | (head (tail x) == "*") && isJust (readMaybe $ head x :: Maybe Int) = (* read (head x))
    | otherwise = (+ read (head x))

-- Translators
toMonkeys :: [[String]] -> [Monkey]
toMonkeys = map toMonkey

analyzed :: [Monkey] -> [Int]
analyzed = map mAnalyzed

monkeyDivs :: [Monkey] -> [Int]
monkeyDivs = map mDiv

extractNumbers :: String -> [Int]
extractNumbers = map read . words . map (\c -> if isDigit c then c else ' ')

-- Only in part2:
-- (a mod kn) mod n = a mod n, now (a mod kn) is stored
-- k has to be chosen as lcm of all n_1, n_2, ... in this case all number are primes so a simple product is ok
problem :: [Monkey] -> Int -> Int -> Int -> [Monkey]
problem monkeys n k part =
    if null (objects (monkeys !! n))
        then monkeys
        else
            let (oldMonkey', currItem) = popItem (monkeys !! n)
                worry' = if part == 1 then (op $ monkeys !! n) currItem `div` 3 else (op $ monkeys !! n) currItem `rem` k
                test = worry' `rem` mDiv (monkeys !! n) == 0
                newMonkeyIndex = if test then true $ monkeys !! n else false $ monkeys !! n
                newMonkey' = pushItem (monkeys !! newMonkeyIndex) worry'
                monkeys' = updateMonkeys monkeys oldMonkey' newMonkey' n newMonkeyIndex 0
             in problem monkeys' n k part

routine :: [Monkey] -> Int -> Int -> Int -> [Int]
routine monkeys roundN limit part =
    if (roundN `div` length monkeys) == limit
        then analyzed monkeys
        else
            let monkeys' =
                    if part == 1
                        then problem monkeys (roundN `rem` length monkeys) 0 1
                        else problem monkeys (roundN `rem` length monkeys) (product (monkeyDivs monkeys)) 2
             in if null (objects (monkeys' !! (roundN `rem` length monkeys)))
                    then routine monkeys' (roundN + 1) limit part
                    else routine monkeys' roundN limit part

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let parsed = chunksOf 6 $ filter (/= "") $ lines contents
    let monkeys = toMonkeys parsed
    let part1 = product $ take 2 $ reverse $ sort (routine monkeys 0 20 1)
    let part2 = product $ take 2 $ reverse $ sort (routine monkeys 0 10000 2)
    print part1
    print part2