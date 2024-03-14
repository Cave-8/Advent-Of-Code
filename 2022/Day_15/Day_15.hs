import Control.Applicative ((<|>))
import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Set qualified as Set
import GHC.OldList (group, nub)
import Text.Parsec (ParseError, char, digit, many, many1, parse, spaces)
import Text.Parsec.Char (noneOf)
import Text.Parsec.Prim (skipMany)
import Text.Parsec.String (Parser)

type Coord = (Int, Int)

number :: Parser Int
number = read <$> (negative <|> positive)
  where
    positive = many1 digit
    negative = (:) <$> char '-' <*> positive

numbers :: Parser [Int]
numbers = many (skipNonNumbers *> number <* skipNonNumbers)

skipNonNumbers :: Parser ()
skipNonNumbers = skipMany (noneOf "-0123456789")

extractCoordinates :: String -> Either ParseError [Int]
extractCoordinates = parse numbers ""

extractSB :: [[Int]] -> Set.Set (Coord, Coord) -> Set.Set (Coord, Coord)
extractSB [] toReturn = toReturn
extractSB ([x, y] : [z, w] : xs) toReturn = extractSB xs (Set.insert ((x, y), (z, w)) toReturn)

flat :: [(Coord, Coord)] -> Set.Set Coord -> Set.Set Coord
flat [(x, y)] toReturn = Set.union toReturn (Set.fromList (x : [y]))
flat ((x, y) : xs) toReturn = flat xs (Set.union toReturn (Set.fromList (x : [y])))

findBeacon :: (Coord, Coord) -> Int -> Set.Set Coord
findBeacon ((sx, sy), (bx, by)) row =
    let diag = 2 * (abs (sx - bx) + abs (sy - by)) + 1
        distY = abs (sy - row)
        numPoints = diag - (2 * distY)
     in Set.fromList [(x, row) | x <- [sx - (numPoints `div` 2) .. sx + (numPoints `div` 2)]]

getOutBorders :: (Coord, Coord) -> Set.Set Coord
getOutBorders ((sx, sy), (bx, by)) =
    let diag = 2 * (abs (sx - bx) + abs (sy - by)) + 1
     in Set.fromList
            [ oneStep (x, y) (sx, sy)
            | d <- [0 .. diag `div` 2]
            , x <- [sx - d, sx + d]
            , y <- [sy - (diag `div` 2 - d), sy + (diag `div` 2 - d)]
            ]

getInside :: Coord -> (Coord, Coord) -> Bool
getInside (x, y) ((sx, sy), (bx, by)) =
    let diag = 2 * (abs (sx - bx) + abs (sy - by)) + 1
     in abs (x - sx) + abs (y - sy) <= diag `div` 2

checkAllSensors :: Coord -> [(Coord, Coord)] -> Bool
checkAllSensors (x, y) [] = True
checkAllSensors (x, y) (c : cs)
    | getInside (x, y) c = False
    | otherwise = checkAllSensors (x, y) cs

oneStep :: Coord -> Coord -> Coord
oneStep (x, y) (sx, sy)
    | y == sy && x < sx = (x - 1, y)
    | y == sy && x > sx = (x + 1, y)
    | x == sx && y > sy = (x, y + 1)
    | x == sx && y < sy = (x, y - 1)
    | x < sx = (x - 1, y)
    | x > sx = (x + 1, y)

-- Kinda slow, can probably be optimized
main :: IO ()
main = do
    contents <- readFile "input.txt"
    let parsed = extractCoordinates contents
    let parsed' = case parsed of
            Right nums -> nums

    let sensorsBeacons = extractSB (chunksOf 2 parsed') Set.empty
    let allPoints = Set.unions $ map (`findBeacon` 2000000) (Set.toList sensorsBeacons)
    let part1 = Set.size $ allPoints `Set.difference` flat (Set.toList sensorsBeacons) Set.empty
    print part1

    let allPointsOutsideBorders = Set.unions $ map getOutBorders (Set.toList sensorsBeacons)
    let pointsToCheck = Set.filter (\(x, y) -> x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000) allPointsOutsideBorders
    let finalPoint = head $ Set.toList $ Set.filter (`checkAllSensors` Set.toList sensorsBeacons) pointsToCheck
    let part2 = fst finalPoint * 4000000 + snd finalPoint
    print part2