import Data.Char (isNumber)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Blueprint = Blueprint Int Int Int (Int, Int) (Int, Int) deriving (Show)
data State = State (Int, Int) (Int, Int) (Int, Int) (Int, Int) deriving (Eq)

pickGeode :: State -> Int
pickGeode (State _ _ _ (x, _)) = x

parseBlueprints :: String -> Blueprint
parseBlueprints str =
    let str' = map (\c -> if c == ':' then ' ' else c) str
        comps = words str'
        nums = mapMaybe (\x -> readMaybe x :: Maybe Int) comps
     in Blueprint (head nums) (nums !! 1) (nums !! 2) (nums !! 3, nums !! 4) (nums !! 5, nums !! 6)

viableState :: State -> Bool
viableState (State (ore, _) (clay, _) (obs, _) (geo, _)) =
    ore >= 0 && clay >= 0 && obs >= 0 && geo >= 0

-- 1 means build that robot, 0 means do not build that robot
analyzer :: Blueprint -> State -> [State]
analyzer (Blueprint _ oreOre oreClay (oreObs, clayObs) (oreGeo, obsGeo)) s@(State (ore, oreBot) (clay, clayBot) (obs, obsBot) (geo, geoBot)) =
    let case0000 = s
        case1000 = State (ore - oreOre, oreBot + 1) (clay, clayBot) (obs, obsBot) (geo, geoBot)
        case0100 = State (ore - oreClay, oreBot) (clay, clayBot + 1) (obs, obsBot) (geo, geoBot)
        case0010 = State (ore - oreObs, oreBot) (clay - clayObs, clayBot) (obs, obsBot + 1) (geo, geoBot)
        case0001 = State (ore - oreGeo, oreBot) (clay, clayBot) (obs - obsGeo, obsBot) (geo, geoBot + 1)

        -- Heuristics
        viableCases = [case0000, case0001]
        viableCases_1 = if oreBot == maximum [oreOre, oreClay, oreObs, oreGeo] then viableCases else viableCases ++ [case1000]
        viableCases_2 = if clayBot == clayObs || (ore - oreBot - 1 >= oreClay) then viableCases_1 else viableCases_1 ++ [case0100]
        viableCases_3 = if obsBot == obsGeo || (clay - clayBot >= clayObs && ore - oreBot - 1 >= oreObs) then viableCases_2 else viableCases_2 ++ [case0010]
        viableStates = filter viableState viableCases_3
     in viableStates

routine :: Blueprint -> State -> Int -> [State] -> Int -> [[State]]
routine blueprint s@(State (ore, oreBot) (clay, clayBot) (obs, obsBot) (geo, geoBot)) minute geoUntilNow time =
    let doableActions = analyzer blueprint (State (ore, oreBot) (clay, clayBot) (obs, obsBot) (geo, geoBot))
        viableStates = map (\(State (x, x') (y, y') (z, z') (w, w')) -> State (x + oreBot + 1, x') (y + clayBot, y') (z + obsBot, z') (w + geoBot, w')) doableActions
     in if minute == time then [reverse geoUntilNow] else concatMap (\newS -> routine blueprint newS (minute + 1) (newS : geoUntilNow) time) viableStates

main :: IO ()
main = do
    contents <- readFile "input.txt"

    let blueprints = map parseBlueprints (lines contents)
    let initState = State (0, 0) (0, 0) (0, 0) (0, 0)

    let problem1 = map (\b -> routine b initState 0 [] 24) blueprints
    let part1 = sum $ zipWith (\b i -> i * maximum (map (pickGeode . last) b)) problem1 [1 ..]
    print part1

    let problem2 = map (\b -> routine b initState 0 [] 32) (take 3 blueprints)
    let part2 = product $ map (maximum . map (pickGeode . last)) problem2
    print part2