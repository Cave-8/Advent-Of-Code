decimalToSnafu :: Int -> String
decimalToSnafu num
    | num == 0 = "0"
    | otherwise = decimalToSnafu' num []
  where
    decimalToSnafu' num toRet
        | num == 0 = reverse toRet
        | otherwise =
            let (n, r) = (num `div` 5, num `rem` 5)
             in if r > 2
                    then decimalToSnafu' (n + 1) (toRet ++ (if r - 5 == -2 then "=" else if r - 5 == -1 then "-" else show (r - 5)))
                    else decimalToSnafu' n (toRet ++ show r)

snafuToDecimal :: String -> Int
snafuToDecimal snafu =
    let powersOfFive = reverse $ map (5 ^) [0 .. length snafu - 1]
        snafuFives = zip snafu powersOfFive
        result = sum $ map (uncurry multiplySnafu) snafuFives
     in result

multiplySnafu :: Char -> Int -> Int
multiplySnafu c p
    | c == '=' = (-2) * p
    | c == '-' = -p
    | c == '0' = 0
    | c == '1' = p
    | c == '2' = 2 * p

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let elems = words contents
    let decRes = sum $ map snafuToDecimal elems
    let res = decimalToSnafu decRes

    print res