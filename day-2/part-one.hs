splitWith :: (Char -> Bool) -> String -> [String]
splitWith _ [] = []
splitWith func s =
    let (chunk, rest) = break func s
    in chunk : case rest of
        [] -> []
        (_ : xs) -> splitWith func xs

turnIntoPairs :: [Int] -> [(Int, Int)]
turnIntoPairs [] = []
turnIntoPairs (x:y:xs) = (x,y):turnIntoPairs xs

possiblePali = [11,1111,111111,11111111,1111111111, 1010, 10101010, 100100,10001000,1000010000]
secondaryPali = [101, 1001, 10001, 100001]
-- tertiaryPali = [1001001]

numDigits :: Int -> Int
numDigits n = length (show (abs n))

iterateFrom :: (Int, Int) -> [Int]
iterateFrom (x, y)
    |x == y = if haveFactor x then [x] else []
    |haveFactor x = x : iterateFrom (x+1, y)
    |otherwise = iterateFrom(x+1, y)

haveFactor :: Int -> Bool
haveFactor x = foldr (||) False [x `mod` z == 0 | z <- factors]
    where
        factors = [y | y <- possiblePali, numDigits y == numDigits x] ++ [y | y <- secondaryPali, numDigits y == (numDigits x `div` 2 + 1), even $ numDigits x]

main :: IO ()
main = do
    file <- readFile "./input.txt"
    let numbers = concatMap (map read . splitWith (== '-')) (splitWith (== ',') file) :: [Int]
    let rangeInputs = turnIntoPairs numbers
    let ret = concatMap iterateFrom rangeInputs
    print (sum ret)
