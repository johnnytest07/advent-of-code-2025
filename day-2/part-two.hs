splitWith :: (Char -> Bool) -> String -> [String]
splitWith _ [] = []
splitWith f s =
    let (chunk, rest) = break f s
    in chunk :
        case rest of
            []     -> []
            (_:xs) -> splitWith f xs

turnIntoPairs :: [Int] -> [(Int, Int)]
turnIntoPairs [] = []
turnIntoPairs (x:y:xs) = (x,y) : turnIntoPairs xs

-- Detect repeated digit patterns
isRepeated :: String -> Bool
isRepeated s =
    let n = length s
    in any (\k -> n `mod` k == 0 &&
                  concat (replicate (n `div` k) (take k s)) == s)
           [1 .. n `div` 2]

isInvalid :: Int -> Bool
isInvalid = isRepeated . show

iterateFrom :: (Int, Int) -> [Int]
iterateFrom (x, y)
    | x > y = []
    | isInvalid x = x : iterateFrom (x+1, y)
    | otherwise   =     iterateFrom (x+1, y)

main :: IO ()
main = do
    file <- readFile "./input.txt"
    let numbers = concatMap (map read . splitWith (== '-'))
                  (splitWith (== ',') file)
    let pairs = turnIntoPairs numbers
    print $ sum $ concatMap iterateFrom pairs
