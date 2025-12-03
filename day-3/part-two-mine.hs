


dropElem :: Int -> [Int] -> [Int]
dropElem inp [] = []
dropElem inp (x:xs)
    |inp == x = xs
    |otherwise = x:dropElem inp xs

convertLstInt :: String -> [Int]
convertLstInt = map (\ x -> read [x] :: Int)

digitsToInt :: [Int] -> Int
digitsToInt = read . concatMap show

reverseCompare :: [Int] -> [Int] -> Bool
reverseCompare xs ys = digitsToInt (reverse xs) > digitsToInt (reverse ys)

-- this is fed in backwards
lstIterate :: [Int] -> [Int]
lstIterate full@(x:xs)
    |length full == 12 = full
    |reverseCompare fstTwelve nextTwelve = lstIterate (fstTwelve ++ tail remaining)
    |otherwise = lstIterate (nextTwelve ++ tail remaining)
    where
        (fstTwelve, remaining) = splitAt 12 full
        nextTwelve = dropElem (minimum fstTwelve) fstTwelve ++ [head remaining]

main :: IO ()
main = do
    file <- readFile "./input.txt"
    let numArray = map convertLstInt (lines file)
    let proper = map (reverse . lstIterate . reverse) numArray
    print (map digitsToInt proper)
