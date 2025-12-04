buffer = ".........................................................................................................................................."

addDot :: String -> String
addDot xs = "." ++ xs ++ "."

occurences :: Eq a => a -> [a] -> Int
occurences _ [] = 0
occurences checkAgainst (x:xs)
    |checkAgainst == x = 1 + occurences checkAgainst xs
    |otherwise = 0 + occurences checkAgainst xs

countAB :: String -> Int
countAB xs = occurences '@' (take 3 xs)

handlingThreeLines :: String -> String -> String -> Int
handlingThreeLines above middle bottom
    |length above < 3 = 0
    |current == '.' = handlingThreeLines (tail above) (tail middle) (tail bottom)
    |numOccurences >= 4 = handlingThreeLines (tail above) (tail middle) (tail bottom)
    |otherwise = 1 + handlingThreeLines (tail above) (tail middle) (tail bottom)
    where
        current = middle!!1
        numOccurences = countAB above + countAB bottom + countAB middle - 1

handlingList :: [String] -> Int
handlingList full@(first:second:third:remaining)
    |null remaining = handlingThreeLines first second third
    |otherwise = handlingThreeLines first second third + handlingList (tail full)

main :: IO()
main = do
    file <- readFile "./input.txt"
    let fixedInput = [buffer] ++ map addDot (lines file) ++ [buffer]
    print $ handlingList fixedInput
