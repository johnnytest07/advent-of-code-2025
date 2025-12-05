

checkAgainstRanges :: Int -> [(Int, Int)] -> Int
checkAgainstRanges _ [] = 0
checkAgainstRanges inp ((lower, higher): remaining)
    |inp >= lower && inp <= higher = 1
    |otherwise = checkAgainstRanges inp remaining

main :: IO()
main = do
    inp1 <- readFile "./ranges.txt"
    inp2 <- readFile "./input.txt"
    let ranges = map ((\(x,y) -> (read x :: Int, read (tail y) :: Int)) . (break (=='-'))) (lines inp1) -- paris of ranges (start, finish)
    let inputs = map (\x -> read x :: Int) (lines inp2)
    let result = map (\i -> checkAgainstRanges i ranges) inputs


    print $ sum result