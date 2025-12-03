import Data.List(sort)
import GHC.Base (VecElem(Int16ElemRep))

convertLstInt :: String -> [Int]
convertLstInt = map (\ x -> read [x] :: Int)

merge :: [Int] -> Int
merge [] = 0
merge (x:y:xs) = x*10 + y + merge xs

findStarting :: [Int] -> ([Int], [Int])
findStarting inp = break (==maxNo) inp
    where maxNo = maximum inp

-- combine :: ([Int],[Int]) -> Int
-- combine (x, y) 
--     |x /= [] = (last x) * 10 + maximum y
--     |otherwise = maximum (reverse (drop 1 x)) * 10 + last x

combine :: ([Int], [Int]) -> Int
combine (xs, ys)
    | length ys > 1 = head ys * 10 + maximum (tail ys)
    | otherwise = maximum xs * 10 + head ys


main :: IO ()
main = do
    file <- readFile "./input.txt"
    let numArray = map convertLstInt (lines file)
    let split = map (combine . findStarting) numArray

    print (sum split)
