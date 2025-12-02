splitIns :: String -> (Char, Int)
splitIns (x : xs) = (x, read xs :: Int)

splitTotIns :: [String] -> [(Char, Int)]
splitTotIns = map splitIns

start :: Int
start = 50

interpret :: [(Char, Int)] -> Int -> Int -> Int
interpret [] _ count = count 
interpret (('L', val): xs) point count = interpret xs lcase (count + ifLZeroPlusOne)
    where
        lcase = (point - val) `mod` 100
        ifLZeroPlusOne = if lcase == 0 then 1 else 0
interpret (('R', val): xs) point count = interpret xs rcase (count + ifRZeroPlusOne)
    where
        rcase = (point + val) `mod` 100
        ifRZeroPlusOne = if rcase == 0 then 1 else 0

main :: IO ()
main = do
  file <- readFile "./input.txt"
  print(interpret(splitTotIns $ lines file) start 0) 
