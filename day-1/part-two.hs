splitIns :: String -> (Char, Int)
splitIns (x : xs) = (x, read xs :: Int)

splitTotIns :: [String] -> [(Char, Int)]
splitTotIns = map splitIns

start :: Int
start = 50

interpret :: [(Char, Int)] -> Int -> Int -> Int
interpret [] _ count = count 
interpret (('L', val): xs) point count = interpret xs lcase (count + noTimes)
    where
        lcase = (point - val) `mod` 100        
        noTimes = (val + (99 - point)) `div` 100
interpret (('R', val): xs) point count = interpret xs rcase (count + noTimes)
    where
        rcase = (point + val) `mod` 100
        noTimes = (val + point) `div` 100

main :: IO ()
main = do
  file <- readFile "./input.txt"
  print(interpret(splitTotIns $ lines file) start 0) 
