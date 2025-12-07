import Data.List (transpose)

splitWith :: (Char -> Bool) -> String -> [String]
splitWith _ [] = []
splitWith func s =
  let (chunk, rest) = break func s
   in chunk : case rest of
        [] -> []
        (_ : xs) -> splitWith func xs

clearEmpty :: [String] -> [String]
clearEmpty [] = []
clearEmpty (x : xs)
  | x == "" = clearEmpty xs
  | otherwise = x : clearEmpty xs

data Sign = Plus | Times
  deriving (Show)

class Op a where
  apply :: a -> Int -> Int -> Int

instance Op Sign where
  apply :: Sign -> Int -> Int -> Int
  apply Plus = (+)
  apply Times = (*)

parseBlock :: [String] -> (Sign, [Int])
parseBlock xs =
  let opStr = last xs
      nums = map read (init xs)
      sign = case opStr of
        "+" -> Plus
        "*" -> Times
        _ -> error ("Unknown operator: " ++ opStr)
   in (sign, nums)

evalBlock :: (Sign, [Int]) -> Int
evalBlock (_, []) = error "Block has no numbers"
evalBlock (sign, n : ns) = foldl (apply sign) n ns

main :: IO ()
main = do
  file <- readFile "./input.txt"
  let rows = map (clearEmpty . splitWith (== ' ')) (lines file)
  let columns = transpose rows
  let blocks = map parseBlock columns
  let results = map evalBlock blocks
  print (sum results)
