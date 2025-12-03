import Data.Char (isDigit)
import System.IO (readFile)

maxK :: Int -> String -> String
maxK k s
  | length s <= k = s
  | otherwise     = take k finalTrimmed
  where
    remove = length s - k

    go :: [Char] -> [Char] -> Int -> [Char]
    go [] stack rm = stack
    go (c:cs) stack rm
      | rm > 0 && not (null stack) && head stack < c = go (c:cs) (tail stack) (rm - 1)
      | otherwise = go cs (c : stack) rm

    stackReversed = reverse (go s [] remove)
    finalTrimmed =
      let remLeft = remove - (length s - length stackReversed)
          len = length stackReversed
      in if remLeft <= 0 then stackReversed else take (len - remLeft) stackReversed

digitsToInteger :: String -> Integer
digitsToInteger "" = 0
digitsToInteger ds = read ds :: Integer

best12FromLine :: String -> Integer
best12FromLine line = digitsToInteger (maxK 12 line)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = filter (not . null) (lines contents)
    let vals = map best12FromLine ls
    print (sum vals)