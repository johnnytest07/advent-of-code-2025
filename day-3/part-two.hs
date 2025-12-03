import Data.Char (isDigit)
import System.IO (readFile)

-- Choose the lexicographically largest subsequence of length k from a digit string
maxK :: Int -> String -> String
maxK k s
  | length s <= k = s
  | otherwise     = take k finalTrimmed
  where
    remove = length s - k

    -- stack: head is top (represents the rightmost kept digit so far, reversed)
    go :: [Char] -> [Char] -> Int -> [Char]
    go [] stack rm = stack
    go (c:cs) stack rm
      | rm > 0 && not (null stack) && head stack < c = go (c:cs) (tail stack) (rm - 1)
      | otherwise = go cs (c : stack) rm

    stackReversed = reverse (go s [] remove)   -- now in original left-to-right order
    -- if removals remain (shouldn't normally except when all digits non-decreasing),
    -- drop from the end
    finalTrimmed =
      let remLeft = remove - (length s - length stackReversed) -- remaining removals
          len = length stackReversed
      in if remLeft <= 0 then stackReversed else take (len - remLeft) stackReversed

-- Convert the chosen string of digits to Integer safely
digitsToInteger :: String -> Integer
digitsToInteger "" = 0
digitsToInteger ds = read ds :: Integer

-- Process one line: pick best 12-digit subsequence and return its Integer value
best12FromLine :: String -> Integer
best12FromLine line = digitsToInteger (maxK 12 line)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let ls = filter (not . null) (lines contents)
    let vals = map best12FromLine ls
    print (sum vals)