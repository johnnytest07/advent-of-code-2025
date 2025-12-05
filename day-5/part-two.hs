-- Main.hs
import Data.List (sortOn, foldl')
import System.IO (readFile)

parseRange :: String -> (Int, Int)
parseRange s =
  let (a, b) = break (== '-') s
  in (read a, read (tail b))

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges ranges = reverse $ foldl' insert [] (sortOn fst ranges)
  where
    insert [] r = [r]
    insert acc@((l0,u0):rest) (l,u)
      | l > u0 + 1 = (l,u) : acc
      | otherwise  = (l0, max u0 u) : rest

main :: IO ()
main = do
  inp <- readFile "./ranges.txt"
  let ranges = map parseRange (lines inp)
      merged = mergeRanges ranges
      totalFresh = sum [u - l + 1 | (l,u) <- merged]
  putStrLn $ "Merged ranges: " ++ show merged
  putStrLn $ "Total fresh IDs: " ++ show totalFresh
