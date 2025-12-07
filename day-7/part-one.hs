import Data.Set as Set

splitBeam :: Set Int -> String -> (Set Int, Int)
splitBeam curLst inp
    | Set.null curLst = (initial, 0)
    | otherwise       = (final, splits)
  where
    initial = posInp inp (0, empty)
    equal = intersection curLst initial
    lower  = Set.map (\x -> x - 1) equal
    higher = Set.map (\x -> x + 1) equal
    final = difference (lower `union` higher `union` curLst) equal
    splits = Set.size equal

posInp :: String -> (Int, Set Int) -> Set Int
posInp [] (_, ret) = ret
posInp (x:xs) (index, confirmed)
    | x == '^'  = posInp xs (index + 1, insert index confirmed)
    | otherwise = posInp xs (index + 1, confirmed)

combinedFunc :: (Set Int, Int) -> [String] -> (Set Int, Int)
combinedFunc acc [] = acc
combinedFunc (curSet, curSplits) (x:xs) =
    combinedFunc (nextSet, curSplits + addedSplits) xs
  where
    (nextSet, addedSplits) = splitBeam curSet x


main :: IO()
main = do
    file <- readFile "./input.txt"
    let ls = lines file
    let startSet = posInp (head ls) (0, empty)
    let (_, totalSplits) = combinedFunc (startSet, 0) ls
    print totalSplits
