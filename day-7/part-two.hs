import Data.List (foldl')

parseLine :: String -> [Int]
parseLine line = [i | (i, c) <- zip [0..] line, c == '^']

insertCount :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
insertCount (i, n) [] = [(i,n)]
insertCount (i, n) ((j,m):xs)
    | i == j    = (j, m+n) : xs
    | otherwise = (j,m) : insertCount (i,n) xs

addAll :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
addAll = foldl' (\acc p -> insertCount p acc)

step :: [(Int, Int)] -> String -> [(Int, Int)]
step beams line =
    let active = parseLine line
    in foldl' (\acc (idx, count) ->
            if idx `elem` active
                then addAll [(idx-1, count), (idx+1, count)] acc
                else insertCount (idx, count) acc
        )
        []
        beams

runAll :: [(Int, Int)] -> [String] -> [(Int, Int)]
runAll = foldl' step

main :: IO ()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let firstLine = head ls
    let initial = [(i,1) | i <- parseLine firstLine]  -- start with each '^' as one beam
    let final = runAll initial ls
    print $ sum (map snd final)
