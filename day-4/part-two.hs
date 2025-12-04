import Data.List
import Data.Maybe

type Grid = [String]

-- Count accessible rolls in current grid
countAccessible :: Grid -> Int
countAccessible grid = sum [1 | row <- [0..rows-1], col <- [0..cols-1], 
                                 grid!!row!!col == '@' && countNeighbors row col < 4]
  where
    rows = length grid
    cols = length (head grid)
    countNeighbors r c = sum [1 | dr <- [-1..1], dc <- [-1..1], 
                                  not (dr == 0 && dc == 0),
                                  let nr = r + dr
                                      nc = c + dc,
                                  nr >= 0 && nr < rows && 
                                  nc >= 0 && nc < cols &&
                                  grid!!nr!!nc == '@']

removeAccessible :: Grid -> Grid
removeAccessible grid = [[if grid!!r!!c == '@' && countNeighbors r c < 4 
                          then '.' else grid!!r!!c | c <- [0..cols-1]] | r <- [0..rows-1]]
  where
    rows = length grid
    cols = length (head grid)
    countNeighbors r c = sum [1 | dr <- [-1..1], dc <- [-1..1], 
                                  not (dr == 0 && dc == 0),
                                  let nr = r + dr
                                      nc = c + dc,
                                  nr >= 0 && nr < rows && 
                                  nc >= 0 && nc < cols &&
                                  grid!!nr!!nc == '@']

removeAll :: Grid -> (Grid, Int)
removeAll grid = go grid 0
  where
    go currentGrid totalRemoved =
      let accessibleCount = countAccessible currentGrid
      in if accessibleCount == 0
         then (currentGrid, totalRemoved)
         else let newGrid = removeAccessible currentGrid
              in go newGrid (totalRemoved + accessibleCount)

main :: IO()
main = do
    file <- readFile "./input.txt"
    let grid = lines file
    let (finalGrid, totalRemoved) = removeAll grid
    print totalRemoved