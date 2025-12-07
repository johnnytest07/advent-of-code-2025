import Data.Char (isDigit, isSpace)
import Data.List (transpose)

findProblems :: [[Char]] -> [[[Char]]]
findProblems columns =
  let isSeparator col = all isSpace col
      groups =
        foldr
          ( \col acc ->
              if isSeparator col
                then [] : acc
                else case acc of
                  [] -> [[col]]
                  (g : gs) -> (col : g) : gs
          )
          []
          columns
   in filter (not . null) groups

main :: IO ()
main = do
  file <- readFile "./input.txt"

  let rows = lines file
      maxLen = maximum (map length rows)
      paddedRows = map (\r -> take maxLen (r ++ repeat ' ')) rows
      columns = transpose paddedRows
      problems = findProblems columns
      reversedProblems = reverse problems
      parseProblem cols =
        let problemRows = transpose cols
            opRow = last problemRows
            op = head (filter (not . isSpace) opRow)
            numberRows = init problemRows
            problemWidth = maximum (map length numberRows)
            paddedNumberRows = map (\r -> take problemWidth (r ++ repeat ' ')) numberRows
            numberCols = transpose paddedNumberRows
            numbers = map (read . filter isDigit) numberCols
         in (op, numbers)

      blocks = map parseProblem reversedProblems

      evalBlock ('+', ns) = sum ns
      evalBlock ('*', ns) = product ns

      results = map evalBlock blocks

  print (sum results)