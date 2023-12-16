module Main (main) where

import System.Environment
import Task1 (solveMoves)
import Task4 (findPaths)

main :: IO ()
main = do
  putStrLn "Task 1 - Knight's Tour:"
  let knightTourResult = solveMoves 0 0 5
  printResult knightTourResult

  putStrLn "\nTask 4 - Find Paths in Graph:"
  let graph = [[1, 3, 4], [0, 2, 3], [1, 3], [0, 1, 2, 4], [0, 3]]
  let findPathsResult = findPaths graph 0 1
  printResult findPathsResult

printResult :: (Show a) => Maybe a -> IO ()
printResult result =
  case result of
    Just solution -> putStrLn $ "Solution Found: " ++ show solution
    Nothing -> putStrLn "No solution found."
