module Task1 (solveMoves) where

import Data.List (elemIndex)
import Data.Maybe (mapMaybe)

type Board = [[Int]]

type Cell = (Int, Int)

-- Main function to solve the knight's tour problem and return the board
solveMoves :: Int -> Int -> Int -> Maybe Board
solveMoves r c size = case solveHelper (Just (r, c)) [] 1 size of
  Just result -> Just $ map (\i -> mapMaybe (\j -> elemIndex (i, j) result) [0 .. size - 1]) [0 .. size - 1]
  Nothing -> Nothing

-- Helper function to perform the recursive backtracking
solveHelper :: Maybe Cell -> [Cell] -> Int -> Int -> Maybe [Cell]
solveHelper Nothing _ _ _ = Nothing
solveHelper (Just cell) visited moveCount size
  | moveCount == size * size = Just $ visited ++ [cell]
  | otherwise = findSolution cell visited moveCount size (filter (`notElem` visited) (nextPositions cell size))

-- Function to find a solution from a given position
findSolution :: Cell -> [Cell] -> Int -> Int -> [Cell] -> Maybe [Cell]
findSolution _ _ _ _ [] = Nothing
findSolution cell visited moveCount size (candidate : candidates) =
  solveHelper (Just candidate) (visited ++ [cell]) (moveCount + 1) size <> findSolution cell visited moveCount size candidates

-- Possible row moves for the knight
rowMoves :: [Int]
rowMoves = [-2, -1, 1, 2, 2, 1, -1, -2]

-- Possible column moves for the knight
colMoves :: [Int]
colMoves = [1, 2, 2, 1, -1, -2, -2, -1]

-- Function to get the next possible positions for the knight
nextPositions :: Cell -> Int -> [Cell]
nextPositions (r, c) size =
  [(r + dr, c + dc) | (dr, dc) <- zip rowMoves colMoves, isValidPosition (r + dr) (c + dc)]
  where
    -- Check if the position is valid (within the board boundaries)
    isValidPosition r' c' = r' >= 0 && r' < size && c' >= 0 && c' < size
