module Task4 (findPaths) where

type Graph = [[Int]]

type Path = [Int]

-- Main function to find all paths from start to end in the given graph
findPaths :: Graph -> Int -> Int -> [Path]
findPaths graph start end = dfs graph start end []

-- Depth-first search function to explore paths in the graph
dfs :: Graph -> Int -> Int -> Path -> [Path]
dfs graph current end path
  | current == end = [path ++ [end]] -- If the current node is the destination, add the path
  | otherwise = concatMap (\neighbor -> dfs graph neighbor end (path ++ [current])) neighbors
  where
    neighbors = filter (`notElem` path) (graph !! current) -- Filter neighbors not already in the path
