#!/usr/bin/env stack

module Main where

import Control.Concurrent
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Conc (numCapabilities, pseq)
import GHC.Conc.Sync (par)
import Gaussian
import System.Environment (getArgs)
import System.Random (StdGen, getStdGen, randoms)
import Text.Printf (printf)

main :: IO ()
main = do
  let size = 30

  putStrLn "Parallel"
  runGaussianParallel size

  putStrLn "Sequential"
  runGaussianSequential size

-- Run Gaussian elimination in parallel
runGaussianParallel :: Int -> IO ()
runGaussianParallel size = do
  (mat1, vec1) <- generateRandom size
  start <- getCurrentTime
  let ans = gaussian mat1 vec1 True
  let x = case ans of
        Exists vec -> vec
        _ -> []
  let y = force x
  print y
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  putStrLn $ "number of cores: " ++ show numCapabilities

-- Run Gaussian elimination sequentially
runGaussianSequential :: Int -> IO ()
runGaussianSequential size = do
  (mat1, vec1) <- generateRandom size
  start <- getCurrentTime
  let ans = gaussian mat1 vec1 False
  let x = case ans of
        Exists vec -> vec
        _ -> []
  let y = force x
  print y
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  putStrLn "number of cores: 1"
