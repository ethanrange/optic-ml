module Main
  ( main
  ) where

import Iris (runPipeline)

main :: IO ()
main = runPipeline 1000 "data/iris.csv"