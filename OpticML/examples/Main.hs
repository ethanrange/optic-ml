module Main
  ( main
  ) where

import Iris (runPipeline)

main :: IO ()
main = runPipeline 400 "data/iris.csv"