module Main where

import qualified OpticML (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  OpticML.someFunc
