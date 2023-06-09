module Main where

import OpticML ( view, p1, p2, alongside, Lens, Para(..), assocL, set, assocR )

-- main :: IO ()
-- main = do
--   let x = view p1 (1, 2)
--   let y = view p2 (1, 2)

--   print x
--   print y

--   print $ view (p1 `alongside` p2) ((1, 2), (3, 4))

--   let para :: Para Int (a, c) (b, c) a b
--       para = Para 5 p1

--   print (params para)

main :: IO ()
main = do
  let x = view assocL ((1, 2), 3)
  let y = set assocL (1, (2, 3)) ((1, 2), 3) 

  let a = view assocR (1, (2, 3))
  let b = set assocR ((1, 2), 3) (1, (2, 3))

  print x
  print y

  print a
  print b
