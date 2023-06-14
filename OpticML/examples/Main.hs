module Main
  ( main
  ) where

import OpticML ( Para(params, plens), dense, view, Para', Lens' )
import Data.Matrix ( fromList, Matrix )

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

-- main :: IO ()
-- main = do
--   let x = view assocL ((1, 2), 3)
--   let y = set assocL (1, (2, 3)) ((1, 2), 3) 

--   let a = view assocR (1, (2, 3))
--   let b = set assocR ((1, 2), 3) (1, (2, 3))

--   print x
--   print y

--   print a
--   print b

main :: IO ()
main = do
  let x :: Para' (Matrix Float, Matrix Float) 
                 ((Matrix Float, Matrix Float), Matrix Float) 
                 (Matrix Float)
      x = dense (4, 3)
  
  let l :: Lens' ((Matrix Float, Matrix Float), Matrix Float) (Matrix Float)
      l = plens x
  let ps = params x

  let input :: Matrix Float
      input = fromList 4 1 [4.9, 3.0 , 1.4, 0.2]

  print ps
  print (view l (ps, input))
