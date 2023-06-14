module Main
  ( main
  ) where

import OpticML ( Para(..), dense, fwd, Para', Lens', rev, (|.|), linearP, mse, lr )
import Data.Matrix ( fromList, Matrix (nrows, ncols) )
import Data.Bifunctor ( bimap )

-- main :: IO ()
-- main = do
--   let x = fwd p1 (1, 2)
--   let y = fwd p2 (1, 2)

--   print x
--   print y

--   print $ fwd (p1 `alongside` p2) ((1, 2), (3, 4))

--   let para :: Para Int (a, c) (b, c) a b
--       para = Para 5 p1

--   print (params para)

-- main :: IO ()
-- main = do
--   let x = fwd assocL ((1, 2), 3)
--   let y = rev assocL ((1, (2, 3)), ((1, 2), 3))

--   let a = fwd assocR (1, (2, 3))
--   let b = rev assocR ((1, (2, 3)), ((1, 2), 3))

--   print x
--   print y

--   print a
--   print b

-- shape :: Matrix a -> (Int, Int)
-- shape m = (nrows m, ncols m)

-- main :: IO ()
-- main = do
--   let x :: Para' (Matrix Float, Matrix Float)
--                  ((Matrix Float, Matrix Float), Matrix Float)
--                  (Matrix Float)
--       x = dense (4, 3)

--   let l :: Lens' ((Matrix Float, Matrix Float), Matrix Float) (Matrix Float)
--       l = plens x
--   let ps = params x

--   let input :: Matrix Float
--       input = fromList 4 1 [4.9, 3.0 , 1.4, 0.2]

--   print (bimap shape shape ps)
--   print (shape input)

--   print ps
--   print (fwd l (ps, input))
--   print (rev l (input, (ps, input)))

-- main :: IO ()
-- main = do
--   let Para ps ln = dense (4, 2) |.| linearP (2, 3) |.| dense (3, 5) 
--   let input = fromList 4 1 [4.9, 3.0 , 1.4, 0.2]

--   print (fwd ln (ps, input))

main :: IO ()
main = do
  let x :: Matrix Float
      x = fromList 4 1 [4.9, 3.0 , 1.4, 0.2]
  let y :: Matrix Float
      y = fromList 4 1 [4.4, 3.5 , 1.4, 0.7]

  let l :: Lens' (Matrix Float, Matrix Float) (Matrix Float)
      l = mse . lr 1.0

  print (fwd mse (x, y))
  print (fwd l (x, y))