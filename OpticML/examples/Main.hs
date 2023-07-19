{-# LANGUAGE RankNTypes #-}
module Main
  ( main
  ) where

import OpticML ( Para(..), dense, fwd, Para', Lens', rev, (|.|), linearP, mse, lr, update, add, biasP, assocL, identityL, alongside, linear, Lens, assocR, lens, liftPara, relu, sigmoid )
import Data.Matrix ( fromList, Matrix (nrows, ncols), fromLists )
import Data.Bifunctor ( bimap )
import Control.Monad (join)

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

-- main :: IO ()
-- main = do
--   let x :: Matrix Float
--       x = fromList 4 1 [4.9, 3.0 , 1.4, 0.2]
--   let y :: Matrix Float
--       y = fromList 4 1 [4.4, 3.5 , 1.4, 0.7]

--   let l :: Lens' (Matrix Float, Matrix Float) (Matrix Float)
--       l = mse . lr 1.0

--   print (fwd mse (x, y))
--   print (fwd l (x, y))

-- main :: IO ()
-- main = do
--   let model :: Para' (Matrix Float, Matrix Float) ((Matrix Float, Matrix Float), Matrix Float) (Matrix Float)
--       model = dense (4, 3)
--   -- let umodel :: Lens' ((Matrix Float, Matrix Float), Matrix Float) (Matrix Float)
--   --     umodel = update >> plens model

--   -- print (fwd umodel (params model, input))

--   let input = fromList 4 1 [4.9, 3.0, 1.4, 0.2]
--   let fpb = fromList 3 1 [0.0, 0.0, 0.0]
--   let fpl = fromLists [[-0.01517759, -0.00055333, -0.00697705, -0.0004722 ],
--        [-0.01130874,  0.00753457,  0.00460682, -0.00851338],
--        [-0.00154599, -0.00427765,  0.0001421 , -0.00557464]]
--   let fps = (fpb, fpl)

--   let ret = fromList 3 1 [1, 2, 3]

--   -- print $ fwd (plens model) (fps, input)
--   -- print $ rev (plens model) (ret, (fps, input))

--   print (ret, (fps, input))
--   -- print (fwd umodel (params model, input))

-- main :: IO ()
-- main = do
--   let q :: Para' (Matrix Float, Matrix Float) ((Matrix Float, Matrix Float), Matrix Float) (Matrix Float)
--       q = dense (2, 2)

--   let j = linearP (2, 2)
--   let s = biasP 2

--   let bp = fromList 2 1 [1, 1]
--   let lp = fromLists [[1, 2], [3, 4]]
--   let inp = fromList 2 1 [7, 8]
--   -- let z = fwd (plens q) ((bp, lp), inp)

--   let ret = fromList 2 1 [9, 9]

--   let ptl = assocL . (identityL `alongside` linear) . add

--   let ps :: (Matrix Float, ((Matrix Float, Matrix Float), Matrix Float))
--       ps = (ret, ((bp, lp), inp))

--   let nadd :: Num a => Lens (a, a) (d, d) a d
--       nadd = lens v u
--         where
--             -- v :: Num i => (i, i) -> i
--             v = uncurry (+)

--             u :: (d, (a, a)) -> (d, d)
--             u = join (,) . fst

--   let r = rev nadd ps
--   -- let h = rev (plens j) (ret, (lp, inp))
--   -- let n = rev (plens s) (ret, (bp, inp))

--   print r

testModel :: IO ()
testModel = do
  let m :: Matrix Double
      m = fromLists [[1, 2, 3],
                     [4, 5, 6]
                    ]

  let x :: Matrix Double
      x = fromList 3 1 [7, 8, 9]

  let y :: Matrix Double
      y = fromList 2 1 [150, 222]

  let b :: Matrix Double
      b = fromList 2 1 [100, 100]

  let loss :: Matrix Double
      loss = fromList 2 1 [10, 20]

  let m' :: Matrix Double
      m' = fromLists [[70, 80, 90],
                     [140, 160, 180]
                    ]

  let x' :: Matrix Double
      x' = fromList 3 1 [90, 120, 150]

  -- let pl :: Para' (Matrix Double) (Matrix Double, Matrix Double) (Matrix Double) 
  --     pl = linearP (3, 2)
  -- let res = fwd (plens pl) (m, x) -- [50 122]
  -- let res = rev (plens pl) (loss, (m, x)) -- ([[70, 80, 90], [140, 160, 180]], [90, 120, 150])

  let pl :: Para' (Matrix Double, (Matrix Double, Matrix Double)) (((), (Matrix Double, Matrix Double)), Matrix Double) (Matrix Double)
      pl = dense (5, 3) identityL
  -- let res = fwd (plens pl) (((), (b, m)), x) -- [150, 222]
  let res = rev (plens pl) (loss, (((), (b, m)), x))

  print res
  -- print (params pl)

testCap :: IO ()
testCap = do
  let loss :: Matrix Double
      loss = fromList 2 1 [10, 20]

  let l :: Lens s Double () ()
      l = lr 0.01
  let pc = liftPara l
  
  -- let res = fwd (plens pc) (params pc, loss) -- ()
  let res = rev (plens pc) ((), (loss, ()))

  print res
  print 1

testLoss :: IO ()
testLoss = do
  let y :: Matrix Double
      y = fromList 2 1 [150, 222]

  let ey :: Matrix Double
      ey = fromList 2 1 [140, 202]

  let l :: Fractional a => Lens' (Matrix a, Matrix a) a
      l = mse

  let lr :: Double
      lr = -0.01

  let pl :: Fractional a => Para' (Matrix Double) (Matrix a, Matrix a) a
      pl = Para (fromLists [[]]) l

  -- let res = fwd (plens pl) (y, ey)
  let res = rev (plens pl) (lr, (y, ey))

  print res

testActivation :: IO ()
testActivation = do
  let m :: Matrix Double
      m = fromLists [[0.01, -0.02, 0.03], [0.04, 0.05, -0.06]]

  let x :: Matrix Double
      x = fromList 3 1 [-0.07, 0.08, 0.09]

  let y :: Matrix Double
      y = fromList 2 1 [0.0004, -0.0042]

  let b :: Matrix Double
      b = fromList 2 1 [1.0, 1.0]

  let loss :: Matrix Double
      loss = fromList 2 1 [0.1, 0.2]

  let pl :: Para' (Matrix Double, (Matrix Double, Matrix Double)) (((), (Matrix Double, Matrix Double)), Matrix Double) (Matrix Double)
      pl = dense (5, 3) sigmoid
  -- let res = fwd (plens pl) (((), (b, m)), x)
  let res = rev (plens pl) (loss, (((), (b, m)), x))

  print res

main :: IO ()
main = testModel