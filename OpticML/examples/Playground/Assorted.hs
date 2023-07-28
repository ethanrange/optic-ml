{-# OPTIONS_GHC -w -W #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Playground.Assorted where

import Control.Monad (join)
import Data.Matrix (Matrix (..), fromList, fromLists)
import OpticML (Lens, Lens', Para (..), Para', alongside, assocL, assocR, biasP, dense, fwd, identityL, lens, linearP, lr, mse, p1, p2, rev, (|.|), EmptyParam)

testProj :: IO ()
testProj = do
  let x = fwd p1 (1, 2)
  let y = fwd p2 (1, 2)

  print x
  print y

  print $ fwd (p1 `alongside` p2) ((1, 2), (3, 4))

  let para :: Para Int (a, c) (b, c) a b
      para = Para 5 p1

  print (params para)

testAssoc :: IO ()
testAssoc = do
  let x = fwd assocL ((1, 2), 3)
  let y = rev assocL ((1, (2, 3)), ((1, 2), 3))

  let a = fwd assocR (1, (2, 3))
  let b = rev assocR ((1, (2, 3)), ((1, 2), 3))

  print x
  print y

  print a
  print b

shape :: Matrix a -> (Int, Int)
shape m = (nrows m, ncols m)

testDense :: IO ()
testDense = do
  let x ::
        Para'
          (EmptyParam, (Matrix Double, Matrix Double))
          ((EmptyParam, (Matrix Double, Matrix Double)), Matrix Double)
          (Matrix Double)
      x = dense (4, 3) identityL

  let l :: Lens' ((EmptyParam, (Matrix Double, Matrix Double)), Matrix Double) (Matrix Double)
      l = plens x
  let ps = params x

  let input :: Matrix Double
      input = fromList 4 1 [4.9, 3.0, 1.4, 0.2]

  --   print (bimap shape shape ps)
  print (shape input)

  print ps
  print (fwd l (ps, input))
  print (rev l (input, (ps, input)))

testDense' :: IO ()
testDense' = do
  let p :: Para' ((EmptyParam, (Matrix Double, Matrix Double)), (Matrix Double, (EmptyParam, (Matrix Double, Matrix Double)))) 
           (((EmptyParam, (Matrix Double, Matrix Double)), (Matrix Double, (EmptyParam, (Matrix Double, Matrix Double)))), Matrix Double)
           (Matrix Double)
      p@(Para ps ln) = dense (4, 2) identityL |.| linearP (2, 3) |.| dense (3, 5) identityL
  let input = fromList 4 1 [4.9, 3.0, 1.4, 0.2]

  print (fwd ln (ps, input))

testMSE :: IO ()
testMSE = do
  let x :: Matrix Double
      x = fromList 4 1 [4.9, 3.0, 1.4, 0.2]
  let y :: Matrix Double
      y = fromList 4 1 [4.4, 3.5, 1.4, 0.7]

  let l :: Lens' (Matrix Double, Matrix Double) ()
      l = mse . lr 1.0

  print (fwd mse (x, y))
  print (fwd l (x, y))

testPipeline :: IO ()
testPipeline = do
  let model :: Para' (EmptyParam, (Matrix Double, Matrix Double)) ((EmptyParam, (Matrix Double, Matrix Double)), Matrix Double) (Matrix Double)
      model = dense (4, 3) identityL
  -- let umodel :: Lens' ((Matrix Double, Matrix Double), Matrix Double) (Matrix Double)
  --     umodel = update >> plens model

  -- print (fwd umodel (params model, input))

  let input = fromList 4 1 [4.9, 3.0, 1.4, 0.2]
  let fpb = fromList 3 1 [0.0, 0.0, 0.0]
  let fpl =
        fromLists
          [ [-0.01517759, -0.00055333, -0.00697705, -0.0004722],
            [-0.01130874, 0.00753457, 0.00460682, -0.00851338],
            [-0.00154599, -0.00427765, 0.0001421, -0.00557464]
          ]
  let fps = (fpb, fpl)

  let ret = fromList 3 1 [1, 2, 3]

  -- print $ fwd (plens model) (fps, input)
  -- print $ rev (plens model) (ret, (fps, input))

  print (ret, (fps, input))

-- print (fwd umodel (params model, input))

testPipeline' :: IO ()
testPipeline' = do
  let q :: Para' (EmptyParam, (Matrix Double, Matrix Double)) ((EmptyParam, (Matrix Double, Matrix Double)), Matrix Double) (Matrix Double)
      q = dense (2, 2) identityL

  let j :: Para' (Matrix Double) (Matrix Double, Matrix Double) (Matrix Double)
      j = linearP (2, 2)
  let s = biasP 2

  let bp = fromList 2 1 [1, 1]
  let lp = fromLists [[1, 2], [3, 4]]
  let inp = fromList 2 1 [7, 8]
  -- let z = fwd (plens q) ((bp, lp), inp)

  let ret = fromList 2 1 [9, 9]

  --   let ptl = assocL . (identityL `alongside` linear) . add

  let ps :: (Matrix Double, ((Matrix Double, Matrix Double), Matrix Double))
      ps = (ret, ((bp, lp), inp))

  let nadd :: Num a => Lens (a, a) (d, d) a d
      nadd = lens v u
        where
          -- v :: Num i => (i, i) -> i
          v = uncurry (+)

          u :: (d, (a, a)) -> (d, d)
          u = join (,) . fst

  --   let r = rev nadd ps
  -- let h = rev (plens j) (ret, (lp, inp))
  -- let n = rev (plens s) (ret, (bp, inp))

  --   print r
  print 1

-- x :: Para p (p, s) (p, t) a b
-- x = undefined

-- y :: Para q (q, a) (q, b) m n
-- y = undefined

-- z :: Para r (r, m) (r, n) i j
-- z = undefined

-- yz :: Para (r, q) ((r, q), a) ((r, q), b) i j
-- yz = y |.| z

-- xy :: Para (q, p) ((q, p), s) ((q, p), t) m n
-- xy = x |.| y

-- xyz1 :: Para (r, (q, p)) ((r, (q, p)), s) ((r, (q, p)), t) i j
-- xyz1 = xy |.| z
-- xyz2 :: Para ((r, q), p) (((r, q), p), s) (((r, q), p), t) i j
-- xyz2 = x |.| yz

-- xyz1 = (assocR `alongside` id) . plens xyz2
-- xyz2 = (assocL `alongside` id) . plens xyz1