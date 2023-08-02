{-# OPTIONS_GHC -w -W #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Playground.ComponentTests where

import Numeric.LinearAlgebra (Matrix, fromLists, Vector, fromList)
import OpticML (Para', Lens, Lens', Para(..), dense, identityL, rev, lr, liftPara, mse, sigmoid, update, alongside, (|.|), EmptyParam(..), fwd, linearP)

type ModelParam = (EmptyParam, (Vector Double, Matrix Double))
type Result = Vector Double
type Input = Vector Double
type Expected = Vector Double
type Loss = Double

type LRParam = EmptyParam
type LossParam = Vector Double
type LearnerParam = ((LRParam, LossParam), ModelParam)

testModel :: IO ()
testModel = do
  let m :: Matrix Double
      m =
        fromLists
          [ [1, 2, 3],
            [4, 5, 6]
          ]

  let x :: Vector Double
      x = fromList [7, 8, 9]

  let y :: Vector Double
      y = fromList [150, 222]

  let b :: Vector Double
      b = fromList [100, 100]

  let loss :: Vector Double
      loss = fromList [10, 20]

  let m' :: Matrix Double
      m' =
        fromLists
          [ [70, 80, 90],
            [140, 160, 180]
          ]

  let x' :: Vector Double
      x' = fromList [90, 120, 150]

  let pl1 :: Para' (Matrix Double) (Matrix Double, Vector Double) (Vector Double)
      pl1 = linearP (3, 2)
  let res1 = fwd (plens pl1) (m, x) -- [50 122]
  let res2 = rev (plens pl1) (loss, (m, x)) -- ([[70, 80, 90], [140, 160, 180]], [90, 120, 150])

  let pl2 :: Para' ModelParam (ModelParam, Input) Result
      pl2 = dense (5, 3) identityL
  let res3 = fwd (plens pl2) ((EmptyParam, (b, m)), x) -- [150, 222]
  let res4 = rev (plens pl2) (loss, ((EmptyParam, (b, m)), x))

  print res1
  print res2
  print res3
  print res4

-- print (params pl)

testCap :: IO ()
testCap = do
  let loss :: Vector Double
      loss = fromList [10, 20]

  let l :: Lens s Double () ()
      l = lr 0.01
  let pc = liftPara l

  let res1 = fwd (plens pc) (params pc, loss) -- ()
  let res2 = rev (plens pc) ((), (loss, ())) -- (loss, 0.01)

  print res1
  print res2

testLoss :: IO ()
testLoss = do
  let y :: Vector Double
      y = fromList [150, 222]

  let ey :: Vector Double
      ey = fromList [140, 202]

  let l :: Lens' (Vector Double, Vector Double) Double
      l = mse

  let lr :: Double
      lr = -0.01

  let pl :: Para' (Vector Double) (Vector Double, Vector Double) Double
      pl = Para (fromList []) l

  let res1 = fwd (plens pl) (y, ey)
  let res2 = rev (plens pl) (lr, (y, ey))

  print res1
  print res2

testActivation :: IO ()
testActivation = do
  let m :: Matrix Double
      m = fromLists [[0.01, -0.02, 0.03], [0.04, 0.05, -0.06]]

  let x :: Vector Double
      x = fromList [-0.07, 0.08, 0.09]

  let y :: Vector Double
      y = fromList [0.0004, -0.0042]

  let b :: Vector Double
      b = fromList [1.0, 1.0]

  let loss :: Vector Double
      loss = fromList [0.1, 0.2]

  let pl :: Para' ModelParam (ModelParam, Input) Result
      pl = dense (5, 3) sigmoid
  let res1 = fwd (plens pl) ((EmptyParam, (b, m)), x)
  let res2 = rev (plens pl) (loss, ((EmptyParam, (b, m)), x))

  print res1
  print res2

testPipeline :: IO ()
testPipeline = do
  let model :: Para' ModelParam (ModelParam, Input) Result
      model = dense (4, 3) sigmoid

  let x :: Input
      x = fromList [-0.07, 0.08, 0.09, 0.06]

  let ey :: Vector Double
      ey = fromList [1.0, 0, 0]

  let mwu :: Para' ModelParam (ModelParam, Input) Result
      mwu = Para (params model) ((update `alongside` id) . plens model)

  let cap :: Para LRParam (LRParam, Loss) (LRParam, Loss) () ()
      cap = liftPara $ lr 0.01

  let loss :: Para LossParam (Result, Expected) (Result, Result) Loss Double
      loss = Para (fromList []) mse

  let learner :: Para' LearnerParam (LearnerParam, Input) ()
      learner = mwu |.| (loss |.| cap)

  let input :: (((LRParam, Expected), ModelParam), Input)
      input = (((lrp, ey), mp), x)
        where
          ((lrp, _), mp) = params learner

  -- let res = fwd (plens mwu) (params model, x)
  -- let res = fwd (plens learner) input
  let (((nPlr, nPmse), (nPact, (nb, nm))), x') = rev (plens learner) ((), input)
  -- let res = rev (plens pl) (loss, (((), (b, m)), x))

  -- print res
  print x'

-- print $ params learner
