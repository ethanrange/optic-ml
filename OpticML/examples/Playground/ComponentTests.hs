{-# OPTIONS_GHC -w -W #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Playground.ComponentTests where

import Data.Matrix (Matrix, fromLists, fromList)
import OpticML (Para', Lens, Lens', Para(..), dense, identityL, rev, lr, liftPara, mse, sigmoid, update, alongside, (|.|), EmptyParam(..))

type ModelParam = (EmptyParam, (Matrix Double, Matrix Double))
type Result = Matrix Double
type Input = Matrix Double
type Expected = Matrix Double
type Loss = Double

type LRParam = EmptyParam
type LossParam = Matrix Double
type LearnerParam = ((LRParam, LossParam), ModelParam)

testModel :: IO ()
testModel = do
  let m :: Matrix Double
      m =
        fromLists
          [ [1, 2, 3],
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
      m' =
        fromLists
          [ [70, 80, 90],
            [140, 160, 180]
          ]

  let x' :: Matrix Double
      x' = fromList 3 1 [90, 120, 150]

  -- let pl :: Para' (Matrix Double) (Matrix Double, Matrix Double) (Matrix Double)
  --     pl = linearP (3, 2)
  -- let res = fwd (plens pl) (m, x) -- [50 122]
  -- let res = rev (plens pl) (loss, (m, x)) -- ([[70, 80, 90], [140, 160, 180]], [90, 120, 150])

  let pl :: Para' ModelParam (ModelParam, Input) Result
      pl = dense (5, 3) identityL
  -- let res = fwd (plens pl) (((), (b, m)), x) -- [150, 222]
  let res = rev (plens pl) (loss, ((EmptyParam, (b, m)), x))

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

  let pl :: Para' ModelParam (ModelParam, Input) Result
      pl = dense (5, 3) sigmoid
  -- let res = fwd (plens pl) (((), (b, m)), x)
  let res = rev (plens pl) (loss, ((EmptyParam, (b, m)), x))

  print res

testPipeline :: IO ()
testPipeline = do
  let model :: Para' ModelParam (ModelParam, Input) Result
      model = dense (4, 3) sigmoid

  let x :: Input
      x = fromList 4 1 [-0.07, 0.08, 0.09, 0.06]

  let ey :: Matrix Double
      ey = fromList 3 1 [1.0, 0, 0]

  let mwu :: Para' ModelParam (ModelParam, Input) Result
      mwu = Para (params model) ((update `alongside` id) . plens model)

  let cap :: Para LRParam (LRParam, Loss) (LRParam, Loss) () ()
      cap = liftPara $ lr 0.01

  let loss :: Para LossParam (Result, Expected) (Result, Result) Loss Double
      loss = Para (fromLists [[]]) mse

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
