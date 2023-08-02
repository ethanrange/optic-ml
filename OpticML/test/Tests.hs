{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import OpticML
import Numeric.LinearAlgebra (Matrix, Vector, fromLists, fromList)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing OpticML" [lensUnitTests, capUnitTests, modelUnitTests, lossUnitTests, activationUnitTests, pipelineUnitTests]

lensUnitTests = testGroup "Lens Unit Tets (run via HUnit)"
    [ testCase "view p1" $
        fwd p1 (1, 2) @?= 1
    , testCase "set p1" $
        rev p1 (3, (1, 2)) @?= (3, 2)
    , testCase "over p1" $
        over p1 (+1) (1, 2) @?= (2, 2)
    ]

type ModelParam = (EmptyParam, (Vector Double, Matrix Double))
type Result = Vector Double
type Input = Vector Double
type Expected = Vector Double
type Loss = Double

type LRParam = EmptyParam
type LossParam = Vector Double
type LearnerParam = ((LRParam, LossParam), ModelParam)

modelUnitTests = testGroup "Model Unit Tets (run via HUnit)"
    [ testCase "testLinearFwd" $ 
        fwd (plens pl1) (m, x) @?= fromList [50, 122]
    , testCase "testLinearRev" $ 
        rev (plens pl1) (loss, (m, x)) @?= (m', x')
    , testCase "testDenseFwd" $ 
        fwd (plens pl2) ((EmptyParam, (b, m)), x) @?= fromList [150, 222]
    , testCase "testDenseRev" $ 
        rev (plens pl2) (loss, ((EmptyParam, (b, m)), x)) @?= ((EmptyParam, (loss, m')), x')
    ]
    where
        m :: Matrix Double
        m = fromLists [[1, 2, 3], [4, 5, 6]]

        x :: Vector Double
        x = fromList [7, 8, 9]

        y :: Vector Double
        y = fromList [150, 222]

        b :: Vector Double
        b = fromList [100, 100]

        loss :: Vector Double
        loss = fromList [10, 20]

        m' :: Matrix Double
        m' =
                fromLists
                [ [70, 80, 90],
                    [140, 160, 180]
                ]

        x' :: Vector Double
        x' = fromList [90, 120, 150]

        pl1 :: Para' (Matrix Double) (Matrix Double, Vector Double) (Vector Double)
        pl1 = linearP (3, 2)

        pl2 :: Para' ModelParam (ModelParam, Input) Result
        pl2 = dense (5, 3) identityL

capUnitTests = testGroup "Cap Unit Tets (run via HUnit)"
    [ testCase "testCapFwd" $ 
        fwd (plens pc) (params pc, loss) @?= ()
    , testCase "testCapRev" $ 
        rev (plens pc) ((), (loss, ())) @?= (loss, -0.01)
    ]
    where
        loss :: Vector Double
        loss = fromList [10, 20]

        l :: Lens s Double () ()
        l = lr (-0.01)
        pc = liftPara l

lossUnitTests = testGroup "Loss Unit Tets (run via HUnit)"
    [ testCase "testLossFwd" $ 
        fwd (plens pl) (y, ey) @?= 250
    , testCase "testLossRev" $ 
        rev (plens pl) (lr, (y, ey)) @?= (fromList [], fromList [-0.1, -0.2])
    ]
    where
        y :: Vector Double
        y = fromList [150, 222]

        ey :: Vector Double
        ey = fromList [140, 202]

        l :: Lens' (Vector Double, Vector Double) Double
        l = mse

        lr :: Double
        lr = -0.01

        pl :: Para' (Vector Double) (Vector Double, Vector Double) Double
        pl = Para (fromList []) l

activationUnitTests = testGroup "Activation Unit Tets (run via HUnit)"
    [ testCase "testActivationFwd" $ 
        fwd (plens pl) ((EmptyParam, (b, m)), x) @?= fromList [0.7311372161343049, 0.7302320075828583]
    , testCase "testActivationRev" $ 
        rev (plens pl) (loss, ((EmptyParam, (b, m)), x)) @?= 
            ((EmptyParam, (fromList [1.965755873176836e-2, 3.9398644536873334e-2], 
                           fromLists [[-1.3760291112237854e-3, 1.5726046985414688e-3, 1.7691802858591523e-3], 
                                      [-2.757905117581134e-3, 3.151891562949867e-3, 3.5458780083186e-3]])
             ), fromList [1.772521368792617e-3, 1.5767810522082997e-3,-1.7741919102593493e-3])
    ]
    where
        m :: Matrix Double
        m = fromLists [[0.01, -0.02, 0.03], [0.04, 0.05, -0.06]]

        x :: Vector Double
        x = fromList [-0.07, 0.08, 0.09]

        y :: Vector Double
        y = fromList [0.0004, -0.0042]

        b :: Vector Double
        b = fromList [1.0, 1.0]

        loss :: Vector Double
        loss = fromList [0.1, 0.2]

        pl :: Para' ModelParam (ModelParam, Input) Result
        pl = dense (5, 3) sigmoid

pipelineUnitTests = testGroup "Pipeline Unit Tets (run via HUnit)"
    [ testCase "testPipelineRev" $ 
        rev (plens learner) ((), input) @?= ((  (EmptyParam, fromList []), 
                                                (EmptyParam, (nb, nm))), x')
    ]
    where
        model :: Para' ModelParam (ModelParam, Input) Result
        model = dense (4, 3) sigmoid
        
        x :: Input
        x = fromList [-0.07, 0.08, 0.09, 0.06]

        ey :: Vector Double
        ey = fromList [1.0, 0, 0]

        mwu :: Para' ModelParam (ModelParam, Input) Result
        mwu = Para (params model) ((update `alongside` id) . plens model)

        cap :: Para LRParam (LRParam, Loss) (LRParam, Loss) () ()
        cap = liftPara $ lr 0.01

        loss :: Para LossParam (Result, Expected) (Result, Result) Loss Double
        loss = Para (fromList []) mse

        learner :: Para' LearnerParam (LearnerParam, Input) ()
        learner = mwu |.| (loss |.| cap)

        input :: (((LRParam, Expected), ModelParam), Input)
        input = (((lrp, ey), mp), x)
                where
                ((lrp, _), mp) = params learner

        x' :: Vector Double
        x' = fromList [-6.776498013458829e-5, -2.176261941939712e-3, -1.1495594636975863e-3,  6.528513067859352e-4]

        nb :: Vector Double
        nb = fromList [1.3217990661356168e-3, -1.321785313330119e-3, -1.2005843745919909e-3]

        nm :: Matrix Double
        nm = fromLists [[-0.19262571524635072, -0.9583693677191268, -1.2018934446323384, 0.805625851575068],
                        [-0.13910418485692538, 1.3458636581796626, -0.3937374245822127, 0.6837179918588936],
                        [-2.195718051184199e-3, -0.7245200609151277, 6.737549424992297e-2, -0.4098007265059785]]
