module Iris
    ( runPipeline
    ) where

import qualified Data.ByteString.Lazy as BSL
import Data.Csv ( decode, HasHeader(HasHeader) )
import qualified Data.Vector as V

import OpticML (Para', Para(..), dense, sigmoid, update, alongside, liftPara, lr, mse, (|.|), rev, fwd, EmptyParam(..))
import Numeric.LinearAlgebra (Matrix, Vector, Konst (konst), accum, fromList, toList, Numeric)
import qualified Data.List as DL

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe ( fromJust, fromMaybe )

type IrisData = (Double, Double, Double, Double, String)
type Features = Vector Double
type Label = Vector Double

type LRParam = EmptyParam
type LossParam = Vector Double
type ActParam = EmptyParam

type ModelParam = (ActParam, (Vector Double, Matrix Double))
type LearnerParam = ((LRParam, LossParam), ModelParam)

type Result = Vector Double
type Input = Vector Double
type Expected = Vector Double
type Loss = Double

type Model = Para' ModelParam (ModelParam, Input) Result
type Learner = Para' LearnerParam (LearnerParam, Input) ()

type OHClassMap = [(String, Int)]

ohMap :: OHClassMap
ohMap = [("Iris-setosa", 0), ("Iris-versicolor", 1), ("Iris-virginica", 2)]

oneHot :: [(String, Int)] -> String -> Label
oneHot opts c = accum (konst 0 3) (const id) [(look c opts, 1.0)]
    where
        look :: String -> OHClassMap -> Int
        look = (.) fromJust . DL.lookup

formatIrisData :: IrisData -> (Features, Label)
formatIrisData (sl, sw, pl, pw, c) = ( fromList [sl, sw, pl, pw]
                                     , oneHot ohMap c
                                     )

ingestData :: String -> IO (Either String [(Features, Label)])
ingestData fp = do
    loaded <- BSL.readFile fp
    return $ V.toList . V.map formatIrisData <$> decode HasHeader loaded

runStep :: Learner -> (Features, Label) -> ModelParam
runStep lrn (fs, ey) = nM
    where
        input = (((lrp, ey), mp), fs)
        ((lrp, _), mp) = params lrn
        -- (((nPlr, nPmse), nM), x')
        (((_, _), nM), _) = rev (plens lrn) ((), input)

runEpoch :: ModelParam -> [(Features, Label)] -> ModelParam
runEpoch = foldl (runStep . (fst . constructLearner) . Just)

constructLearner :: Maybe ModelParam -> (Learner, ModelParam)
constructLearner omp = (mwu |.| (loss |.| cap), mp)
    where
        model :: Model
        model = dense (4, 3) sigmoid

        mp :: ModelParam
        mp = fromMaybe (params model) omp

        mwu :: Para' ModelParam (ModelParam, Input) Result
        mwu = Para mp ((update `alongside` id) . plens model)

        cap :: Para LRParam (LRParam, Loss) (LRParam, Loss) () ()
        cap = liftPara $ lr 0.01

        loss :: Para LossParam (Result, Expected) (Result, Result) Loss Double
        loss = Para (fromList []) mse

argMax :: (Ord a, Numeric a) => Vector a -> Int
argMax = fst . maximumBy (comparing snd) . zip [0..] . toList

accuracy :: [(Features, Label)] -> ModelParam -> Double
accuracy td mp = fromIntegral (length matches) / fromIntegral (length td)
    where
        mod :: Model
        mod = dense (4, 3) sigmoid

        compute fs = fwd (plens mod) (mp, fs)
        matches = filter (\(fs, l) -> argMax (compute fs) == argMax l) td

runLearning :: Int -> [(Features, Label)] -> IO ()
runLearning epochs td = do
    let (_, mp) = constructLearner Nothing
    let params :: [(Int, ModelParam)]
        params = zip [1..] $ iterate (`runEpoch` td) mp

    let printAcc :: (Int, ModelParam) -> IO ()
        printAcc (e, m) = putStrLn $  "epoch "               ++ show e 
                                 ++ "\ttraining accuracy " ++ show (accuracy td m)

    putStr "training..."
    mapM_ printAcc (take epochs params)

runPipeline :: Int -> String -> IO ()
runPipeline epochs fp = do
    trainData <- ingestData fp
    either putStrLn (runLearning epochs) trainData
