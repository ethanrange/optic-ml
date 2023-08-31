module Demo where

import Numeric.LinearAlgebra
import OpticML

type Features = Vector Double
type Label = Vector Double

type ModelParam = ( (EmptyParam, (Vector Double, Matrix Double))
                  , (EmptyParam, (Vector Double, Matrix Double))
                  )
type LearnerParam = ((EmptyParam, Vector Double), ModelParam)

type Model = Para' ModelParam (ModelParam, Label) Features
type Pipeline = Para' LearnerParam (LearnerParam, Features) ()


model = dense (4, 5) relu |.| dense (5, 3) sigmoid
modelWithUpdate = Para (params model) ((update `alongside` id) . plens model)

learningRate = liftPara $ lr 0.01
loss = Para (fromList []) mse

pipeline :: Pipeline
pipeline = modelWithUpdate |.| (loss |.| learningRate)

predict :: Model -> Features -> Label
predict m fs = fwd (plens m) (params m, fs)

learn :: Pipeline -> (Features, Label) -> Pipeline
learn p (feats, expected) = p { params = ((lrParam, lossParam), newModelParams)}
    where
        ((lrParam, lossParam), modelParams) = params p
        input = (((lrParam, expected), modelParams), feats)
        
        (((_, _), newModelParams), _) = rev (plens p) ((), input)

features :: Features
features = fromList [5.1, 3.5, 1.4, 0.2]

label :: Label
label = fromList [1, 0, 0]