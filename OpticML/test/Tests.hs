module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import OpticML

main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing OpticML" [lensUnitTests]

lensUnitTests = testGroup "Lens Unit Tets (run via HUnit)"
    [ testCase "view p1" $
        view p1 (1, 2) @?= 1
    , testCase "set p1" $
        set p1 3 (1, 2) @?= (3, 2)
    , testCase "over p1" $
        over p1 (+1) (1, 2) @?= (2, 2)
    ]
