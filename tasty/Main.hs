module Main
    where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "List comparison (different length)" $
         [1, 2, 3] `compare` [1, 2] @?= GT
    ]

main :: IO ()
main = defaultMain tests
