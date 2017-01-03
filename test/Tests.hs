module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TautologyChecker

p1 :: Prop
p1 = And (Var 'A') (Not  (Var 'A'))

tautologyCheckerSuite :: TestTree
tautologyCheckerSuite =
  testGroup "Tautology checker tests"
    [ testGroup "isTaut"
        [ testCase ("isTaut "++(show p1)++" -> "++show False) $
            (isTaut p1) @?= False
        ]
    ]
main = defaultMain tautologyCheckerSuite
