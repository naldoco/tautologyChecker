module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TautologyChecker

p1 :: Prop
p1 = And (Var 'A') (Not  (Var 'A'))

p7 :: Prop
p7 = And (Var 'A') (Not  (Var 'B'))

aFbT :: Subst
aFbT = [('A', False), ('B', True)]



tautologyCheckerSuite :: TestTree
tautologyCheckerSuite =
  testGroup "Tautology checker tests"
    [ testGroup "isTaut"
        [ testCase ("isTaut "++(show p1)++" -> "++show False) $
            (isTaut p1) @?= False
        ]
    , testGroup "eval"
        [ testCase ("eval ("++(show aFbT)++") ("++(show p7)++") -> "++show False) $
            (eval aFbT p7) @?= False
        ]
    ]
main = defaultMain tautologyCheckerSuite
