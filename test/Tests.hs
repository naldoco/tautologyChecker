module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TautologyChecker

p1 :: Prop
p1 = And (Var 'A') (Not  (Var 'A'))

p2 :: Prop
p2 =  Imply (And (Var 'A') (Var 'B')) (Var 'A')

p7 :: Prop
p7 = And (Var 'A') (Not  (Var 'B'))

aFbT :: Subst
aFbT = [('A', False), ('B', True)]

p2v :: [Char]
p2v = ['A', 'B', 'A']

bools2 :: [[Bool]]
bools2 = [[False,False],[False,True],[True,False],[True,True]]

bools3 :: [[Bool]]
bools3 = [[False,False,False],
          [False,False,True ],
          [False,True ,False],
          [False,True ,True ],
          [True ,False,False],
          [True ,False,True ],
          [True ,True ,False],
          [True ,True ,True ]]
p1s :: [Subst]
p1s = [[('A', True)], [('A', False)]]
p2s :: [Subst]
p2s = [[('A',False),('B',False)],[('A',False),('B',True)],[('A',True),('B',False)],[('A',True),('B',True)]]

tautologyCheckerSuite :: TestTree
tautologyCheckerSuite =
  testGroup "Tautology checker tests"
    [ testGroup "isTaut"
        [ testCase ("isTaut ("++(show p1)++") -> "++show False) $
            (isTaut p1) @?= False
        ]
    , testGroup "eval"
        [ testCase ("eval ("++(show aFbT)++") ("++(show p7)++") -> "++show False) $
            (eval aFbT p7) @?= False
        ]
    , testGroup "vars"
        [ testCase ("vars ("++(show p2)++") -> "++show p2v) $
            (vars p2) @?= p2v
        ]
    , testGroup "bools"
        [ testCase ("bools "++ "2"++" -> "++show bools2) $
            (bools 2) @?= bools2
        ]
    , testGroup "substs"
        [ testCase ("substs ("++(show p2)++") -> "++show p2s) $
            (substs p2) @?= p2s
        ]
    ]
main = defaultMain tautologyCheckerSuite
