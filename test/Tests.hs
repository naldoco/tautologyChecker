module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TautologyChecker

p1 :: Prop
p1 = And (Var 'A') (Not  (Var 'A'))

p2 :: Prop
p2 =  Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 =  Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 =  Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop   -- ¬(p∧q) ⇔ ¬p∨¬q
p5 = Equiv (Not (And (Var 'A') (Var 'B'))) (Or  (Not (Var 'A')) (Not (Var 'B')))

p6 :: Prop   -- ¬(p∨q) ⇔ ¬p∧¬q
p6 = Equiv (Not (Or  (Var 'A') (Var 'B'))) (And (Not (Var 'A')) (Not (Var 'B')))

ps :: [Prop]
ps = [p1,p2,p3,p4,p5,p6]

psAnswers :: [Bool]
psAnswers =  [False, True, False, True, True, True]

p7 :: Prop
p7 = And (Var 'A') (Not  (Var 'B'))

aFbT :: Subst
aFbT = [('A', False), ('B', True)]

p2v :: [Char]
p2v = ['A', 'B', 'A']

p3v :: [Char]
p3v = ['A', 'A', 'B']
       
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
    , testGroup "isTaut" $ map ( \(p, answ) ->
          testCase ("isTaut ("++show p ++ ") -> " ++show answ) $
            (isTaut p) @?= answ)
            (zip ps psAnswers)
    , testGroup "eval"
        [ testCase ("eval ("++(show aFbT)++") ("++(show p7)++") -> "++show False) $
            (eval aFbT p7) @?= False
        ]
    , testGroup "vars"
        [ testCase ("vars ("++(show p2)++") -> "++show p2v) $
            (vars p2) @?= p2v
        , testCase ("vars ("++(show p3)++") -> "++show p3v) $
            (vars p3) @?= p3v
        ]
    , testGroup "bools"
        [ testCase ("bools "++ "2"++" -> "++show bools2) $
            (bools 2) @?= bools2
        , testCase ("bools "++ "3"++" -> "++show bools3) $
            (bools 3) @?= bools3
        ]
    , testGroup "substs"
        [ testCase ("substs ("++(show p2)++") -> "++show p2s) $
            (substs p2) @?= p2s
        ]
    ]
main = defaultMain tautologyCheckerSuite
