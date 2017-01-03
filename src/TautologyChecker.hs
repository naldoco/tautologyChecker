module TautologyChecker where

data Prop = Const Bool
          | Var   Char
          | Not   Prop
          | And   Prop Prop
          | Or    Prop Prop
          | Imply Prop Prop deriving Show

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']
    
isTaut :: Prop -> Bool
isTaut _ = False

eval :: Subst -> Prop -> Bool
eval = undefined
                     
