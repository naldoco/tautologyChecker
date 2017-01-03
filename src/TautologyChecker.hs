module TautologyChecker where

data Prop = Const Bool
          | Var   Char
          | Not   Prop
          | And   Prop Prop
          | Imply Prop Prop deriving Show

-- First past the post
isTaut :: Prop -> Bool
isTaut = undefined
