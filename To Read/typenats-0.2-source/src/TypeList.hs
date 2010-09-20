{-
 -
 -  Copyright 2005-2006, Robert Dockins.
 -
 -}

module TypeList where

import TypeNats

data TyTrue
data TyFalse

class TyBool a where toBool :: a -> Bool
instance TyBool TyTrue  where toBool _ = True
instance TyBool TyFalse where toBool _ = False

data Nil
data Cons a b

class List a
instance List Nil
instance List b => List (Cons a b)

class Take n l w | n l -> w
instance Take n Nil Nil
instance ( Take2 n' b p
         , NatNormalize n n'
         , Trichotomy n Z () Nil (Cons a p) w
         ) => Take n (Cons a b) w

class Take2 n b p | n b -> p
instance                           Take2 Z     b ()
instance (Take (O n) b w)       => Take2 (I n) b w
instance ( NatPred n n'
         , Take (I n') b w)     => Take2 (O n) b w

tyTake :: Take n l w => n -> l -> w
tyTake = undefined
