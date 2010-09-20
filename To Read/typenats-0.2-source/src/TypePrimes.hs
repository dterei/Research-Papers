{-
 -
 -  Copyright 2006, Robert Dockins.
 -
 -}

module TypePrimes where

import TypeNats
import TypeList

-- | Remove all multiples of @p@ from the list @l@
class PrimeSieve p l w | p l -> w
instance PrimeSieve p Nil Nil
instance ( PrimeSieve p b w'
         , DivMod a p q r
         , Trichotomy r Z () w' (Cons a w') w
         ) => PrimeSieve p (Cons a b) w

primeSieve :: PrimeSieve p l w => p -> l -> w
primeSieve = undefined


-- | Generate a list containing all naturals staring with @x@
--   and counting upwards to contain @n@ elements.
class FromCount x n l | x n -> l
instance FromCount x Z Nil

instance ( NatSucc x x'
         , NatPred (O n) (I n')
	 , NatNormalize (I n') n''
         , FromCount x' n'' b
         ) => FromCount x (O n) (Cons x b)

instance ( NatSucc x x'
         , NatPred (I n) (O n)
         , NatNormalize (O n) n''
         , FromCount x' n'' b
         ) => FromCount x (I n) (Cons x b)


tyFromCount :: FromCount x n l => x -> n -> l
tyFromCount = undefined


-- | Recursivly run the prime sive over the given list @l@.
class RunSieve l w | l -> w
instance RunSieve Nil Nil
instance ( PrimeSieve p l l'
         , RunSieve l' l'' 
         ) => RunSieve (Cons p l) (Cons p l'')

-- | Generate a list of all primes up to @x@.
class PrimesUpTo x w | x -> w
instance ( Sub x One x'
         , FromCount Two x' l
         , RunSieve l w
         ) => PrimesUpTo x w

tyPrimesUpTo :: PrimesUpTo x w => x -> w
tyPrimesUpTo = undefined


-- | Check if the given natural @x@ is in the list @l@.
class NumIn x l w | x l -> w
instance NumIn x Nil TyFalse
instance ( NumIn x z w'
         , Trichotomy y x w' TyTrue TyFalse w
         ) => NumIn x (Cons y z) w

tyNumIn :: NumIn x l w => x -> l -> w
tyNumIn = undefined


-- | Check if @x@ is a prime number.  Return @TyTrue@ if so,
--   or @TyFalse@ if not.  Zero and one are not considered prime.
class IsPrime x w | x -> w
instance ( PrimesUpTo x l
         , NumIn x l w
         ) => IsPrime x w

tyIsPrime :: IsPrime x b => x -> b
tyIsPrime = undefined
