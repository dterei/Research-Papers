{-
 -
 -  Copyright 2005-2006, Robert Dockins.
 -
 -}

module Nats where

import Test.QuickCheck

data Nat
  = Z
  | O Nat
  | I Nat
  | CantSubtract
 deriving Show

natToIntegral :: Integral a => Nat -> a
natToIntegral Z = fromInteger 0
natToIntegral (O a) = let x = natToIntegral a in x+x
natToIntegral (I a) = let x = natToIntegral a in succ (x+x)
natToIntegral CantSubtract = error "subtraction failure"

integralToNat :: Integral a => a -> Nat
integralToNat n
   | n < 0       = error "cannot convert negative integrals"
   | n == 0      = Z
   | m == 0      = O (integralToNat d)
   | otherwise   = I (integralToNat d)
 where (d,m) = n `divMod` 2
       
instance Arbitrary Nat where
   arbitrary = sized (\size -> do
                  x <- choose (0,size+1)
                  resize (size-1)
                    (case x of { x
                      | x == 0         -> return Z
                      | x `mod` 2 == 0 -> arbitrary >>= return . O
                      | otherwise      -> arbitrary >>= return . I }))

   coarbitrary Z     = variant 0
   coarbitrary (O x) = variant 1 . coarbitrary x
   coarbitrary (I x) = variant 2 . coarbitrary x


eqNat :: Nat -> Nat -> Bool
eqNat Z Z = True
eqNat Z (O x) = eqNat Z x
eqNat (O x) Z = eqNat x Z
eqNat (O x) (O y) = eqNat x y
eqNat (I x) (I y) = eqNat x y
eqNat _ _ = False

instance Eq Nat where (==) = eqNat

natSucc :: Nat -> Nat
natSucc Z = (I Z)
natSucc (O x) = (I x)
natSucc (I x) = O (natSucc x)

add :: Nat -> Nat -> Nat
add Z x = x
add x Z = x

add (O a) (O b) = O (add a b)
add (I a) (O b) = I (add a b)
add (O a) (I b) = I (add a b)
add (I a) (I b) = O (addC a b)

addC :: Nat -> Nat -> Nat
addC Z x = natSucc x
addC x Z = natSucc x

addC (O a) (O b) = I (add a b)
addC (I a) (O b) = O (addC a b)
addC (O a) (I b) = O (addC a b)
addC (I a) (I b) = I (addC a b)

sub :: Nat -> Nat -> Nat
sub a Z = a
sub Z (O b) = sub Z b
sub Z (I b) = CantSubtract

sub (O a) (O b) = case sub a b of CantSubtract -> CantSubtract; x -> O x
sub (I a) (I b) = case sub a b of CantSubtract -> CantSubtract; x -> O x
sub (I a) (O b) = case sub a b of CantSubtract -> CantSubtract; x -> I x
sub (O a) (I b) = case borrow a of 
                    CantSubtract -> CantSubtract; 
                    a' -> case sub a' b of 
                            CantSubtract -> CantSubtract
			    x -> I x

borrow :: Nat -> Nat
borrow Z = CantSubtract
borrow (I x) = O x
borrow (O x) = case borrow x of CantSubtract -> CantSubtract; y -> I y


mul :: Nat -> Nat -> Nat
mul a (O b) = mul (O a) b
mul a (I b) = add a (mul (O a) b)
mul a Z = Z


natDivMod :: Nat -> Nat -> (Nat,Nat)
natDivMod a b = startDiv b a Z Z

-- reverse the digits
startDiv :: Nat -> Nat -> Nat -> Nat -> (Nat,Nat)
startDiv (O d) (O r) w z = startDiv d (O r) (O w) z
startDiv (O d) (I r) w z = startDiv d (I r) (O w) z
startDiv (O d) Z     w z = startDiv d Z     (O w) z
startDiv (I d) (O r) w z = startDiv d (O r) (I w) z
startDiv (I d) (I r) w z = startDiv d (I r) (I w) z
startDiv (I d) Z     w z = startDiv d Z     (I w) z

startDiv Z (O r) w z = startDiv Z r w (O z)
startDiv Z (I r) w z = startDiv Z r w (I z)

startDiv Z Z     w z = startDiv2 w z

-- throw away extra zeros
startDiv2 :: Nat -> Nat -> (Nat,Nat)
startDiv2 (O w) Z     = startDiv2 w Z
startDiv2 (O w) (O z) = startDiv2 w z
startDiv2 (O w) (I z) = startDiv2 w (I z)
startDiv2 (I w) (O z) = startDiv2 (I w) z
startDiv2 (I w) Z     = startDiv3 (I w) Z Z Z
startDiv2 (I w) (I z) = startDiv3 (I w) (I z) Z Z
startDiv2 Z     z     = error "division by zero"

-- start unreversing the nubmers until all digits of
-- the divisor are in proper order
startDiv3 :: Nat -> Nat -> Nat -> Nat -> (Nat,Nat)
startDiv3 (O w) (O z) d r = startDiv3 w z (O d) (O r)
startDiv3 (O w) (I z) d r = startDiv3 w z (O d) (I r)
startDiv3 (O w) Z     d r = startDiv3 w Z (O d) r
startDiv3 (I w) (O z) d r = startDiv3 w z (I d) (O r)
startDiv3 (I w) (I z) d r = startDiv3 w z (I d) (I r)
startDiv3 (I w) Z     d r = startDiv3 w Z (I d) r
startDiv3 Z     z     d r = doDiv z d r Z

-- do the long division
doDiv :: Nat -> Nat -> Nat -> Nat -> (Nat,Nat)
doDiv Z d r q = 
    case sub r d of
        CantSubtract -> (O q,r)
        Z            -> (I q,Z)
        O x          -> (I q,O x)
        I x          -> (I q,I x)

doDiv (O z) d r q = 
    case sub r d of
        CantSubtract -> doDiv z d (O r)     (O q)
        Z            -> doDiv z d Z         (I q)
        O x          -> doDiv z d (O (O x)) (I q)
        I x          -> doDiv z d (O (I x)) (I q)

doDiv (I z) d r q = 
    case sub r d of
        CantSubtract -> doDiv z d (I r)     (O q)
        Z            -> doDiv z d (I Z)     (I q)
        O x          -> doDiv z d (I (O x)) (I q)
        I x          -> doDiv z d (I (I x)) (I q)

natDiv :: Nat -> Nat -> Nat
natDiv x y = let (q,_) = natDivMod x y in q

natMod :: Nat -> Nat -> Nat
natMod x y = let (_,r) = natDivMod x y in r

zero      =                 Z
one       =  (I             Z)
two       =  (O (I          Z))
three     =  (I (I          Z))
four      =  (O (O (I       Z)))
five      =  (I (O (I       Z)))
six       =  (O (I (I       Z)))
seven     =  (I (I (I       Z)))
eight     =  (O (O (O (I    Z))))
nine      =  (I (O (O (I    Z))))
ten       =  (O (I (O (I    Z))))
eleven    =  (I (I (O (I    Z))))
twelve    =  (O (O (I (I    Z))))
thirteen  =  (I (O (I (I    Z))))
fourteen  =  (O (I (I (I    Z))))
fifteen   =  (I (I (I (I    Z))))
sixteen   =  (O (O (O (O (I Z)))))
seventeen =  (I (O (O (O (I Z)))))
eighteen  =  (O (I (O (O (I Z)))))
nineteen  =  (I (I (O (O (I Z)))))

{-
main = do let x = nineteen; y =  six
          --putStrLn $ show $ doDiv Z (I (I Z)) (O (O (I (O Z)))) (I (O Z))
          putStrLn $ show $ natToIntegral $ natDiv x y
          putStrLn $ show $ natToIntegral $ natMod x y
-}