{-
 -
 -  Copyright 2005-2006, Robert Dockins.
 -
 -}

module MkTypeNats where

import Test.HUnit
import System.Random
import Control.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Test.QuickCheck

import Nats ( Nat (..), natToIntegral, integralToNat )
import TypeNats

mkNat :: Nat -> ExpQ
mkNat Z     = conE 'Z
mkNat (O x) = appE (conE 'O) (mkNat x)
mkNat (I x) = appE (conE 'I) (mkNat x)

instance Lift Nat where lift = mkNat

mkTypeNat :: Nat -> TypeQ
mkTypeNat Z     = conT ''Z
mkTypeNat (I x) = appT (conT ''I) (mkTypeNat x)
mkTypeNat (O x) = appT (conT ''O) (mkTypeNat x)

mkTypeNatExpr :: Nat -> ExpQ
mkTypeNatExpr n = sigE [| undefined |] (mkTypeNat n)

mkTypeNatTest :: Nat -> Nat -> ExpQ
mkTypeNatTest n1 n2 = do
   let x1 = natToIntegral n1 :: Integer
   let x2 = natToIntegral n2 :: Integer
   let xa = if x1 < x2 then x1 else x2
   let xb = if x1 < x2 then x2 else x1
   let na = if x1 < x2 then n1 else n2
   let nb = if x1 < x2 then n2 else n1
   tnaName <- newName "tna"
   tnbName <- newName "tnb"
   leqName <- newName "leqTest"
   eqName  <- newName "eqTest"
   let tna  = varE tnaName
   let tnb  = varE tnbName
   let leq  = varE leqName
   let eq   = varE eqName
   letE [ funD tnaName [ clause [] (normalB (mkTypeNatExpr na)) [] ]
        , funD tnbName [ clause [] (normalB (mkTypeNatExpr nb)) [] ]
        , sigD leqName [t| LEqNat a b => a -> b -> Bool |]
        , funD leqName [ clause [] (normalB [| \_ _ -> True |]) [] ]
        , sigD eqName  [t| EqNat  a b => a -> b -> Bool |]
        , funD eqName  [ clause [] (normalB [| \_ _ -> True |]) [] ]
        ]
     [| TestLabel (concat ["Nat test with '",show x1,"' and '",show x2,"'"]) $ TestCase (do
           assertBool  "eq test"           ($eq $tna $tna)
           assertEqual "addition test 1"   (xa+xb) (naturalToIntegral ($tna `add` $tnb))
           assertEqual "addition test 2"   (xb+xa) (naturalToIntegral ($tnb `add` $tna))
           assertEqual "subtraction test"  (xb-xa) (naturalToIntegral ($tnb `sub` $tna))
           assertBool  "leqTest"           ($leq $tna $tnb)
           assertEqual "multiply test 1"   (xa*xb) (naturalToIntegral ($tna `mul` $tnb))
           assertEqual "multiply test 2"   (xb*xa) (naturalToIntegral ($tnb `mul`$tna))
           assertEqual "div2 test 1"       (xa `div` 2) (naturalToIntegral (div2 $tna))
           assertEqual "div2 test 2"       (xb `div` 2) (naturalToIntegral (div2 $tnb))
           assertEqual "gcd test 1"        (succ xa `gcd` succ xb) (naturalToIntegral (natSucc $tna `natGCD` natSucc $tnb))
           assertEqual "gcd test 2"        (succ xb `gcd` succ xa) (naturalToIntegral (natSucc $tnb `natGCD` natSucc $tna))
	   assertEqual "normalize test 1"  xa (naturalToIntegral (normalize $tna))
           assertEqual "normalize test 2"  xb (naturalToIntegral (normalize $tnb))
           assertEqual "div test 1"        (xb `div` succ xa) (naturalToIntegral ($tnb `natDiv` natSucc $tna))
           assertEqual "div test 2"        (xa `div` succ xb) (naturalToIntegral ($tna `natDiv` natSucc $tnb))
           assertEqual "mod test 1"        (xb `mod` succ xa) (naturalToIntegral ($tnb `natMod` natSucc $tna))
           assertEqual "mod test 2"        (xa `mod` succ xb) (naturalToIntegral ($tna `natMod` natSucc $tnb))
        )
     |]


pairList :: [Nat] -> [(Nat,Nat)]
pairList []       = []
pairList (a:[])   = (Z,a) : []
pairList (a:b:xs) = (a,b) : pairList xs

mkTypeNatTests :: Int -> Q [ExpQ]
mkTypeNatTests numTests = do
   stdGen <- runIO (getStdGen)
   let (stdGen1,stdGen') = split stdGen
   let (stdGen2,stdGen3) = split stdGen'
   let nats1 = generate 8  stdGen1 (vector (2*numTests))
   let nats2 = generate 16  stdGen2 (vector (2*numTests))
   let nats3 = generate 32 stdGen3 (vector (2*numTests))
   let nats' = pairList (nats1++nats2++nats3)
   return [ mkTypeNatTest x1 x2 | (x1,x2) <- nats' ]

mkTypeNatTestCase :: Int -> Q [Dec]
mkTypeNatTestCase numTests = do
  tests <- mkTypeNatTests numTests
  sequence [funD (mkName "typeNatTests") 
      [ clause [] (normalB (listE tests)) [] ]]
