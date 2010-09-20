{-
 -
 -  Copyright 2005-2006, Robert Dockins.
 -
 -}

{- | This module defines type-level natural numbers and arithmetic operation on them
     including addition, subtraction, multiplication, division and GCD.
   
     Numbers are represented as a list of binary digits, terminated by a distinguished type 'Z'.
     Least significant digits are outermost, which makes the numbers little-endian when read.

     Because a binary representation is used, reasonably large numbers can be
     represented.  I have personally done tests with numbers at the order
     of 10^15 in GHC.  However, larger numbers require the GHC \'-fcontext-stack\' option.
     For example, the test suite sets \'-fcontext-stack64\'.

     Because of the limitations of typeclasses, some of the algorithms are pretty
     messy.  The division algorithm is particularly ugly.  Suggestions for improvements
     are welcome.
 -}

module TypeNats where

-- | Terminates a list of binary digits with an imagined infinity of zeros.
data Z   

-- | A zero bit.
data O a

-- | A one bit.
data I a -- a one bit

type Zero      =                 Z
type One       =  (I             Z)
type Two       =  (O (I          Z))
type Three     =  (I (I          Z))
type Four      =  (O (O (I       Z)))
type Five      =  (I (O (I       Z)))
type Six       =  (O (I (I       Z)))
type Seven     =  (I (I (I       Z)))
type Eight     =  (O (O (O (I    Z))))
type Nine      =  (I (O (O (I    Z))))
type Ten       =  (O (I (O (I    Z))))
type Eleven    =  (I (I (O (I    Z))))
type Twelve    =  (O (O (I (I    Z))))
type Thirteen  =  (I (O (I (I    Z))))
type Fourteen  =  (O (I (I (I    Z))))
type Fifteen   =  (I (I (I (I    Z))))
type Sixteen   =  (O (O (O (O (I Z)))))
type Seventeen =  (I (O (O (O (I Z)))))
type Eighteen  =  (O (I (O (O (I Z)))))
type Nineteen  =  (I (I (O (O (I Z)))))

type Twenty    =  (O (O (I (O (I Z)))))
type Thirty    =  (O (I (O (I (I Z)))))
type Fourty    =  (O (O (O (I (O (I Z))))))
type Fifty     =  (O (I (O (O (I (I Z))))))
type Sixty     =  (O (O (I (I (I (I Z))))))
type Seventy   =  (O (I (I (O (O (O (I Z)))))))
type Eighty    =  (O (O (O (O (I (O (I Z)))))))
type Ninety    =  (O (I (O (I (I (O (I Z)))))))
type Hundred   =  (O (O (I (O (O (I (I Z)))))))

-- Hexidecimal digits
type Ox0 a = (O (O (O (O a))))
type Ox1 a = (I (O (O (O a))))
type Ox2 a = (O (I (O (O a))))
type Ox3 a = (I (I (O (O a))))
type Ox4 a = (O (O (I (O a))))
type Ox5 a = (I (O (I (O a))))
type Ox6 a = (O (I (I (O a))))
type Ox7 a = (I (I (I (O a))))
type Ox8 a = (O (O (O (I a))))
type Ox9 a = (I (O (O (I a))))
type Oxa a = (O (I (O (I a))))
type Oxb a = (I (I (O (I a))))
type Oxc a = (O (O (I (I a))))
type Oxd a = (I (O (I (I a))))
type Oxe a = (O (I (I (I a))))
type Oxf a = (I (I (I (I a))))

-- some larger numbers
type Thousand = Ox8 (Oxe (Ox3 Z))
type Million  = Ox0 (Ox4 (Ox2 (Ox4 (Oxf Z))))
type Billion  = Ox0 (Ox0 (Oxa (Oxc (Oxa (Ox9 (Oxb (Ox3 Z)))))))

-- | The Natural class, with conversion to intergral values.
--   The conversion should be linear in the number of bits, which
--   is logarithmic in the magnitute of the number.

class Natural a where
  naturalToIntegral :: Integral b => a -> b

instance Natural Z where 
  naturalToIntegral _ = fromInteger 0

instance Natural a => Natural (O a) where
  naturalToIntegral _ = let x = naturalToIntegral (undefined::a) in x+x

instance Natural a => Natural (I a) where
  naturalToIntegral _ = let x = naturalToIntegral (undefined::a) in succ (x+x)

-- | Zero predicate.  Zero is represented by a single 'Z' or
--   a string of 'O' terminated by 'Z'.
class IsZero a 
instance IsZero Z
instance IsZero a => IsZero (O a)

-- | The equality relation on nats.
class EqNat a b
instance (IsZero a)  => EqNat a Z
instance (EqNat Z b) => EqNat Z (O b)
instance (EqNat a b) => EqNat (O a) (O b)
instance (EqNat a b) => EqNat (I a) (I b)

-- | The successor function; defined for all naturals.
class (Natural a, Natural b) => NatSucc a b | a -> b
instance                NatSucc Z (I Z)
instance Natural a   => NatSucc (O a) (I a)
instance NatSucc a b => NatSucc (I a) (O b)

natSucc :: (NatSucc a b) => a -> b
natSucc = undefined

-- | The predecessor function; not defined for zero.
class (Natural a, Natural b) => NatPred a b | a -> b
instance Natural a   => NatPred (I a) (O a)
instance NatPred a b => NatPred (O a) (I b)

natPred :: NatPred a b => a -> b
natPred = undefined


-- | Binary addition. This is a pretty basic full adder.
--   The 'AddC' class represents add with carry.
--   Addition is defined for all pairs of natural numbers.

class (Natural a,Natural b,Natural c) => Add a b c | a b -> c
instance                 Add Z Z Z
instance (Natural a)  => Add Z (O a) (O a)
instance (Natural a)  => Add Z (I a) (I a)
instance (Natural a)  => Add (O a) Z (O a)
instance (Natural a)  => Add (I a) Z (I a)

instance Add  a b c   => Add (O a) (O b) (O c)
instance Add  a b c   => Add (I a) (O b) (I c)
instance Add  a b c   => Add (O a) (I b) (I c)
instance AddC a b c   => Add (I a) (I b) (O c)

class (Natural a,Natural b,Natural c) => AddC a b c | a b -> c
instance                 AddC Z Z (I Z)
instance Natural a    => AddC Z (O a) (I a)
instance NatSucc a b  => AddC Z (I a) (O b)
instance Natural a    => AddC (O a) Z (I a)
instance NatSucc a b  => AddC (I a) Z (O b)

instance Add  a b c   => AddC (O a) (O b) (I c)
instance AddC a b c   => AddC (I a) (O b) (O c)
instance AddC a b c   => AddC (O a) (I b) (O c)
instance AddC a b c   => AddC (I a) (I b) (I c)

add :: Add a b c => a -> b -> c
add = undefined 




-- | A distinguished error type which is returned when
--   subtraction is impossible.
data CantSubtract


-- | Binary subtraction. Slightly less elegant than the adder but it works.
--   'DoSub' returns a distingushed error type if a < b and '()' if the subtraction suceeds.

class (Natural a, Natural b) => Sub a b c | a b -> c
instance DoSub a b c ()           => Sub a b c

class (Natural a, Natural b) => DoSub a b c d | a b -> c d
instance Natural a             => DoSub a Z a ()
instance DoSub Z b c d         => DoSub Z (O b) c d  -- this rule does not loop, because
                                                     -- the b parameter is decreasing
instance Natural b             => DoSub Z (I b) () CantSubtract

instance DoSub a b c d         => DoSub (O a) (O b) (O c) d
instance DoSub a b c d         => DoSub (I a) (O b) (I c) d
instance DoSub a b c d         => DoSub (I a) (I b) (O c) d
instance SubBorrow b c a z z d => DoSub (O a) (I b) (I c) d

sub :: Sub a b c => a -> b -> c
sub = undefined

-- this is tricky, we use the top level call to SubBorrow
-- to unify the final result with z, so that is is avaliable
-- when we want to call Sub again
class (Natural b,Natural x) => SubBorrow b c x y z d | x z b -> c d, x -> y

instance Natural b                 => SubBorrow b () Z    ()    z CantSubtract
instance (Natural x,DoSub z b c d) => SubBorrow b c (I x) (O x) z d
instance SubBorrow b c x y z d     => SubBorrow b c (O x) (I y) z d

{-
  how we used to do it.  This way can't return the distinguished
  CantSubtract type when borrowing fails.  We need that distinguisged
  type in order to do the tests we need for division and GCD

  instance (Borrow a a',Sub a' b c) => Sub (O a) (I b) (I c)

  class Borrow a b | a -> b
  instance               Borrow (I a) (O a)
  instance Borrow a b => Borrow (O a) (I b)
-}


class Trichotomy n m lt eq gt z | n m lt eq gt -> z
instance (DoSub n m p a, DoSub m n q b, DoTrichotomy a b lt eq gt z) => Trichotomy n m lt eq gt z

class DoTrichotomy a b lt eq gt z | a b lt eq gt -> z
instance DoTrichotomy CantSubtract () lt eq gt lt
instance DoTrichotomy () () lt eq gt eq
instance DoTrichotomy () CantSubtract lt eq gt gt


-- comparison relations on naturals, defined
-- in terms of subtraction

-- | The less-than-or-equal-to relation
class LEqNat a b
instance (DoSub b a c ())           => LEqNat a b

-- | The greater-than-or-equal-to relation
class GEqNat a b
instance (DoSub a b c ())           => GEqNat a b

-- | The less-than relation
class LTNat a b
instance (DoSub a b c CantSubtract) => LTNat a b

-- | The greater-than relation
class GTNat a b
instance (DoSub b a c CantSubtract) => GTNat a b


-- | Binary multiplication. This is a 
--   simple shift and add peasant multiplier.

class Mul a b c | a b -> c
instance Mul a Z Z
instance (Mul (O a) b x)               => Mul a (O b) x
instance (Mul (O a) b c,Add a c x)     => Mul a (I b) x

mul :: Mul a b c => a -> b -> c
mul = undefined




-- | Division by 2.
--   This is real easy, just throw away the outermost
--   type constructor.

class (Natural a,Natural b) => Div2 a b | a -> b
instance                   Div2 Z Z
instance Natural a      => Div2 (O a) a
instance Natural a      => Div2 (I a) a

div2 :: Div2 a b => a -> b
div2 = undefined


-- | Normalize a nat; that is, remove all leading zeros.

class NatNormalize a b | a -> b

instance (NatRev a Z c
         ,NatNorm c c'
         ,NatRev c' Z a'
         ) 
     => NatNormalize a a'

class NatRev a b c | a b -> c
instance                       NatRev Z b b
instance NatRev a (O b) c  =>  NatRev (O a) b c
instance NatRev a (I b) c  =>  NatRev (I a) b c

class NatNorm a b | a -> b
instance                       NatNorm Z Z
instance                       NatNorm (I a) (I a)
instance NatNorm a b       =>  NatNorm (O a) b


normalize :: NatNormalize a b => a -> b
normalize = undefined



-- | A distinguished error type returned when 
--   attempting to divide by zero.
data DivideByZero

-- | Binary division and modulus. This one is suprisingly difficult to implement.
class DivMod a d q r | a d -> q r
instance ( NatRev a Z a'
         , NatRev d Z d'
         , PreDivMod a' d' (q,r)
         ) => DivMod a d q r

-- here we throw away leading zeros and test for 
-- a zero divisor
class PreDivMod a d z | a d -> z
instance PreDivMod  Z     d         z => PreDivMod Z     (O d) z
instance PreDivMod  a     d         z => PreDivMod (O a) (O d) z
instance PreDivMod  (I a) d         z => PreDivMod (I a) (O d) z
instance PreDivMod  a     (I d)     z => PreDivMod (O a) (I d) z
instance PreDivMod2 Z     (I d) Z Z z => PreDivMod Z     (I d) z
instance PreDivMod2 (I a) (I d) Z Z z => PreDivMod (I a) (I d) z
instance                                 PreDivMod a     Z     DivideByZero

-- now we begin to "unreverse" until all the divisor
-- digits are in the correct order
class PreDivMod2 a w d r z | a w d r -> z
instance PreDivMod2 Z w (O d) r     z => PreDivMod2 Z     (O w) d r z
instance PreDivMod2 a w (O d) (O r) z => PreDivMod2 (O a) (O w) d r z
instance PreDivMod2 a w (O d) (I r) z => PreDivMod2 (I a) (O w) d r z
instance PreDivMod2 Z w (I d) r     z => PreDivMod2 Z     (I w) d r z
instance PreDivMod2 a w (I d) (O r) z => PreDivMod2 (O a) (I w) d r z
instance PreDivMod2 a w (I d) (I r) z => PreDivMod2 (I a) (I w) d r z
instance DoDivMod a d Z r z           => PreDivMod2 a     Z     d r z

-- now we do binary long division
-- first we do case analysis on the 
-- remining dividend, then case analysis
-- on the difference between the current
-- remainder and the dividend....

class DoDivMod a d q r z | a d q r -> z
instance ( DoSub r d x err
         , DoDivModZ err x q r z
         ) => DoDivMod Z d q r z

instance ( DoSub r d x err
         , DoDivModO err x a d q r z
         ) => DoDivMod (O a) d q r z

instance ( DoSub r d x err
         , DoDivModI err x a d q r z
         ) => DoDivMod (I a) d q r z


class DoDivModZ err x q r z | err x q r -> z
instance DoDivModZ () Z     q r (I q,r)
instance DoDivModZ () (O x) q r (I q,O x)
instance DoDivModZ () (I x) q r (I q,I x)
instance DoDivModZ CantSubtract x q r (O q,r)

class DoDivModO err x a d q r z | err x a d q r -> z
instance DoDivMod a d (I q) Z         z => DoDivModO () Z     a d q r z
instance DoDivMod a d (I q) (O (O x)) z => DoDivModO () (O x) a d q r z
instance DoDivMod a d (I q) (O (I x)) z => DoDivModO () (I x) a d q r z
instance DoDivMod a d (O q) (O r)     z => DoDivModO CantSubtract x a d q r z

class DoDivModI err x a d q r z | err x a d q r -> z
instance DoDivMod a d (I q) (I Z)     z => DoDivModI () Z     a d q r z
instance DoDivMod a d (I q) (I (O x)) z => DoDivModI () (O x) a d q r z
instance DoDivMod a d (I q) (I (I x)) z => DoDivModI () (I x) a d q r z
instance DoDivMod a d (O q) (I r)     z => DoDivModI CantSubtract x a d q r z


natDivMod :: DivMod a b q r => a -> b -> (q,r)
natDiv    :: DivMod a b q r => a -> b -> q
natMod    :: DivMod a b q r => a -> b -> r

natDivMod = undefined
natDiv    = undefined
natMod    = undefined


-- | A distinguished error type returned when
--   an attempt is made to take the GCD of 0 and 0.
data GCDZeroZero

-- | Greatest Common Divisor (GCD).
--   Here we use the binary euclidian algorithm.

--   there is a little fancy dancing to with DoGCD2
--     in order to replace the largest argument with
--     their difference

class GCD a b c | a b -> c
instance ( NatNormalize a a'
         , NatNormalize b b'
         , DoGCD a' b' c
         )  => GCD a b c

class DoGCD a b c | a b -> c
instance                            DoGCD Z     Z     GCDZeroZero
instance                            DoGCD (O a) Z     (O a)
instance                            DoGCD (I a) Z     (I a)
instance                            DoGCD Z     (O a) (O a)
instance                            DoGCD Z     (I a) (I a)
instance DoGCD a b c             => DoGCD (O a) (O b) (O c)
instance DoGCD a (I b) c         => DoGCD (O a) (I b) c
instance DoGCD (I a) b c         => DoGCD (I a) (O b) c

-- in this branch we want to replace the largest argument with
-- the absolute value of their difference.  All this nastyness
-- is necessary to be able to figure out which one is bigger and
-- subtract the correct way.

instance (DoSub a b x1 f1
         ,DoSub b a x2 f2
         ,DoGCD2 x1 x2 f1 f2
           (I a) (I b) c)        => DoGCD (I a) (I b) c


class DoGCD2 x1 x2 f1 f2 a b c | x1 x2 f1 f2 a b -> c

-- handle GCD(x,x) case.  x and y must be (possibly distinct) representations of zero
instance (IsZero x,IsZero y)          => DoGCD2 x  y  () () a b a

-- replace the larger with the difference
instance DoGCD (O x1) b c             => DoGCD2 x1 x2 () CantSubtract a b c
instance DoGCD a (O x2) c             => DoGCD2 x1 x2 CantSubtract () a b c


natGCD :: GCD a b c => a -> b -> c
natGCD = undefined



-- define a bunch of useful naturals

zero      :: Zero
one       :: One
two       :: Two
three     :: Three
four      :: Four
five      :: Five
six       :: Six
seven     :: Seven
eight     :: Eight
nine      :: Nine
ten       :: Ten
eleven    :: Eleven
twelve    :: Twelve
thirteen  :: Thirteen
fourteen  :: Fourteen
fifteen   :: Fifteen
sixteen   :: Sixteen
seventeen :: Seventeen
eighteen  :: Eighteen
nineteen  :: Nineteen

twenty    :: Add Twenty  b c => b -> c
twenty_   :: Twenty
thirty    :: Add Thirty  b c => b -> c
thirty_   :: Thirty
fourty    :: Add Fourty  b c => b -> c
fourty_   :: Fourty
fifty     :: Add Fifty   b c => b -> c
fifty_    :: Fifty
sixty     :: Add Sixty   b c => b -> c
sixty_    :: Sixty
seventy   :: Add Seventy b c => b -> c
seventy_  :: Seventy
eighty    :: Add Eighty  b c => b -> c
eighty_   :: Eighty
ninety    :: Add Ninety  b c => b -> c
ninety_   :: Ninety

hundred   :: (Mul Hundred  a b,Add b x y) => a -> x -> y
thousand  :: (Mul Thousand a b,Add b x y) => a -> x -> y
million   :: (Mul Million  a b,Add b x y) => a -> x -> y
billion   :: (Mul Billion  a b,Add b x y) => a -> x -> y

infixr 5 `billion`
infixr 5 `million`
infixr 5 `thousand`
infix  6 `hundred`

zero      = undefined
one       = undefined
two       = undefined
three     = undefined
four      = undefined
five      = undefined
six       = undefined
seven     = undefined
eight     = undefined
nine      = undefined
ten       = undefined
eleven    = undefined
twelve    = undefined
thirteen  = undefined
fourteen  = undefined
fifteen   = undefined
sixteen   = undefined
seventeen = undefined
eighteen  = undefined
nineteen  = undefined

twenty    = undefined
thirty    = undefined
fourty    = undefined
fifty     = undefined
sixty     = undefined
seventy   = undefined
eighty    = undefined
ninety    = undefined
twenty_   = undefined
thirty_   = undefined
fourty_   = undefined
fifty_    = undefined
sixty_    = undefined
seventy_  = undefined
eighty_   = undefined
ninety_   = undefined

hundred   = undefined
thousand  = undefined
million   = undefined
billion   = undefined
