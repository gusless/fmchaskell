{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

zero, one, two, three, four, five, six, seven, eight, nine, false, true :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven
nine  = S eight

false = zero
true = one
----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    O == O = True
    S x == S y = x == y
    _ == _ = False

instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    O <= _ = True
    S _ <= O = False
    S x <= S y = x <= y

    (<) :: Nat -> Nat -> Bool
    O < O = False
    O < _ = True
    _ < O = False
    S x < S y = x < y

    (>) :: Nat -> Nat -> Bool
    O > O = False
    O > _ = False
    _ > O = True
    S x > S y = x > y

    (>=) :: Nat -> Nat -> Bool
    O >= _ = False
    S x >= O = True
    S x >= S y = x >= y

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min :: Nat -> Nat -> Nat
    min O _ = O
    min _ O = O
    min (S x) (S y) = S (min x y)

    max :: Nat -> Nat -> Nat
    max O y       = y
    max (S x) O   = S x
    max (S x) (S y) = S (max x y)


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S x) = x

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S x)) = even x

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S x)) = odd x


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O   = n
n <+> S m = S (n <+> m)

infixl 6 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (-*)

(-*) :: Nat -> Nat -> Nat
O -* _ = O
x -* O = x
(S x) -* (S y) = x -* y

infixl 6 -*

-- multiplication
times :: Nat -> Nat -> Nat
times = (<*>)

(<*>) :: Nat -> Nat -> Nat
O <*> _ = O
_ <*> O = O
x <*> (S y) = x <*> y + x

infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow = exp

exp :: Nat -> Nat -> Nat
exp = (<^>)

(<^>) :: Nat -> Nat -> Nat
_ <^> O = one
x <^> (S y) = x <^> y * x

infixr 8 <^>

-- quotient

(</>) :: Nat -> Nat -> Nat
(</>) = (//)

(//) :: Nat -> Nat -> Nat
_ // O = undefined
O // _ = O
x // y =
  if x >= y
    then S ((x -* y) // y)
    else O

infixl 7 //

-- remainder
(<%>) :: Nat -> Nat -> Nat
x <%> y = x -* (y <*> (x // y ))

infixl 7 <%>

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (x, y) = (x // y, x <%> y)

-- divides
(<|>) :: Nat -> Nat -> Bool
O <|> _ = True
_ <|> O = False
x <|> y = 
  case x <%> y of
    O -> True
    _ -> False

divides :: Nat -> Nat -> Bool
divides = (<|>)

-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = (|-|)

(|-|) :: Nat -> Nat -> Nat
x |-| y = 
  if x >= y
    then x -* y
    else y -* x


factorial :: Nat -> Nat
factorial = fat

fat :: Nat -> Nat
fat O = one
fat (S x) = S x * fat x

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg _ = one
--sg -x = undefined

-- lo b a is the floor of the logarithm base b of a
log :: Nat -> Nat -> Nat
log O _ = undefined
log _ O = undefined
log y x =
  if x >= y
    then S (log y (x // y))
    else O


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat x
  | x > 0 = S (toNat (x - 1))
  | otherwise = error "toNaT: número negativo"

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S x) = 1 + fromNat x


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) :: Nat -> Nat -> Nat
    (+) = (<+>)
    
    (*) :: Nat -> Nat -> Nat
    (*) = (<*>)

    (-) :: Nat -> Nat -> Nat
    (-) = (-*)
    
    abs :: Nat -> Nat
    abs n = n
    
    signum :: Nat -> Nat
    signum = sg

    fromInteger x
      | x < 0     = error "Número negativo"
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))

