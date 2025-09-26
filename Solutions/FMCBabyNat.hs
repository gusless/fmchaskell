module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
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
nine = S eight -- adicionei o 9, pois estava feio

false = zero
true = one

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = true
isZero (S _) = false

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S x) = x

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = true
even (S O) = false
even (S (S x)) = even x

odd :: Nat -> Nat
odd O = false
odd (S O) = true
odd (S (S x)) = odd x

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
(*) :: Nat -> Nat -> Nat
O * _ = O
x * (S y) = x * y + x

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
_ ^ O = one
x ^ (S y) = x * (x ^ y)

-- decide: infix? ? ^
infixr 8 ^

(>=) :: Nat -> Nat -> Nat
O >= O = true
O >= (S _) = false
(S _) >= O = true
(S n) >= (S m) = n >= m

-- quotient
(/) :: Nat -> Nat -> Nat
_ / O = undefined
O / _ = O
x / y =
  if x >= y == true
    then S ((x -* y) / y)
    else O 

infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
_ % O = undefined
x % y = x -* (y * (x / y))

infixl 7 %

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) = undefined

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined

