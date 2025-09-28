module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

f, v, t :: Bool
f = False
v = True
t = True

instance Show Bool where

    show False = "False"
    show True = "True" 

instance Enum Bool where

    toEnum 0 = False
    toEnum 1 = True
    toEnum _ = error "toEnum: o valor não é bool"

    fromEnum False = 0
    fromEnum True = 1

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
False && False = False
True && False = False
False && True = False
True && True = True

infixr 3 &&

and :: Bool -> Bool -> Bool
and = (&&)

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
False || False = False
True || False = True
False || True = True
True || True = True

infixr 2 ||

or :: Bool -> Bool -> Bool
or = (||)

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
x /|\ y = not (x && y)

infixr 2 /|\

nand :: Bool -> Bool -> Bool
nand = (/|\)

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
x \|/ y = not (x || y)

infixr 2 \|/

nor :: Bool -> Bool -> Bool
nor = (\|/)

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
x <=/=> y = (not x && y) || (x && not y)

infixr 2 <=/=>


xor :: Bool -> Bool -> Bool
xor = (<=/=>)

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x y = x
ifThenElse False x y = y

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
True ==> True = True
True ==> False = False
False ==> True = True
False ==> False = True

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
True <== True = True
False <== True = False
True <== False = True
False <== False = True  

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
x <=> y = (x ==> y) && (x <== y)

infixr 1 <=>


