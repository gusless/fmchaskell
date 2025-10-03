{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use elem" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use infix" #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import GHC.Base (TrName(TrNameD))

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "empty list"
head (x : xs) = x

tail :: [a] -> [a]
tail [] = error "empty list"
tail (x : xs) = xs

null :: [a] -> Bool
null [] = True
null (_:_) = False

length :: Integral i => [a] -> i
length [] = 0
length (_ : xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 0
product [x] = x
product (x : xs) =  x * product xs

(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(x : xs) ++ ys = x : (xs ++ ys)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc x xs = xs ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a

minimum [] = error "empty list"
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

take :: Int -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_ : xs) = drop (n-1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x = dropWhile p xs
  | otherwise = x : xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs 

init :: [a] -> [a]
init [] = error "empty list"
init [x] = []
init (x : xs) = x : init xs

addEach :: a -> [[a]] -> [[a]]
addEach _ [] = []
addEach x (p : ps) = (x : p) : addEach x ps

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = 
  let ps = inits xs in
    [] : addEach x ps

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) = 
  let ps = subsequences xs in
    ps ++ addEach x ps

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x : xs) = p x || any p xs

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p (x : xs) =
  if p x then all p xs
  else False

and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x : xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (lx : lxs) = lx ++ concat lxs   


-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x xs = any (== x) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys)
  | x == y = True
  | otherwise = elem' x ys


(!!) :: [a] -> Int -> a
[] !! _ = error "index out of bounds"
(x : xs) !! 0 = x
(x : xs) !! n
  | n < 0 = error "index out of bounds"
  | otherwise = xs !! (n - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x : xs) =
  if p x then x : filter p xs
  else filter p xs


map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

cycle :: [a] -> [a]
cycle [] = error "empty list"
cycle xs = xs ++ cycle xs 

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate 0 x = []
replicate n x = x : replicate (n -1) x

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = (x == y) && isPrefixOf xs ys 

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf xs ys
  | length xs > length ys = False
  | otherwise = isInfixOf xs ys


isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf xs (y : ys)
  | isSuffixOf xs ys = True
  | otherwise = drop (length ys - length xs) ys == xs

zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f _ [] = []
zipWith f [] _ = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

intercalate :: [a] -> [[a]] -> [a] 
intercalate _ [] = []
intercalate _ [x] = x 
intercalate e (x : xs) = x ++ e ++ intercalate e xs 

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) = nub (filter (/= x) xs)

splitAt :: Int -> [a] -> ([a],[a])
splitAt _ [] = ([], [])
splitAt 0 xs = ([], xs)
splitAt n (x : xs) =
  let (ys, zs) = splitAt (n - 1) xs
  in (x : ys, zs)

-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

break :: (a -> Bool) -> [a] -> ([a],[b])
break f [] = ([], [])
break f (x : xs)
  | f x = ([], x : xs)
  | otherwise =
    let (ys, zs) = break f xs
    in (x : ys, zs)

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)

normalize :: String -> String
normalize s = [C.toLower c | c <- s, C.isAlphaNum c]

palindrome :: String -> Bool
palindrome s = normalize s == reverse (normalize s)

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

