{-# LANGUAGE GADTs #-}

module ExList where

import Prelude hiding
    ( (.) , ($)
    , flip , curry , uncurry
    , iterate
    )

-- use your mind to infer the types, don't cheat!
data Nat where
    O :: Nat
    S :: Nat -> Nat

zero, one, two, three, four, five, six, seven, eight, nine :: Nat
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

-- curry takes a "traditional" binary function
-- and returns its currified version
curry :: ((x, y) -> z) -> x -> y -> z
curry f x y = f (x, y)

-- uncurry takes a currified function
-- and returns its "traditional" binary version
uncurry :: (x -> y -> z) -> (x, y) -> z
uncurry f (x, y) = f x y

-- flip takes a (currified) binary function
-- and returns one that behaves the same but takes its arguments in the opposite order

-- (.) takes two composable functions and returns their composition

-- (.>) is composition but in diagramatic notation (should be ; but Haskell forbids)

flip :: (x -> y -> z) -> y -> x -> z
flip f x y = f y x
 
(.) :: (y -> z) -> (x -> y) -> x -> z
(f . g) x = f (g x)

(.>) :: (x -> y) -> (y -> z) -> x -> z
(.>) = flip (.)

-- ($) takes a function and a suitable argument and applies the function to the argument
-- think: why would we ever want that?
($) :: (x -> y) -> x -> y
f $ x = f x

-- iterate: figure it out by its type
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

-- orbit
orbit :: y -> (y -> y) -> [y]
orbit = flip iterate

