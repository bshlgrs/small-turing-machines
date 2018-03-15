module ExampleModule
where
import Prelude (Show)

data Bool = True | False
data Nat = Z | S Nat

decrement :: Nat -> Nat
decrement Z = Z
decrement (S x) = x

infinity :: Nat
infinity = S infinity

(==) :: Nat -> Nat -> Bool
(==) a b = case (a, b) of
  (Z, Z) -> True
  (Z, _) -> False
  (_, Z) -> False
  (S a2, S b2) -> a2 == b2

(+) :: Nat -> Nat -> Nat
(+) x y = case x of
  Z -> y
  S x2 -> S (x2 + y)

(*) :: Nat -> Nat -> Nat
(*) x y = case x of
  Z -> Z
  S x2 -> y + (x2 * y)

factorial :: Nat -> Nat
factorial n = case n of
  Z -> S Z
  (S n2) -> n * factorial n2

-- -- myNot True = False
-- -- myNot False = True


-- f :: Nat -> Nat -> Nat
-- f a b = a + b

-- two :: (a -> a) -> a -> a
-- two s d = let first = s d in s first

-- -- all :: (a -> Bool) -> [a] -> Bool
-- -- all p list = and (map p list)
