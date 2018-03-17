module ExampleModule
where
import Prelude (Show)

data Bool = True | False
data Nat = Z | S Nat

-- (&&) :: Bool -> Bool -> Bool
-- (&&) x y = case (x, y) of
--   (True, True) -> True
--   _ -> False

-- mainVal = True && False

-- decrement :: Nat -> Nat
-- decrement Z = Z
-- decrement (S x) = x

-- infinity :: Nat
-- infinity = S infinity

-- (==) :: Nat -> Nat -> Bool
-- (==) a b = case (a, b) of
--   (Z, Z) -> True
--   (Z, _) -> False
--   (_, Z) -> False
--   (S a2, S b2) -> a2 == b2

churchify :: Nat -> (Bool -> Bool) -> Bool -> Bool
churchify n f z = case n of
  Z -> z
  S n -> f (churchify n f z)

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

-- type Church = (Nat -> Nat) -> Nat -> Nat

-- addChurchily :: Church -> Church -> Church
-- addChurchily m n s z = (m s) (n s z)

-- multiplyChurchily :: Church -> Church -> Church
-- multiplyChurchily m n s z = m (n s) z

mainVal = churchify (factorial ((S Z + (S (S Z))) + (S (S Z))))

--- multiplyChurchily (\x y -> x (x y)) (\x y -> x (x y))-- churchify (S (S (S Z))) -- (factorial (S (S (S Z)))

-- not True = False
-- not False = True

-- f :: Nat -> Nat -> Nat
-- f a b = a + b

-- two :: (a -> a) -> a -> a
-- two s d = let first = s d in s first

-- all :: (a -> Bool) -> [a] -> Bool
-- all p list = and (map p list)

-- and :: [Bool] -> Bool
-- and [] = True
-- and (x:xs) = x && and xs

-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x:xs) = f x :
