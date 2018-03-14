module ExampleModule
where
import Prelude (Show, Eq)

data Nat = Z | S Nat
  deriving (Eq)

(+) :: Nat -> Nat -> Nat
(+) x y = case x of
  Z -> y
  S x2 -> S (x2 + y)

-- myNot True = False
-- myNot False = True


f :: Nat -> Nat -> Nat
f a b = a + b

two :: (a -> a) -> a -> a
two s z = s (s z)

-- all :: (a -> Bool) -> [a] -> Bool
-- all p list = and (map p list)
