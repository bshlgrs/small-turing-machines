module MyList where

import Prelude (fst, Bool (True), (&&))

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x:(xs ++ ys)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && (and xs)

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ (concat xs)

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) = if p x then x:filter p xs else filter p xs

all :: (a -> Bool) -> [a] -> Bool
all p list = and (map p list)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

--- Elem is hard

