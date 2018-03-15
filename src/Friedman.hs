{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Friedman (
  friedmanCounterexample
) where

import Data.Bool

import Data.Bits
import Data.Word
import Data.Char
import Prelude (
  -- Mathematical operators
  (+), (-), (*), mod, div, gcd, (==), (<), (>), (<=), (>=),
  -- Haskell necessities that won't be compiled
  Integer, Show, Eq, Ord,
  -- List functions
  (++), and, concat, filter, all, elem, null, any, head, concatMap, reverse, map, or,
  -- Specialized stuff
  ($), return, fst, snd)
import Data.List (nub, genericTake, genericLength)

type List a = [a]
type Pair a = (a, a)

-- ---------- Bools
-- data Bool = True | False

-- (&&) :: Bool -> Bool -> Bool
-- (&&) True True = True
-- (&&) _ _ = False

-- (||) :: Bool -> Bool -> Bool
-- (||) True True = True
-- (||) _ _ = False

-- not True = False
-- not False = True

----------- Natural numbers

-- data Nat = Z | S Nat

-- (==) :: Integer -> Integer -> Bool
-- (==) a b = case (a, b) of
--   (Z, Z) -> True
--   (Z, _) -> False
--   (_, Z) -> False
--   (S a2, S b2) -> a2 == b2

-- (+) :: Integer -> Integer -> Nat
-- (+) x y = case x of
--   Z -> y
--   S x2 -> S (x2 + y)

-- (*) :: Integer -> Integer -> Nat
-- (*) x y = case x of
--   Z -> Z
--   S x2 -> y + (x2 * y)

-- ------ Pairs

-- data Pair a = Pair a a

-- fst :: Pair a -> a
-- fst (Pair x _) = x

-- snd :: Pair a -> a
-- snd (Pair _ y) = y

------------ Lists

-- data List a = Nil | Cons a (List a)

-- (:) :: a -> List a -> List a
-- (:) x xs = Cons x xs

-- singleton :: a -> List a
-- singleton x = Cons x Nil

-------

type Graph a = List (Pair a)

ush :: List Integer -> List Integer
ush set = map (\x -> if x < 0 then x else x + 1) set

allSubsetsWithSizeAtMostK :: List a -> Integer -> List (List a)
allSubsetsWithSizeAtMostK set k = do
  size <- [0..k]
  setOfThatSize <- allSubsetsWithSize set size
  return setOfThatSize

allSubsetsWithSize :: List a -> Integer -> List (List a)
allSubsetsWithSize set 0 = [[]]
allSubsetsWithSize [] _ = []
allSubsetsWithSize (item:rest) size = subsetsUsingItem ++ subsetsNotUsingItem
  where
    subsetsUsingItem = map (item:) (allSubsetsWithSize rest (size - 1))
    subsetsNotUsingItem = allSubsetsWithSize rest size

allSubsets :: List a -> List (List a)
allSubsets set = allSubsetsWithSizeAtMostK set (genericLength set)

isListFree :: List Integer -> Graph Integer -> Bool
isListFree set graph = not $ or [elem (x, y) graph || elem (y, x) graph
                                 | x <- set, y <- set]

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

setReducesOtherList :: Graph Node -> List Node -> List Node -> Bool
setReducesOtherList graph xsNode ysNode =
  all
    (\y ->
      any
        (\x ->
          x == y ||
          (leqRlex x y && elem (x, y) graph)
        )
        xsNode
    )
    ysNode

type Node = List Integer

orderEquivalent :: (List Integer, List Integer) -> (List Integer, List Integer) -> Bool
orderEquivalent (a, b) (c, d) = genericLength a == genericLength c
                              && genericLength b == genericLength d
                              && orderingList a b == orderingList c d
  where
    orderingList :: List Integer -> List Integer -> List Bool
    orderingList xs ys = [x < y | x <- xs, y <- ys]

allVertices :: Graph Node -> List Node
allVertices graph = nub $ concatMap (\x -> [fst x, snd x]) graph

graphIsOrderInvariant :: Graph Node -> Bool
graphIsOrderInvariant graph = and fourTupleOfEdgesIsOrderEquivalent
  where
    graphVertices = allVertices graph
    fourTupleOfEdgesIsOrderEquivalent = do
      a <- graphVertices
      b <- graphVertices
      c <- graphVertices
      d <- graphVertices
      return (not (orderEquivalent (a, b) (c, d)) ||
              elem (a, b) graph == elem (c, d) graph)

--- I think the paper is ambiguous about how to define this
leqRlex :: Node -> Node -> Bool
leqRlex x y = (reverse x) <= (reverse y)

allNaturalTrios :: List (Integer, Integer, Integer)
allNaturalTrios = allNaturalTrios' 0 0 0
  where
    allNaturalTrios' :: Integer -> Integer -> Integer -> List (Integer, Integer, Integer)
    allNaturalTrios' a 0 0 = (a, 0, 0) : allNaturalTrios' (a + 1) (a + 1) (a + 1)
    allNaturalTrios' a b 0 = (a, b, 0) : allNaturalTrios' a (b - 1) a
    allNaturalTrios' a b c = (a, b, c) : allNaturalTrios' a b (c - 1)

friedmanCounterexample :: (Integer, Integer, Integer)
friedmanCounterexample = head $ filter checkTrio allNaturalTrios

-- returns True if the trio contains a counterexample
checkTrio :: (Integer, Integer, Integer) -> Bool
checkTrio (k, n, r) = any (\edges -> isValidGraph vertices edges k n r) edgeSelections
  where
    numbers = allIntegersWithBoundedComplexity k n r
    --  we generate the nodes in a potential order invariant graph by
    -- adding to N all possible lists of k or fewer numbers from N .
    -- We call this list of lists V .
    vertices = allSubsetsWithSizeAtMostK numbers k
    edgeSelections = allSubsets [(x, y) | x <- vertices, y <- vertices, x > y]

-- returns true if there is a
isValidGraph :: List (List Integer) -> Graph Node -> Integer -> Integer -> Integer -> Bool
isValidGraph vertices edges k n r = not (null subsetsWhichAllReduce)
  where
    lengthRSubsets :: List (List Node)
    lengthRSubsets = allSubsetsWithSize vertices r

    subsetsWithUshes :: List (List Node)
    subsetsWithUshes = filter (\s -> all (\x -> elem (ush x) vertices) s) lengthRSubsets

    subsetsWhichAllReduce :: List (List Node)
    subsetsWhichAllReduce = filter testListAllReduces subsetsWithUshes

    testListAllReduces :: List Node -> Bool
    testListAllReduces set = and [setReducesOtherList edges (lhsThing i) (rhsThing i) |
                                  i <- [1..factorial (8 * k * n * r)]]
      where
        -- {x1, . . . , x_{(8kni)}!}
        lhsThing :: Integer -> List Node
        lhsThing i = genericTake (factorial (8 * k * n * i)) set

        rhsThing :: Integer -> List Node
        rhsThing i = allSubsetsWithSizeAtMostK (nub (concat (genericTake i set) ++ [0..n])) k


allIntegersWithBoundedComplexity :: Integer -> Integer -> Integer -> List Integer
allIntegersWithBoundedComplexity k n r = 0 : rest
  where
    rest = [sign *
            factorial (factorial (eightKN * r)) *
            factorial (eightKN * i) `div`
            factorial (eightKN * j)
            | sign <- [1, -1],
              i <- [1..eightKN * r],
              j <- [1..eightKN * r],
              gcd i j == 1]
    eightKN = 8 * k * n


