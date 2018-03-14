module Friedman (

) where

-- import Show
import Data.Bool

import Data.Bits
import Data.Word
import Data.Char
import Prelude (
  -- Mathematical operators
  (+), (-), (*), (/), mod, div, gcd, (==), (<), (>), (<=), (>=),
  -- Haskell necessities that won't be compiled
  Integer, Show, Eq, Ord,
  -- List functions
  (++), and, concat, filter, all, elem, null, any, head, concatMap, reverse, map, or,
  -- Specialized stuff
  ($), return, fst, snd)
import Data.List (nub, genericTake, genericLength)

type Set a = [a]
type Graph a = [(a, a)]

ush :: Set Integer -> Set Integer
ush set = map (\x -> if x < 0 then x else x + 1) set

allSubsetsWithSizeAtMostK :: Set a -> Integer -> [Set a]
allSubsetsWithSizeAtMostK set k = do
  size <- [0..k]
  setOfThatSize <- allSubsetsWithSize set size
  return setOfThatSize

allSubsetsWithSize :: Set a -> Integer -> [Set a]
allSubsetsWithSize set 0 = [[]]
allSubsetsWithSize [] _ = []
allSubsetsWithSize (item:rest) size = subsetsUsingItem ++ subsetsNotUsingItem
  where
    subsetsUsingItem = map (item:) (allSubsetsWithSize rest (size - 1))
    subsetsNotUsingItem = allSubsetsWithSize rest size

allSubsets :: Set a -> [Set a]
allSubsets set = allSubsetsWithSizeAtMostK set (genericLength set)

allBinaryListsOfLength :: Integer -> [[Bool]]
allBinaryListsOfLength 0 = [[]]
allBinaryListsOfLength n = do
  xs <- allBinaryListsOfLength (n - 1)
  newList <- [True:xs, False:xs]
  return newList

isSetFree :: (Eq a) => Set a -> Graph a -> Bool
isSetFree set graph = not $ or [elem (x, y) graph || elem (y, x) graph
                                 | x <- set, y <- set]

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

setReducesOtherSet :: Graph [Integer] -> Set Node -> Set Node -> Bool
setReducesOtherSet graph xsNode ysNode =
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

type Node = [Integer]

orderEquivalent :: Ord a => (Set a, Set a) -> (Set a, Set a) -> Bool
orderEquivalent (a, b) (c, d) = genericLength a == genericLength c
                              && genericLength b == genericLength d
                              && orderingList a b == orderingList c d
  where
    orderingList :: (Ord a) => [a] -> [a] -> [Bool]
    orderingList xs ys = [x < y | x <- xs, y <- ys]

allVertices :: Eq a => Graph a -> [a]
allVertices graph = nub $ concatMap (\x -> [fst x, snd x]) graph

graphIsOrderInvariant :: Ord a => Graph [a] -> Bool
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
leqRlex :: Ord a => [a] -> [a] -> Bool
leqRlex x y = (reverse x) <= (reverse y)

allNaturalTrios :: [(Integer, Integer, Integer)]
allNaturalTrios = allNaturalTrios' 0 0 0
  where
    allNaturalTrios' :: Integer -> Integer -> Integer -> [(Integer, Integer, Integer)]
    allNaturalTrios' a 0 0 = (a, 0, 0) : allNaturalTrios' (a + 1) (a + 1) (a + 1)
    allNaturalTrios' a b 0 = (a, b, 0) : allNaturalTrios' a (b - 1) a
    allNaturalTrios' a b c = (a, b, c) : allNaturalTrios' a b (c - 1)

friedmanCounterexample :: (Integer, Integer, Integer)
friedmanCounterexample = head $ filter checkTrio allNaturalTrios

-- returns True if the trio contains a counterexample
checkTrio :: (Integer, Integer, Integer) -> Bool
-- TODO I am unsure if this works--should I be looping over V somehow?
checkTrio (k, n, r) = any (\edges -> isValidGraph vertices edges k n r) edgeSelections
  where
    numbers = allIntegersWithBoundedComplexity k n r
    --  we generate the nodes in a potential order invariant graph by
    -- adding to N all possible lists of k or fewer numbers from N .
    -- We call this list of lists V .
    vertices = allSubsetsWithSizeAtMostK numbers k
    edgeSelections = allSubsets [(x, y) | x <- vertices, y <- vertices, x > y]

-- returns true if there is a
isValidGraph :: [[Integer]] -> Graph [Integer] -> Integer -> Integer -> Integer -> Bool
isValidGraph vertices edges k n r = not (null subsetsWhichAllReduce)
  where
    lengthRSubsets :: [Set Node]
    lengthRSubsets = allSubsetsWithSize vertices r

    subsetsWithUshes :: [Set Node]
    subsetsWithUshes = filter (\s -> all (\x -> elem (ush x) vertices) s) lengthRSubsets

    subsetsWhichAllReduce :: [Set Node]
    subsetsWhichAllReduce = filter testSetAllReduces subsetsWithUshes

    testSetAllReduces :: Set Node -> Bool
    testSetAllReduces set = and [setReducesOtherSet edges (lhsThing i) (rhsThing i) |
                                  i <- [1..factorial (8 * k * n * r)]]
      where
        -- {x1, . . . , x_{(8kni)}!}
        lhsThing :: Integer -> Set Node
        lhsThing i = genericTake (factorial (8 * k * n * i)) set

        rhsThing :: Integer -> Set Node
        rhsThing i = allSubsetsWithSizeAtMostK (nub (concat (genericTake i set) ++ [0..n])) k


allIntegersWithBoundedComplexity :: Integer -> Integer -> Integer -> Set Integer
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


