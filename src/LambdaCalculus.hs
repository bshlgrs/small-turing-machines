module LambdaCalculus
where

import Data.List (nub, delete)

data LambdaTerm = App LambdaTerm LambdaTerm
                | Lam String LambdaTerm
                | Var String
  deriving (Show, Eq)

names :: LambdaTerm -> [String]
names term = nub (names' term)
  where
    names' term = case term of
      (App lhs rhs) -> (names' lhs) ++ (names' rhs)
      (Lam name body) -> delete name (names' body)
      (Var string) -> [string]

newName :: LambdaTerm -> String
newName term = head $ filter (\x -> not (elem x termNames)) ["var" ++ show i | i <- [0..]]
  where
    termNames = names term

applyMany :: LambdaTerm -> [LambdaTerm] -> LambdaTerm
applyMany f [] = f
applyMany f (x:xs) = applyMany (App f x) xs

abstractMany :: LambdaTerm -> [String] -> LambdaTerm
abstractMany term [] = term
abstractMany term (name:names) = Lam name (abstractMany term names)

size :: LambdaTerm -> Int
size term = case term of
  App l r -> 1 + size l + size r
  Lam _ r -> 1 + size r
  Var _ -> 1
