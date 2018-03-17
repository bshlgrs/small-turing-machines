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
      (Lam name body) -> filter (/= name) (names' body)
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

------- TODO: implement conversion to SKI calculus
-- eg https://gist.github.com/jozefg/dc5f40c76c94969f9619

lcString :: LambdaTerm -> String
lcString term = case term of
  App l r -> lcString l ++ " (" ++ lcString r ++ ")"
  Lam n r -> "(\\" ++ n ++ " -> " ++ lcString r ++ ")"
  Var n -> n

renameAll :: LambdaTerm -> LambdaTerm
renameAll t = snd (renameAllFromNum 0 t)
  where
    renameAllFromNum :: Int -> LambdaTerm -> (Int, LambdaTerm)
    renameAllFromNum n t = case t of
      Var name -> (n, Var name)
      Lam currentName body ->
        let
          newName = ("v" ++ show n)
          firstRenamed = replaceName currentName newName body
          (newNum, fullyRenamed) = renameAllFromNum (n + 1) firstRenamed
        in
          (newNum, Lam newName fullyRenamed)
      App l r ->
        let
          (newN, newL) = renameAllFromNum n l
          (newestN, newR) = renameAllFromNum newN r
        in
          (newestN, (App newL newR))


replaceName :: String -> String -> LambdaTerm -> LambdaTerm
replaceName old new t = let replace = replaceName old new in case t of
  Var v
    | v == old -> Var new
    | otherwise -> Var v
  App x y -> App (replace x) (replace y)
  Lam n y
    | n == old -> Lam n y
    | otherwise -> Lam n (replace y)

