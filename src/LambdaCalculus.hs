module LambdaCalculus
where

data LambdaTerm = LApp LambdaTerm LambdaTerm
                | LAbs String LambdaTerm
                | LVar String
  deriving (Show, Eq)
