module Lib
    ( someFunc
    ) where

someFunc :: String
someFunc = "foooooo"

type Name = String
data LaconicType = IntType | ListType | List2Type
  deriving (Show, Eq)

data Stmt = VariableDeclaration LaconicType Name
          | ExprStmt Expr
          | WhileStmt Expr [Stmt]
          | IfStmt Expr [Stmt] [Stmt]
          | ReturnStmt
          | HaltStmt
          | AssignmentStmt Name Expr
  deriving (Show, Eq)

data Expr = BinOpExpr BinOp Expr Expr
          | UnaryOpExpr UnaryOp Expr
          | ConstIntExpr Int
          | ConstListExpr [Expr]
          | ConstList2Expr [[Expr]]
  deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition [Name] [Stmt]
  deriving (Show, Eq)

data LaconicProgram = LaconicProgram [FunctionDefinition]
  deriving (Show, Eq)

data BinOp = Plus | Minus
  deriving (Show, Eq)

data UnaryOp = UnaryMinus
  deriving (Show, Eq)
