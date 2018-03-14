module HsToLambdaCalculus (

) where

import GHC
import GHC.Paths (libdir)
import Var (varName)
import Literal (Literal (..))
import Name (nameStableString)
import DataCon (dataConName)
import Outputable (showSDocUnsafe)
import HscTypes (mg_binds)
import Type (pprType)
import CoreSyn
import DynFlags
import Control.Monad ((<=<))
import Data.List (intersperse)

------- This is copied from StackOverflow

compileToCore :: String -> IO [CoreBind]
compileToCore modName = runGhc (Just libdir) $ do
    setSessionDynFlags =<< getSessionDynFlags
    let location = "src/" ++ modName
    target <- guessTarget location Nothing
    setTargets [target]
    load LoadAllTargets
    ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName modName
    return $ mg_binds . coreModule $ ds

----- https://downloads.haskell.org/~ghc/7.6.2/docs/html/libraries/ghc/CoreSyn.html

-- Silly example function that analyzes Core
countCases :: [CoreBind] -> Int
countCases = sum . map countBind
  where
    countBind (NonRec _ e) = 1 + countExpr e
    countBind (Rec bs) = 1 + (sum . map (countExpr . snd) $ bs)

    countExpr (Case e _ _ alts) = 1 + countExpr e + sum (map countAlt alts)
    countExpr (App f e) = 1 + countExpr f + countExpr e
    countExpr (Lam _ e) = 1 + countExpr e
    countExpr (Let b e) = 1 + countBind b + countExpr e
    countExpr (Cast e _) = countExpr e
    countExpr (Tick _ e) = countExpr e
    countExpr _ = 0

    countAlt (_, _, rhs) = 1 + countExpr rhs

showCoreBind :: CoreBind -> String
showCoreBind x = case x of
  (NonRec binding expr) -> showBinding binding ++ " " ++ showCoreExpr expr
  (Rec list) -> "(Rec [" ++ concat (intersperse ", "
    (map (\(b, e) -> showBinding b ++ " " ++ showCoreExpr e) list)) ++ "])"

showCoreExpr :: Expr CoreBndr -> String
showCoreExpr expr = case expr of
  (App f e) -> concat ["(App ", showCoreExpr f, " ", showCoreExpr e, ")"]
  (Case e _ _ alts) -> concat (["(Case ", showCoreExpr e] ++ ["["] ++ (map showAlt alts) ++ ["])"])
  (Let b e) -> concat ["(Let ", showCoreBind b, " ", showCoreExpr e, ")"]
  (Lam var e) -> concat ["(Lam ", nameStableString (varName var), " ", showCoreExpr e, ")"]
  (Cast _ _) -> "Cast"
  (Tick _ _) -> "Tick"
  (Var var) -> nameStableString $ varName var
  (Lit lit) -> showLiteral lit
  (Type t) -> "(Type " ++ (showSDocUnsafe (pprType t)) ++ ")"
  (Coercion c) -> "Coercion"

showAlt :: Alt CoreBndr -> String
showAlt (altCon, vars, expr) = "(Alt " ++ altConStr ++ " " ++
                                          namesStr ++ showCoreExpr expr ++ ")"
  where
    altConStr = case altCon of
      (DataAlt dataCon) -> "(Datacon " ++ nameStableString (dataConName dataCon) ++ ")"
      (LitAlt literal) -> showLiteral literal
      (DEFAULT) -> "DEFAULT"

    namesStr = "[" ++ concat (intersperse ", " (map (nameStableString . varName) vars)) ++ "]"

showLiteral :: Literal -> String
showLiteral lit =  case lit of
  MachInt int -> show int
  MachInt64 int -> show int
  MachWord int -> show int
  MachWord64 int -> show int
  MachLabel _ _ _ -> "machlabel"
  MachNullAddr -> "machNullAddr"
  MachFloat float -> show float
  MachDouble double -> show double
  MachChar char -> [char]
  MachStr str -> "(stringlit " ++ show str ++ ")"
  _ -> error "don't know how to show literal"

showBinding :: CoreBndr -> String
showBinding = nameStableString . varName

main :: IO ()
main = do
   core <- compileToCore "ExampleModule"
   sequence $ intersperse (putStrLn "\b") (map (putStrLn . showCoreBind) core)
   return ()


example = "f x y = x + y + 2"
-- compiles to \x y. (+) x ((+) y 2)

example2 = "sum [] = 0\nsum (x:xs) = x + sum xs"
-- goes to (\f -> f f)
--         (\sum' list -> list 0 (\head tail -> (+) head (sum' tail)))
