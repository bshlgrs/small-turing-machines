module HsToLambdaCalculus where

import GHC
import GHC.Paths (libdir)
import Var (varName, Var)
import Literal (Literal (..))
import Name (nameStableString)
import DataCon (dataConName)
import Outputable (showSDocUnsafe, ppr)
import HscTypes (mg_binds)
import Type (pprType)
import TyCon (tyConName)
import CoreSyn
import DynFlags
import Control.Monad ((<=<))
import Data.List (intersperse, find, isInfixOf)
import qualified LambdaCalculus as L

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

showCoreBind :: CoreBind -> String
showCoreBind x = case x of
  (NonRec binding expr) -> showBinding binding ++ " := " ++ showCoreExpr expr
  (Rec list) -> "(Rec [" ++ concat (intersperse ", "
    (map (\(b, e) -> showBinding b ++ " := " ++ showCoreExpr e) list)) ++ "])"


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
  _ -> showSDocUnsafe (ppr lit)

showBinding :: CoreBndr -> String
showBinding = nameStableString . varName

main :: IO ()
main = do
   core <- compileToCore "Friedman"
   -- showCoreBind
   sequence $ map (print . compileTopLevelBinding) core
   print (map (\(_, x) -> L.size x) (concatMap compileTopLevelBinding core))
   return ()

compileModuleToLambdaTermIO :: IO ()
compileModuleToLambdaTermIO = do
  core <- compileToCore "ExampleModule"
  print (compileModuleToLambdaTerm core "mainComputation")

compileModuleToLambdaTerm :: [CoreBind] -> String -> L.LambdaTerm
compileModuleToLambdaTerm bindings nameOfMainComputation = reduceToSingleTerm orderedLambdaTermGroups
  where
    compiledBindings = map compileTopLevelBinding bindings
    orderedLambdaTermGroups = topoSortGroups compiledBindings nameOfMainComputation

topoSortGroups :: [[(String, L.LambdaTerm)]] -> String -> [[(String, L.LambdaTerm)]]
topoSortGroups groups mainComputation = error "TODO"
{-
This is just a DFS.

what if the main function is mutually recursive???
-}

reduceToSingleTerm :: [[(String, L.LambdaTerm)]] -> L.LambdaTerm
reduceToSingleTerm groups = error "TODO"

printCore :: IO ()
printCore = do
   core <- compileToCore "Friedman"
   sequence $ map (putStrLn . showCoreBind) core
   return ()

compileTopLevelBinding :: CoreBind -> [(String, L.LambdaTerm)]
compileTopLevelBinding coreBind = case coreBind of
  (NonRec binding e) -> let name = varToStr binding in
    if ("$$" `isInfixOf` name)
    then []
    else [(name, cleanUpNonsense (compileCoreExpr e))]
  (Rec [(binding, expr)]) -> [(name, compiledExpr)]
    where
      name = varToStr binding
      exprBody = cleanUpNonsense (compileCoreExpr expr)
      compiledExpr = L.App
                       (L.Lam dummyVar (L.App (L.Var dummyVar) (L.Var dummyVar)))
                       (L.Lam name exprBody)
      dummyVar = L.newName exprBody
  (Rec multipleBindings) -> error "don't know how to handle mutual recursion"

compileBinding :: CoreBind -> [(String, L.LambdaTerm)]
compileBinding coreBind = case coreBind of
  (NonRec binding e) -> [(nameStableString (varName binding), compileCoreExpr e)]
  _ -> undefined

showCoreExpr :: Expr CoreBndr -> String
showCoreExpr expr = case expr of
  (App f e) -> concat ["(App ", showCoreExpr f, " ", showCoreExpr e, ")"]
  (Case e name exprType alts) ->
    concat (
      ["(Case ", showCoreExpr e, " ", varToStr name, " ",
      showSDocUnsafe (pprType exprType), " "] ++
      ["["] ++ (map showAlt alts) ++ ["])"])
  (Let b e) -> concat ["(Let ", showCoreBind b, " ", showCoreExpr e, ")"]
  (Lam var e) -> concat ["(Lam ", nameStableString (varName var), " ", showCoreExpr e, ")"]
  (Cast _ _) -> "Cast"
  (Tick _ _) -> "Tick"
  (Var var) -> varToStr var
  (Lit lit) -> "(Lit " ++ showLiteral lit ++ ")"
  (Type t) -> "(Type " ++ (showSDocUnsafe (pprType t)) ++ ")"
  (Coercion c) -> "Coercion"

compileCoreExpr :: Expr CoreBndr -> L.LambdaTerm
compileCoreExpr expr = case expr of
  (App f e) -> L.App (compileCoreExpr f) (compileCoreExpr e)
  (Lam var e) -> L.Lam (varToStr var) (compileCoreExpr e)
  (Var var) -> L.Var (varToStr var)
  (Let binding e) -> case compileBinding binding of
    [("$_sys$fail", lambdaTerm)] -> compileCoreExpr e
    [(name, lambdaTerm)] -> L.App (L.Lam name (compileCoreExpr e)) lambdaTerm
    _ -> undefined
  (Case e _ _ alts) -> compileAlts (compileCoreExpr e) alts
  _ -> error ("new thing: " ++ showCoreExpr expr)

varToStr :: Var -> String
varToStr = nameStableString . varName

showAlt :: Alt CoreBndr -> String
showAlt (altCon, vars, expr) = "(Alt " ++ altConStr ++ " " ++
                                          namesStr ++ " " ++ showCoreExpr expr ++ ")"
  where
    altConStr = case altCon of
      (DataAlt dataCon) -> "(Datacon " ++ nameStableString (dataConName dataCon)
                            ++ " ----" ++ typeConName ++ " WOW " ++ otherDataConsStr ++ ")"
        where
          typeConName = nameStableString (tyConName tyCon)
          tyCon :: TyCon
          tyCon = dataConTyCon dataCon
          otherDataCons :: [DataCon]
          otherDataCons = tyConDataCons tyCon
          otherDataConsStr = concat (intersperse ", " (map (nameStableString . dataConName) otherDataCons))
      (LitAlt literal) -> showLiteral literal
      (DEFAULT) -> "DEFAULT"

    namesStr = "[" ++ concat (intersperse ", " (map (nameStableString . varName) vars)) ++ "]"


compileAlts :: L.LambdaTerm -> [Alt CoreBndr] -> L.LambdaTerm
compileAlts inExpr cases = case cases of
  (DataAlt dataCon, _, _):_ -> compileDataAlts (tyConDataCons (dataConTyCon dataCon)) Nothing inExpr cases
  (DEFAULT, _, defaultExpr):(DataAlt dataCon, _, _):_ ->
    compileDataAlts (tyConDataCons (dataConTyCon dataCon)) (Just defaultExpr) inExpr (tail cases)
  (LitAlt _, _, _):_ -> error "I don't know how to handle LitAlts"
  (DEFAULT, _, _):(LitAlt _, _, _):_ -> error "I don't know how to handle LitAlts"
  _ -> undefined


compileDataAlts :: [DataCon] -> Maybe CoreExpr -> L.LambdaTerm -> [Alt CoreBndr] -> L.LambdaTerm
compileDataAlts dataCons mbDefault inExpr cases =
  L.applyMany inExpr (orderedCaseExprs)
    where
      orderedCaseExprs :: [L.LambdaTerm]
      orderedCaseExprs = [getExprToUse dataCon | dataCon <- dataCons]

      getExprToUse :: DataCon -> L.LambdaTerm
      getExprToUse dataCon = case find (\(alt, _, _) -> alt == DataAlt dataCon) cases of
        Just (_, varNames, resultExpr) -> L.abstractMany (compileCoreExpr resultExpr) (map varToStr varNames)
        Nothing -> case mbDefault of
          Just (defaultExpr) -> compileCoreExpr defaultExpr
          Nothing -> error "You didn't have all possible cases filled in, and you didn't have a default case :("
        -- look for this dataCon in cases. If you don't find it, try to use the mbDefault thing.


{-
Todo:

- In the case where there's literal expressions being used, compile those to a
  series of if expressions
-}

cleanUpNonsense :: L.LambdaTerm -> L.LambdaTerm
cleanUpNonsense term = case term of
  (L.App x (L.App (L.Var "$_sys$fail") (L.Var "$ghc-prim$GHC.Prim$void#"))) -> cleanUpNonsense x
  (L.App x y) -> L.App (cleanUpNonsense x) (cleanUpNonsense y)
  (L.Lam name term2) -> L.Lam name (cleanUpNonsense term2)
  (L.Var _) -> term
