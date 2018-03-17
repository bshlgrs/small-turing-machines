module HsToLambdaCalculus where

import GHC (runGhc, mkModuleName, getModSummary, parseModule, typecheckModule,
  desugarModule, coreModule, DataCon, TyCon, dataConTyCon, tyConDataCons,
  LoadHowMuch(LoadAllTargets), load, setSessionDynFlags, getSessionDynFlags,
  setTargets, guessTarget, dm_core_module)
import GHC.Paths (libdir)
import Var (varName, Var)
import Literal (Literal (..))
import Name (nameOccName, occNameString, isTvOcc, nameStableString)
import DataCon (dataConName, dataConSourceArity)
import Outputable (showSDocUnsafe, ppr)
import HscTypes (mg_binds, mg_tcs)
import Type (pprType)
import TyCon (tyConName)
import CoreSyn
import DynFlags
import Control.Monad ((<=<))
import Data.List (intersperse, find, isInfixOf, nub)
import Data.Maybe (fromMaybe)
import qualified LambdaCalculus as L
import Debug.Trace (traceShowId)

------- This is copied from StackOverflow

compileToCore :: String -> IO ([CoreBind], [TyCon])
compileToCore modName = runGhc (Just libdir) $ do
    setSessionDynFlags =<< getSessionDynFlags
    let location = "src/" ++ modName
    target <- guessTarget location Nothing
    setTargets [target]
    load LoadAllTargets
    ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName modName
    return (mg_binds (coreModule ds), mg_tcs (dm_core_module ds))

----- https://downloads.haskell.org/~ghc/7.6.2/docs/html/libraries/ghc/CoreSyn.html

showCoreBind :: CoreBind -> String
showCoreBind x = case x of
  (NonRec binding expr) -> varToStr binding ++ " := " ++ showCoreExpr expr
  (Rec list) -> "(Rec [" ++ concat (intersperse ", "
    (map (\(b, e) -> varToStr b ++ " := " ++ showCoreExpr e) list)) ++ "])"


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


main :: IO ()
main = do
   (core, tyCons) <- compileToCore "ExampleModule"
   -- showCoreBind
   sequence $ map (print . compileTopLevelBinding) core
   print (map (\list -> map (\(name, x) -> (name, L.names x)) list) (map compileTopLevelBinding core))
   print (map makeDataConstructorTermsFromTyCon tyCons)
   return ()

makeDataConstructorTermsFromTyCon :: TyCon -> [(String, L.LambdaTerm)]
makeDataConstructorTermsFromTyCon tyCon = map compileDataCon dataCons
  where
    dataCons :: [DataCon]
    dataCons = tyConDataCons tyCon

    compileDataCon :: DataCon -> (String, L.LambdaTerm)
    compileDataCon dataCon = (dataConNameString dataCon, term)
        where
          term :: L.LambdaTerm
          term = L.abstractMany
            (callbackWrapper
              (L.applyMany (L.Var ("$$if" ++ myName)) (map L.Var myVars)))
              myVars

          myVars :: [String]
          myVars = ["$$var" ++ show i | i <- [1..dataConSourceArity dataCon]]

          myName :: String
          myName = dataConNameString dataCon

    callbackWrapper :: L.LambdaTerm -> L.LambdaTerm
    callbackWrapper t = L.abstractMany t ["$$if" ++ dataConNameString d | d <- dataCons]

dataConNameString :: DataCon -> String
dataConNameString = nameStableString . dataConName -- occNameString . nameOccName . dataConName

compileModuleToLambdaTermIO :: IO ()
compileModuleToLambdaTermIO = do
  (core, tyCons) <- compileToCore "ExampleModule"
  putStrLn ("main = " ++ (L.lcString $ compileModuleToLambdaTerm core tyCons "$main$ExampleModule$mainVal"))
  putStrLn ("main = " ++ (L.lcString $ L.renameAll $ compileModuleToLambdaTerm core tyCons "$main$ExampleModule$mainVal"))
  putStrLn "main"

type Group = [(String, L.LambdaTerm)]

compileModuleToLambdaTerm :: [CoreBind] -> [TyCon] -> String -> L.LambdaTerm
compileModuleToLambdaTerm bindings tyCons nameOfMainFn = reduceToSingleTerm orderedLambdaTermGroups
  where
    ------------- TODO: also, find all data constructors referred to, and build their definitions
    compiledBindings :: [Group]
    compiledBindings = map compileTopLevelBinding bindings
    compiledDataConstructors :: [Group]
    compiledDataConstructors =
          map (\x -> [x]) (concatMap makeDataConstructorTermsFromTyCon tyCons)

    orderedLambdaTermGroups =
      topoSortGroups (compiledBindings ++ compiledDataConstructors) nameOfMainFn

allPatternMatchedTypesInModule :: [CoreBind] -> [TyCon]
allPatternMatchedTypesInModule binds = nub $ concatMap allPatternMatchedTypes binds
  where
    allPatternMatchedTypes :: CoreBind -> [TyCon]
    allPatternMatchedTypes = error "TODO"

topoSortGroups :: [Group] -> String -> [Group]
topoSortGroups groups mainFn = reverse (topohelper [] (children mainFnGroup) mainFnGroup)
  where
    mainFnGroup = groupForFunction mainFn

    definedFunctions :: Group -> [String]
    definedFunctions list = map fst list

    usedFunctions :: Group -> [String]
    usedFunctions list = concatMap (L.names . snd) list

    groupForFunction :: String -> Group
    groupForFunction fName = fromMaybe (error ("Function " ++ fName ++ " wasn't found"))
                              $ find (\x -> fName `elem` definedFunctions x) groups

    topohelper :: [Group] -> [Group] -> Group -> [Group]
    topohelper explored [] goal = goal : explored
    topohelper explored (x:xs) goal
      | x `elem` explored = topohelper explored xs goal
      | otherwise         = topohelper (topohelper explored (children x) x) xs goal

    children :: Group -> [Group]
    children group = nub $ [groupForFunction usedFunction |
                              usedFunction <- usedFunctions group]

reduceToSingleTerm :: [[(String, L.LambdaTerm)]] -> L.LambdaTerm
reduceToSingleTerm [] = error "can't reduce empty list"
reduceToSingleTerm [[(name, term)]] = term
reduceToSingleTerm ([(name, term)]: others) =
                            L.App (L.Lam name (reduceToSingleTerm others)) term
reduceToSingleTerm (_: others) = error "can't handle mutually recursive functions yet"

printCore :: IO ()
printCore = do
   (core, _) <- compileToCore "ExampleModule"
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
                        -- this is the Y combinator
                        (L.Lam varFName
                          (L.App
                            (L.Lam varXName (L.App varF (L.App varX varX)))
                            (L.Lam varXName (L.App varF (L.App varX varX)))
                          )
                        )
                        (L.Lam name exprBody)
      varFName = L.newName exprBody
      varF = L.Var varFName
      varXName = L.newName (L.App exprBody varF)
      varX = L.Var varXName
  (Rec multipleBindings) -> error "don't know how to handle mutual recursion"

compileBinding :: CoreBind -> [(String, L.LambdaTerm)]
compileBinding coreBind = case coreBind of
  (NonRec bindingVar e) -> [(varToStr bindingVar, compileCoreExpr e)]
  _ -> compileTopLevelBinding coreBind

showCoreExpr :: Expr CoreBndr -> String
showCoreExpr expr = case expr of
  (App f e) -> concat ["(App ", showCoreExpr f, " ", showCoreExpr e, ")"]
  (Case e name exprType alts) ->
    concat (
      ["(Case ", showCoreExpr e, " ", varToStr name, " ",
      showSDocUnsafe (pprType exprType), " "] ++
      ["["] ++ (map showAlt alts) ++ ["])"])
  (Let b e) -> concat ["(Let ", showCoreBind b, " ", showCoreExpr e, ")"]
  (Lam var e) -> concat ["(Lam ", varToStr var, " ", showCoreExpr e, ")"]
  (Cast _ _) -> "Cast"
  (Tick _ _) -> "Tick"
  (Var var) -> varToStr var
  (Lit lit) -> "(Lit " ++ showLiteral lit ++ ")"
  (Type t) -> "(Type " ++ (showSDocUnsafe (pprType t)) ++ ")"
  (Coercion c) -> "Coercion"

compileCoreExpr :: Expr CoreBndr -> L.LambdaTerm
compileCoreExpr expr = case expr of
  (App f (Type t)) -> compileCoreExpr f
  (App f e) -> L.App (compileCoreExpr f) (compileCoreExpr e)
  (Lam var e)
    | isTvOcc (nameOccName (varName var)) -> compileCoreExpr e
    | otherwise -> L.Lam (varToStr var) (compileCoreExpr e)
  (Var var)
    | varToStr var == "$ghc-prim$GHC.Prim$void#" -> L.Lam "void" (L.Var "void")
    | varToStr var == "$_sys$GHC.Prim$void#" -> L.Lam "void" (L.Var "void")
    | otherwise -> L.Var (varToStr var)
  (Let binding e) -> case compileBinding binding of
    -- [("$_sys$fail", lambdaTerm)] -> compileCoreExpr e
    [(name, lambdaTerm)] -> L.App (L.Lam name (compileCoreExpr e)) lambdaTerm
    _ -> error "can't compile more complicated let expressions"
  (Case e _ _ alts) -> compileAlts (compileCoreExpr e) alts
  _ -> error ("new thing: " ++ showCoreExpr expr)

varToStr :: Var -> String
varToStr var = nameStableString name --occNameString (nameOccName name)
  where
    name = varName var

showAlt :: Alt CoreBndr -> String
showAlt (altCon, vars, expr) = "(Alt " ++ altConStr ++ " " ++
                                          namesStr ++ " " ++ showCoreExpr expr ++ ")"
  where
    altConStr = case altCon of
      (DataAlt dataCon) -> "(Datacon " ++ dataConNameString dataCon ++ ")"
      (LitAlt literal) -> showLiteral literal
      (DEFAULT) -> "DEFAULT"

    namesStr = "[" ++ concat (intersperse ", " (map varToStr vars)) ++ "]"


compileAlts :: L.LambdaTerm -> [Alt CoreBndr] -> L.LambdaTerm
compileAlts inExpr cases = case cases of
  (DataAlt dataCon, _, _):_ -> compileDataAlts (tyConDataCons (dataConTyCon dataCon)) Nothing inExpr cases
  (DEFAULT, _, defaultExpr):(DataAlt dataCon, _, _):_ ->
    compileDataAlts (tyConDataCons (dataConTyCon dataCon)) (Just defaultExpr) inExpr (tail cases)
  (LitAlt _, _, _):_ -> error "I don't know how to handle LitAlts"
  (DEFAULT, _, _):(LitAlt _, _, _):_ -> error "I don't know how to handle LitAlts"


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
