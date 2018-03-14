module HsToLambdaCalculus (

) where

import GHC
import GHC.Paths (libdir)
import HscTypes (mg_binds)
import CoreSyn
import DynFlags
import Control.Monad ((<=<))

------- This is copied from StackOverflow

compileToCore :: String -> String -> IO [CoreBind]
compileToCore modName location = runGhc (Just libdir) $ do
    setSessionDynFlags =<< getSessionDynFlags
    target <- guessTarget location Nothing
    setTargets [target]
    load LoadAllTargets
    ds <- desugarModule <=< typecheckModule <=< parseModule <=< getModSummary $ mkModuleName modName
    return $ mg_binds . coreModule $ ds

-- Silly example function that analyzes Core
countCases :: [CoreBind] -> Int
countCases = sum . map countBind
  where
    countBind (NonRec _ e) = countExpr e
    countBind (Rec bs) = sum . map (countExpr . snd) $ bs

    countExpr (Case e _ _ alts) = countExpr e + sum (map countAlt alts)
    countExpr (App f e) = countExpr f + countExpr e
    countExpr (Lam _ e) = countExpr e
    countExpr (Let b e) = countBind b + countExpr e
    countExpr (Cast e _) = countExpr e
    countExpr (Tick _ e) = countExpr e
    countExpr _ = 0

    countAlt (_, _, rhs) = 1 + countExpr rhs

main :: IO ()
main = do
   core <- compileToCore "Friedman" "src/Friedman"
   print $ countCases core


example = "f x y = x + y + 2"
-- compiles to \x y. (+) x ((+) y 2)

example2 = "sum [] = 0\nsum (x:xs) = x + sum xs"
-- goes to (\f -> f f)
--         (\sum' list -> list 0 (\head tail -> (+) head (sum' tail)))
