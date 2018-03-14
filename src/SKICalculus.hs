module SKICalculus ()
where

data SKITerm = S | K | I | App SKITerm SKITerm
  deriving (Show, Eq)

evalOneStep :: SKITerm -> SKITerm
evalOneStep (App I term) = term
evalOneStep (App (App K term) _) = term
evalOneStep (App (App (App S x) y) z) = App (App x z) (App y z)
evalOneStep (App x y) = App (evalOneStep x) y
evalOneStep other = other

eval :: SKITerm -> SKITerm
eval term = let nextTerm = evalOneStep term in
  if nextTerm == term then term else eval nextTerm

