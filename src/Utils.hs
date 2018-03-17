module Utils where

indentBrackets :: String -> String
indentBrackets = ib 0
  where
    ib :: Int -> String -> String
    ib n (x:xs) = case x of
      '(' -> '\n' : indent n ++ "(" ++ ib (n + 1) xs
      '[' -> '\n' : indent n ++ "[" ++ ib (n + 1) xs
      ')' -> ")\n" ++ indent (n - 2) ++ ib (n - 1) xs
      ']' -> "]\n" ++ indent (n - 2) ++ ib (n - 1) xs
      _ -> x : ib n xs
    ib _ "" = ""

    indent :: Int -> String
    indent n = concat $ take n (repeat "  ")
