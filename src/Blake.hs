module Blake
where


part :: Int -> Int
part x = (sumpart x 1) - 1

sumpart :: Int -> Int -> Int
sumpart x n = case x > 0 of
  False -> 0
  True -> case n == x of
    True -> 1
    False -> cont + curr where
      cont = sumpart (x - n) n
      curr = sumpart x (n + 1)
