module InterpretingSki
where

getTerm :: String -> Maybe (String, String)
getTerm "" = Nothing
getTerm stuff = Just (getTerm' 1 stuff)
  where
    getTerm' :: Int -> String -> (String, String)
    getTerm' 0 stuff = ("", stuff)
    getTerm' n ('(':xs) = let (restOfTerm, end) = getTerm' (n+1) xs
                          in ('(':restOfTerm, end)
    getTerm' n (x:xs) = let (restOfTerm, end) = getTerm' (n-1) xs
                          in (x:restOfTerm, end)

interpret :: String -> String
interpret "" = ""
interpret input@(x:xs) = case x of
  '(' -> xs
  'I' -> case getTerm xs of
    Nothing -> "I"
    Just (next, rest) -> next ++ rest
  'K' -> case getTerm xs of
    Nothing -> input
    Just (first, notFirst) -> case getTerm notFirst of
      Nothing -> input
      Just (second, notSecond) -> first ++ notSecond
  'S' -> case getTerm xs of
    Nothing -> input
    Just (first, notFirst) -> case getTerm notFirst of
      Nothing -> input
      Just (second, notSecond) -> case getTerm notSecond of
        Nothing -> input
        Just (third, notThird) ->
          '(': first ++ third ++ "(" ++ second ++ third ++ notThird
  _ -> input

recursivelyInterpret :: String -> [String]
recursivelyInterpret x = if next == x then [x] else x:recursivelyInterpret next
  where
    next = interpret x

data Tape = Tape [TapeSymbol] TapeSymbol [TapeSymbol]
instance Show Tape where
  show (Tape l x r) = concat (map show (reverse l)) ++ "|" ++ show x ++ "|" ++ concatMap show r

data TapeSymbol = A | B | X
  deriving Show

data Direction = L | R



blankTape = Tape [] X []

curr :: Tape -> TapeSymbol
curr (Tape _ x _) = x

moveLeft :: Tape -> Tape
moveLeft (Tape [] x right) = Tape [] X (x:right)
moveLeft (Tape (l:ls) x right) = Tape ls l (x:right)

moveRight :: Tape -> Tape
moveRight (Tape left x []) = Tape (x:left) X []
moveRight (Tape left x (r:rs)) = Tape (x:left) r rs

move :: Direction -> Tape -> Tape
move L = moveLeft
move R = moveRight

toList :: Tape -> [TapeSymbol]
toList (Tape left x right) = reverse left ++ [x] ++ right



type TuringMachineTransition a = a -> TapeSymbol -> (Bool, Direction, TapeSymbol, a)

transition :: Tape -> a -> TuringMachineTrans-> (Bool, Tape, a)
transition (Tape left x right) s f = let (finished, dir, newSym, newState) = f s x in
  (finished, move dir (Tape left newSym right), newState)

runTM :: Tape -> a -> TuringMachineTransition a -> [Tape]
runTM tape s f = let (finished, newTape, newState) = transition tape s f in
               if finished then [tape] else tape:(runTM newTape newState f)

data InverterTmState = S1 | S2
  deriving Show

inverter :: TuringMachineTransition InverterTmState
inverter _ A = (False, R, B, S1)
inverter _ B = (False, R, A, S1)
inverter _ X = (True, R, X, S1)

data CopierTmState = AboutToStartShifting | CarryingA | CarryingB | Returning | Done
  deriving Show

-- copier :: TuringMachineTransition CopierTmSt
