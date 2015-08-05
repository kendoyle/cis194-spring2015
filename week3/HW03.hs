module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st key value s
    | key == s = value
    | otherwise = st s 

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE st (Val i) = i
evalE st (Op e1 bop e2) = 
    let x = evalE st e1
        y = evalE st e2
        convert = (\f -> if f then 1 else 0)
    in case bop of
        Plus -> x + y
        Minus -> x - y
        Times -> x * y
        Divide -> x `quot` y
        Gt -> convert $ x > y
        Ge -> convert $ x >= y
        Lt -> convert $ x < y
        Le -> convert $ x <= y
        Eql -> convert $ x == y

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e) = DAssign s e
desugar (Incr s) = DAssign s (Op (Val 1) Plus (Var s))
desugar (If e s1 s2) = DIf e (desugar s1) (desugar s2)
desugar (While e s) = DWhile e (desugar s)
desugar (For s1 e s2 s3) = 
    DSequence (desugar s1) (DWhile e (DSequence (desugar s3) (desugar s2))) 
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s e) = extend st s (evalE st e)
evalSimple st (DIf e ds1 ds2) = 
    if (evalE st e) /= 0 then (evalSimple st ds1) else (evalSimple st ds2)
evalSimple st (DWhile e ds) = 
    if (evalE st e) /= 0 then evalSimple (evalSimple st ds) (DWhile e ds) else st
evalSimple st (DSequence ds1 ds2) =
    evalSimple (evalSimple st ds1) ds2
evalSimple st DSkip = st

run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Sequence
                    (Assign "T" (Var "Out"))
                    (Assign "Out" (Op (Var "In") Times (Var "Out"))))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
