module AST where

-- Identificadores de Variable
type Variable = String

-- Expresiones Aritméticas (Enteras y Flotantes)
data Exp = Const Integer
         | DoubleConst Double
         | Var Variable
         | UMinus Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Question BoolExp Exp Exp
         | Sin Exp
         | Cos Exp
         | Tan Exp
         | Ceil Exp
         | Floor Exp
         | Round Exp Exp
         | Pi
    deriving (Show, Eq)

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq Exp Exp
             | Lt Exp Exp
             | Gt Exp Exp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
    deriving (Show, Eq)

-- Comandos (Sentencias)
data Comm = Skip
          | Let Variable Exp
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat Comm BoolExp
          | Input Variable
          | Print (Maybe String) Exp
          | Tic Exp
    deriving (Show, Eq)
