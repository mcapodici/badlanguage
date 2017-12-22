module Parser.Data where

import Data.Text (Text)

data Operator
    = Add
    | Multiply
    | And
    | Or
    | If
    | Set
    | Get
    | Do
    | Eq
    | Neq
    | While
    | Print
    | Input
    deriving Show
  
data Expression
  = Sub Operator [Expression]
  | Terminal LTerminal
  deriving Show

data LTerminal
  = LTValue LValue
  | LTVar String

data LValue 
  = LVBool Bool
  | LVFloat Float
  | LVString String
  deriving Eq
  
instance Show LValue where
  show (LVBool v) = show v
  show (LVFloat v) = show v
  show (LVString v) = show v

instance Show LTerminal where
  show (LTValue v) = show v
  show (LTVar v) = v
