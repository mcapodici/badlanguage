{-# LANGUAGE OverloadedStrings #-}  

module Parser.Parser (program) where
    
import Parser.FloatParser
import Text.Parsec.Text
import Text.Parsec.Prim (try)
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import Parser.Data
import Control.Applicative

program :: Parser Expression
program = program' <* eof

program' :: Parser Expression
program' =
  (try list) <|> atom
  where
    list :: Parser Expression
    list = do
      char '('
      op <- opParser
      spaces
      subs <- sepBy program' spaces
      spaces
      char ')'
      spaces
      return $ Sub op subs

    atom :: Parser Expression
    atom = Terminal <$> valParser

    opParser :: Parser Operator
    opParser =
      choice $ Prelude.map try
        [ string "+" >> return Add
        , string "*" >> return Multiply
        , string "and" >> return And
        , string "or" >> return Or
        , string "if" >> return If
        , string "set" >> return Set
        , string "get" >> return Get
        , string "do" >> return Do
        , string "eq" >> return Eq
        , string "neq" >> return Neq
        , string "while" >> return While
        , string "print" >> return Print
        , string "input" >> return Input
        ]

    valParser :: Parser LTerminal
    valParser =
      choice $ Prelude.map try
      [ string "true" >> return (asBool True)
      , string "false" >> return (asBool False)
      , asFloat <$> float
      , asString <$> (char '"' *> many(noneOf "\"") <* char '"')
      , LTVar <$> many1 alphaNum 
      ]
      where
        asBool = LTValue . LVBool
        asFloat = LTValue . LVFloat 
        asString = LTValue . LVString 


