module Parser.FloatParser (float) where

import Control.Monad
import Text.Parsec.Text
import Text.Parsec
import Control.Applicative hiding ((<|>))

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

integer = plus <|> minus <|> number

float :: Parser Float
float = fmap rd $ integer <++> decimal <++> exponent
    where
        rd = read :: String -> Float
        decimal  = option "" $ char '.' <:> number
        exponent = option "" $ oneOf "eE" <:> integer