{-# LANGUAGE OverloadedStrings #-}  
{-# LANGUAGE TemplateHaskell #-}

module Parser.CompileToJs (compile) where

import Parser.Data
import Data.Text as T
import Data.FileEmbed
import Data.List as L

compile :: Expression -> Either String T.Text
compile ex = do
    result <- expCompile ex
    return $ T.replace "{placeholder}" result preamble

preamble :: T.Text
preamble = $(embedFile "src/Parser/preamble.js")

expCompile :: Expression -> Either String T.Text
expCompile (Terminal t) = Right (compileTerminal t)
expCompile (Sub op exps) = compileSubexpression op exps

compileTerminal :: LTerminal -> T.Text
compileTerminal (LTValue (LVBool True)) = "true"
compileTerminal (LTValue (LVBool False)) = "false"
compileTerminal (LTValue (LVFloat v)) = T.pack $ show v
compileTerminal (LTValue (LVString s)) =  T.concat ["'", escapeJsString (pack s), "'"]
compileTerminal (LTVar (s)) = T.pack s

escapeJsString :: Text -> Text
escapeJsString = 
    T.replace "'" "\\\'" .
    T.replace "\\" "\\\\"

compileAsFunctionLookup :: Operator -> Maybe (Maybe Int, Text)
compileAsFunctionLookup Add = Just (Just 2, "add")
compileAsFunctionLookup Multiply = Just (Just 2, "mul")
compileAsFunctionLookup And = Just (Just 2, "and")
compileAsFunctionLookup Or = Just (Just 2, "or")
compileAsFunctionLookup If = Just (Just 3, "_if")
compileAsFunctionLookup Do = Just (Nothing, "_do")
compileAsFunctionLookup Eq = Just (Just 2, "eq")
compileAsFunctionLookup Neq = Just (Just 2, "neq")
compileAsFunctionLookup While = Just (Just 2, "_while")
compileAsFunctionLookup Print = Just (Just 1, "print")
compileAsFunctionLookup Input = Just (Just 0, "input")
compileAsFunctionLookup _ = Nothing

compileSubexpression :: Operator -> [Expression] -> Either String T.Text
compileSubexpression op exps =
    case compileAsFunctionLookup op of
        Just (expectedArgCountMaybe, name) -> 
            if (expectedArgCountMaybe == Nothing) || expectedArgCountMaybe == Just (L.length exps)
                then delegateToJsFunction name exps
                else Left "unexpected number of arguments"
        _ -> 
            case (op, exps) of
                (Set, [Terminal (LTVar varName), val]) -> delegateToJsFunction "set" [Terminal (LTValue (LVString varName)), val]
                (Get, [Terminal (LTVar varName)]) -> delegateToJsFunction "get" [Terminal (LTValue (LVString varName))]
                _ -> Left "error"

delegateToJsFunction :: Text -> [Expression] -> Either String T.Text
delegateToJsFunction jsFnName exps = do
    compilations <- mapM expCompile exps
    return $ T.concat $ ["(function(){return ", jsFnName, "("] ++ L.intersperse "," compilations ++ [");})"]
