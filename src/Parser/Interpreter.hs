{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Parser.Interpreter (runAndEvaluate) where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Parser.Data
import qualified Control.Monad.State.Lazy as Stl
import qualified Data.Map as Dm

runAndEvaluate :: Expression -> IO (Either String LValue)
runAndEvaluate exp =
  let progState = run exp :: ProgramStateMonad LValue in 
  Stl.evalStateT (runExceptT progState) initialEnvironment 

run :: ProgramState ps => Expression -> ps LValue
run (Terminal (LTValue x)) = return x
run (Terminal (LTVar x)) = throw "Unexpected variable name"
run (Sub op subs) = runOp op subs

runOp :: ProgramState ps => Operator -> [Expression] -> ps LValue
runOp Add [e1, e2] = do
  r1 <- run e1
  r2 <- run e2
  case (r1, r2) of
    (LVFloat a, LVFloat b) -> return $ LVFloat (a + b)
    _ -> throw "Type mismatch"

runOp Multiply [e1, e2] = do
  r1 <- run e1
  r2 <- run e2
  case (r1, r2) of
    (LVFloat a, LVFloat b) -> return $ LVFloat (a * b)
    _ -> throw "Type mismatch"

runOp And [e1, e2] = do
  r1 <- run e1
  r2 <- run e2
  case (r1, r2) of
    (LVBool a, LVBool b) -> return $ LVBool (a && b)
    _ -> throw "Type mismatch"
    
runOp Or [e1, e2] = do
  r1 <- run e1
  r2 <- run e2
  case (r1, r2) of
    (LVBool a, LVBool b) -> return $ LVBool (a || b)
    _ -> throw "Type mismatch"
    
runOp If [e1, e2, e3]  = do
  r1 <- run e1
  case r1 of
    (LVBool True) -> run e2
    (LVBool False) -> run e3
    _ -> throw "Type mismatch"

runOp Set [Terminal (LTVar varName), e] = do
  val <- run e
  storeVar varName val
  return (LVBool True)

runOp Get [Terminal (LTVar varName)] = getVar varName

runOp Do [exp] = run exp
runOp Do (x:xs) = do
  run x
  runOp Do xs

runOp Eq [e1, e2] = do
  r1 <- run e1
  r2 <- run e2
  return $ LVBool (r1 == r2)

runOp Neq [e1, e2] = do
  r1 <- run e1
  r2 <- run e2
  return $ LVBool (r1 /= r2)

runOp While [cond, exp] = do
  result <- run cond  
  case result of
    (LVBool True) -> do
      run exp
      runOp While [cond, exp]
    (LVBool False) -> return $ LVBool False
    _ -> throw "Type mismatch" 

runOp Print [exp] = do
  result <- run exp
  ps_print $ show result
  return (LVBool True)

runOp Input [] = LVString <$> input

runOp op _ = throw "Type mismatch or incorrect argument count"

-- Types

newtype Environment = Environment {
  environmentVariables :: Dm.Map String LValue
}

initialEnvironment :: Environment
initialEnvironment = Environment { environmentVariables = Dm.empty}

type ProgramStateMonad = (ExceptT String (Stl.StateT Environment IO))

class Monad a => ProgramState a where
  storeVar :: String -> LValue -> a ()
  getVar :: String -> a LValue
  throw :: forall b. String -> a b
  ps_print :: String -> a ()
  input :: a String

instance ProgramState ProgramStateMonad where
  storeVar varName val = Stl.lift $ Stl.modify (setVariable varName val)
    where    
      setVariable name value (Environment vars) =
        Environment { environmentVariables = Dm.insert name value vars }

  getVar varName = do 
    env <- Stl.lift Stl.get
    case Dm.lookup varName (environmentVariables env) of
      Just result -> Stl.lift $ return result
      Nothing -> throwE $ "No variable stored: " ++ varName
  throw = throwE
  ps_print s = Stl.liftIO $ putStrLn s
  input = Stl.liftIO getLine