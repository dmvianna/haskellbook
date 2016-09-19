{-# LANGUAGE FlexibleContexts #-}
-- from the tutorial by
-- Martin Grabmuller
-- https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf

module Transformers where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map

type Name = String -- variable names

data Exp = Lit Integer -- expressions
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)
                  
data Value = IntVal Integer -- values
           | FunVal Env Name Exp
           deriving (Show)

type Env = Map.Map Name Value -- mapping from names to values

-- Without Monad Transformers

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of
                          FunVal env' n body ->
                            eval0 (Map.insert n val2 env') body

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- eval0 Map.empty exampleExp

-- With Monad Transformers

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                             FunVal env' n body ->
                               eval1 (Map.insert n val2 env') body

-- runEval1 (eval1 Map.empty exampleExp)

-- Adding Error Handling

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = case Map.lookup n env of
  Just val -> return val
  Nothing -> throwError ("unbound variable: " ++ n)
eval2a env (Plus e1 e2) = do e1' <- eval2a env e1
                             e2' <- eval2a env e2
                             case (e1', e2') of
                               (IntVal i1, IntVal i2) ->
                                 return $ IntVal (i1 + i2)
                               _ -> throwError "type error in addition"
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                              FunVal env' n body ->
                                eval2a (Map.insert n val2 env') body
                              _ -> throwError "type error in application"

-- runEval2 (eval2a Map.empty exampleExp)
-- runEval2 (eval2a Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))

-- Hiding the Environment

type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runExceptT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                     Just val -> return val
                     Nothing -> throwError ("unbound variable: " ++ n)
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) ->
                            return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return $ FunVal env n e
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                         FunVal env' n body ->
                           local (const (Map.insert n val2 env')) (eval3 body)
                         _ -> throwError "type error in application"

-- runEval3 Map.empty (eval3 exampleExp)

-- Adding State

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev =
  runIdentity (runStateT (runExceptT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do tick
                   return $ IntVal i
eval4 (Var n) = do tick
                   env <- ask
                   case Map.lookup n env of
                     Nothing -> throwError ("unbound variable: " ++ n)
                     Just val -> return val
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) ->
                            return $ IntVal (i1 + i2)
                          _ -> throwError "type error in addition"
eval4 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval4 (App e1 e2) = do tick
                       val1 <- eval4 e1
                       val2 <- eval4 e2
                       case val1 of
                         FunVal env' n body ->
                           local (const (Map.insert n val2 env')) (eval4 body)
                         _ -> throwError "type error in application"

-- runEval4 Map.empty 0 (eval4 (exampleExp))
