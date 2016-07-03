module Env where

import DataTypes
import Data.IORef
import Control.Monad.Except
import Control.Monad

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (\_ -> True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting unbound var" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String ->LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting unbound var" var) (liftIO . (flip writeIORef value)) (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do alreadyDefined <- liftIO $ isBound envRef var
                                case alreadyDefined of
                                  True -> setVar envRef var value >> return value
                                  otherwise -> liftIO $ do
                                                  valueRef <- newIORef value
                                                  env <- readIORef envRef
                                                  writeIORef envRef ((var, valueRef):env)
                                                  return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
                           where extendEnv bindings env = liftM (++ env)
                                                          (mapM addBinding bindings)
                                 addBinding (var, value) = do ref <- newIORef value
                                                              return (var, ref)
