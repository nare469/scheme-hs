
module Evaluator where

import DataTypes
import Error
import Control.Monad.Except

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp (div)),
              ("mod", numericBinOp (mod)),
              ("=", numBoolBinOp (==)),
              ("<", numBoolBinOp (<)),
              (">", numBoolBinOp (>)),
              ("/=", numBoolBinOp (/=)),
              (">=", numBoolBinOp (>=)),
              ("<=", numBoolBinOp (<=)),
              ("&&", boolBoolBinOp (&&)),
              ("||", boolBoolBinOp (||)),
              ("string=?", strBoolBinOp (==)),
              ("string>?", strBoolBinOp (>)),
              ("string<?", strBoolBinOp (<)),
              ("string>=?", strBoolBinOp (>=)),
              ("string<=?", strBoolBinOp (<=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv)]


numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op [] = throwError $  NumArgs 2 []
numericBinOp op (val:[]) = throwError $ NumArgs 2 [val]
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op (arg1:arg2:[]) = do
                                       left <- unpacker arg1
                                       right <- unpacker arg2
                                       return $ Bool $ op left right

numBoolBinOp = boolBinOp unpackNum
strBoolBinOp = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool

-- Unpacker functions

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s :: [(Integer, String)] in
                            if null parsed
                            then throwError $ TypeMismatch "number" $ String s
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- Special List functions

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] =  throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] =  throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                 (all eqvPair $ zip arg1 arg2)
                                                     where eqvPair (x1, x2) = case eqv [x1,x2] of
                                                                                Left err -> False
                                                                                Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- Evaluator functions

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do result <- eval pred
                                                case result of
                                                    Bool False -> eval alt
                                                    otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized function" func)
                        ($ args)
                        (lookup func primitives)
