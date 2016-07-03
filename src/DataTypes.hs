module DataTypes where

import Data.IORef
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: Env }
instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | UnboundVar String String
               | NotFunction String String
instance Show LispError where show = showError

showVal :: LispVal -> String
showVal (String val) = "\"" ++ val ++ "\""
showVal (Atom val) = val
showVal (Number num) =  show num
showVal (Bool bool) = if bool == True then "#t" else "#f"
showVal (List val) = "(" ++ unwordsList val ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showError :: LispError -> String
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (UnboundVar message val) = message ++ ": " ++ val
showError (NotFunction message func) = message ++ ": " ++ func
showError (NumArgs expected found) = "Expected " ++ show expected
                                   ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid Type; expected " ++ expected
                                        ++ ", found " ++ show found
showError (Parser err) = "Parse error at " ++ show err

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
