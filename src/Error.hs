module Error where

import Control.Monad.Except
import DataTypes
import Text.ParserCombinators.Parsec hiding (spaces)

data LispError = Numargs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | UnboundVar String String
               | NotFunction String String

instance Show LispError where show = showError

showError :: LispError -> String
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (UnboundVar message val) = message ++ ": " ++ val
showError (NotFunction message func) = message ++ ": " ++ func
showError (Numargs expected found) = "Expected " ++ show expected
                                   ++ " args; found values" ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid Type; expected " ++ expected
                                        ++ ", found " ++ show found
showError (Parser err) = "Parse error at " ++ show err

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
