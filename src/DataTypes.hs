module DataTypes where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String val) = "\"" ++ val ++ "\""
showVal (Atom val) = val
showVal (Number num) =  show num
showVal (Bool bool) = if bool == True then "#t" else "#f"
showVal (List val) = "(" ++ unwordsList val ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
