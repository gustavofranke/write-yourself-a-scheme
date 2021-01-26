module LispVal where

import Control.Monad.Except
import Data.IORef
import System.IO
import Text.Parsec

type IOThrowsError = ExceptT LispError IO

type Env = IORef [(String, IORef LispVal)]

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

instance Show LispVal where show = showVal

-- |
-- >>> showVal (String "Hello")
-- "\"Hello'"
-- >>> showVal (Bool True)
-- "#t"
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\'"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
  "(lambda (" ++ unwords (map show args)
    ++ ( case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

-- |
-- >>> unwordsList [Bool True, Bool False]
-- "#t #f"
-- >>> unwordsList [String "Hello", String "world"]
-- "\"Hello' \"world'"
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args: found values "
    ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-- instance Except LispError where
--   noMsg = Default "An error has occurred"
--   strMsg = Default

type ThrowsError = Either LispError
