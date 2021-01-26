{-# LANGUAGE OverloadedStrings #-}

module LispVal where

import Control.Monad.Except (ExceptT)
import Data.IORef (IORef)
import qualified Data.Text as T
import System.IO (Handle)
import Text.Parsec (ParseError)

type IOThrowsError = ExceptT LispError IO

type Env = IORef [(T.Text, IORef LispVal)]

data LispVal
  = Atom T.Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String T.Text
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [T.Text], vararg :: Maybe T.Text, body :: [LispVal], closure :: Env}
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

instance Show LispVal where
  show = T.unpack . showVal

-- |
-- >>> showVal (String $ T.pack "Hello")
-- "\"Hello'"
-- >>> showVal (Bool True)
-- "#t"
showVal :: LispVal -> T.Text
showVal (String contents) = T.concat ["\"", contents, "\'"]
showVal (Atom name) = name
showVal (Number contents) = (T.pack . show) contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = T.concat ["(", unwordsList contents, ")"]
showVal (DottedList head0 tail0) = T.concat ["(", unwordsList head0, ".", showVal tail0, ")"]
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = _, closure = _} =
  T.concat
    [ "(lambda (",
      T.unwords (map (T.pack . show) args),
      case varargs of
        Nothing -> ""
        Just arg -> T.concat [" . ", arg],
      ") ...)"
    ]
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

-- |
-- >>> unwordsList [Bool True, Bool False]
-- "#t #f"
-- >>> unwordsList [String $ T.pack "Hello", String $ T.pack "world"]
-- "\"Hello' \"world'"
unwordsList :: [LispVal] -> T.Text
unwordsList = T.unwords . map showVal

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch T.Text LispVal
  | Parser ParseError
  | BadSpecialForm T.Text LispVal
  | NotFunction T.Text T.Text
  | UnboundVar T.Text T.Text
  | Default T.Text

showError :: LispError -> T.Text
showError (UnboundVar message varname) = T.concat [message, ": ", varname]
showError (BadSpecialForm message form) = T.concat [message, ": ", (T.pack . show) form]
showError (NotFunction message func) = T.concat [message, ": ", (T.pack . show) func]
showError (NumArgs expected found) =
  T.concat
    [ "Expected ",
      (T.pack . show) expected,
      " args: found values ",
      unwordsList found
    ]
showError (TypeMismatch expected found) = T.concat ["Invalid type: expected ", expected, ", found ", (T.pack . show) found]
showError (Parser parseErr) = T.concat ["Parse error at ", (T.pack . show) parseErr]
showError (Default err) = T.concat ["Default error at ", (T.pack . show) err]

instance Show LispError where show = T.unpack . showError

-- instance Except LispError where
--   noMsg = Default "An error has occurred"
--   strMsg = Default

type ThrowsError = Either LispError
