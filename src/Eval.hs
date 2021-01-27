{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    MonadIO (liftIO),
    runExceptT,
  )
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import LispVal
  ( Env,
    IOThrowsError,
    LispError (BadSpecialForm, NumArgs, Parser, UnboundVar, Default),
    LispVal
      ( Atom,
        Bool,
        DottedList,
        Func,
        List,
        Number,
        PrimitiveFunc,
        String
      ),
    ThrowsError,
    showVal,
  )
import Parser (parseExpr, spaces)
import Prim
import Text.ParserCombinators.Parsec (Parser, endBy, parse)

-- |
-- >>> newIORef [] >>= (\e -> runExceptT $ eval e (Bool True))
-- Right #t
-- >>> newIORef [] >>= (\e -> runExceptT $ eval e (String $ T.pack "Hello"))
-- Right "Hello'
-- >>> newIORef [] >>= (\e -> runExceptT $ eval e (List [Atom $ T.pack "if", (Bool True), (String $ T.pack "Hello"), (String $ T.pack "No no")]))
-- Right "Hello'
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom ident) = getVar env ident
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predi, conseq, alt]) = do
  result <- eval env predi
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List [Atom "load", String filename]) = load filename >>= fmap last . mapM (eval env)
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc :: Monad m => Maybe T.Text -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeVarargs = makeFunc . Just . showVal

load :: T.Text -> IOThrowsError [LispVal]
load filename = liftIO (readFile (T.unpack filename)) >>= liftThrows . readExprList . T.pack

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

isBound :: Env -> T.Text -> IO Bool
isBound envRef var = readIORef envRef <&> isJust . lookup var

-- |
-- >>> readExpr $ T.pack "     $"
-- Left Parse error at "lisp" (line 1, column 1):
-- unexpected " "
-- expecting letter, "\"", digit, "'" or "("
-- >>> readExpr $ T.pack "     !"
-- Left Parse error at "lisp" (line 1, column 1):
-- unexpected " "
-- expecting letter, "\"", digit, "'" or "("
-- >>> readExpr $ T.pack "     %"
-- Left Parse error at "lisp" (line 1, column 1):
-- unexpected " "
-- expecting letter, "\"", digit, "'" or "("
-- >>> readExpr $ T.pack "(a test)"
-- Right (a test)
-- >>> readExpr $ T.pack "(a (nested) test)"
-- Right (a (nested) test)
-- >>> readExpr $ T.pack "(a (dotted . list) test)"
-- Right (a (dotted.list) test)
-- >>> readExpr $ T.pack "(a '(quoted (dotted . list)) test)"
-- Right (a (quote (quoted (dotted.list))) test)
-- >>> readExpr $ T.pack "(a '(imbalanced parens)"
-- Left Parse error at "lisp" (line 1, column 24):
-- unexpected end of input
-- expecting space or ")"
readExpr :: T.Text -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readOrThrow :: Parser a -> T.Text -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" (T.unpack input) of
  Left err -> throwError $ Parser err
  Right val -> return val

readExprList :: T.Text -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

-- |
-- >>> runExceptT(apply (PrimitiveFunc (numericBinop (+))) [Number 4, Number 7])
-- Right 11
-- >>> runExceptT(apply (PrimitiveFunc (numBoolBinop (>))) [Number 4, Number 7])
-- Right #f
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env
apply val vals = throwError $ Default (T.pack ("invalid call with: " ++ show val ++ " and: " ++ show vals))

bindVars :: Env -> [(T.Text, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv binds env = fmap (++ env) (mapM addBinding binds)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

getVar :: Env -> T.Text -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> T.Text -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> T.Text -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value
