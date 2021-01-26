{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Control.Monad.Except
  ( MonadError (catchError),
    MonadIO (liftIO),
    runExceptT,
  )
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import qualified Data.Text as T
import Eval (apply, bindVars, eval, liftThrows, load, readExpr)
import LispVal
  ( Env,
    IOThrowsError,
    LispVal (Atom, Bool, IOFunc, List, Port, PrimitiveFunc, String),
    ThrowsError,
  )
import Prim (primitives)
import System.IO
  ( IOMode (ReadMode, WriteMode),
    hClose,
    hFlush,
    hGetLine,
    hPrint,
    hPutStrLn,
    openFile,
    stderr,
    stdin,
    stdout,
  )

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predi prompt action = do
  result <- prompt
  if predi result
    then return ()
    else action result >> until_ predi prompt action

nullEnv :: IO Env
nullEnv = newIORef []

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

flushStr :: T.Text -> IO ()
flushStr str = putStr (T.unpack str) >> hFlush stdout

readPrompt :: T.Text -> IO T.Text
readPrompt prompt = flushStr prompt >> fmap T.pack getLine

ioPrimitives :: [(T.Text, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile (T.unpack filename) mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr . T.pack

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap (String . T.pack) $ liftIO $ readFile (T.unpack filename)

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

evalAndPrint :: Env -> T.Text -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn . T.unpack

-- |
-- >>> primitiveBindings >>= (\e -> evalString e $ T.pack "(+ 2 3)")
-- "5"
-- >>> primitiveBindings >>= (\e -> evalString e $ T.pack "(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))")
-- "(lambda (\"x\") ...)"
evalString :: Env -> T.Text -> IO T.Text
evalString env expr = runIOThrows $ fmap (T.pack . show) $ liftThrows (readExpr expr) >>= eval env

runOne :: [T.Text] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (T.pack . show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr . T.unpack

runIOThrows :: IOThrowsError T.Text -> IO T.Text
runIOThrows action = runExceptT (trapError action) <&> extractValue

trapError :: (MonadError a m, Show a) => m T.Text -> m T.Text
trapError action = catchError action (return . T.pack . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
