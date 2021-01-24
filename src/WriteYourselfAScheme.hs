{-# LANGUAGE ExistentialQuantification #-}

module WriteYourselfAScheme where

import Control.Monad
import Control.Monad.Except
import Data.Functor
import Data.IORef
import Data.Maybe
import System.IO hiding (try)
import Text.ParserCombinators.Parsec hiding (spaces)

-- |
-- >>> parse symbol "" "$"
-- Right '$'
-- >>> parse symbol "" "a"
-- Left (line 1, column 1):
-- unexpected "a"
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

-- |
-- >>> readExpr "     $"
-- Left Parse error at "lisp" (line 1, column 1):
-- unexpected " "
-- expecting letter, "\"", digit, "'" or "("
-- >>> readExpr "     !"
-- Left Parse error at "lisp" (line 1, column 1):
-- unexpected " "
-- expecting letter, "\"", digit, "'" or "("
-- >>> readExpr "     %"
-- Left Parse error at "lisp" (line 1, column 1):
-- unexpected " "
-- expecting letter, "\"", digit, "'" or "("
-- >>> readExpr "(a test)"
-- Right (a test)
-- >>> readExpr "(a (nested) test)"
-- Right (a (nested) test)
-- >>> readExpr "(a (dotted . list) test)"
-- Right (a (dotted.list) test)
-- >>> readExpr "(a '(quoted (dotted . list)) test)"
-- Right (a (quote (quoted (dotted.list))) test)
-- >>> readExpr "(a '(imbalanced parens)"
-- Left Parse error at "lisp" (line 1, column 24):
-- unexpected end of input
-- expecting space or ")"
readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

-- |
-- >>> parse spaces "default" "    sd"
-- Right ()
-- >>> parse spaces "default" "sd"
-- Left "default" (line 1, column 1):
-- unexpected "s"
-- expecting space
spaces :: Parser ()
spaces = skipMany1 space

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

-- |
-- >>> parse parseString "default" "\"hello\""
-- Right "hello'
-- >>> parse parseString "default" "hello"
-- Left "default" (line 1, column 1):
-- unexpected "h"
-- expecting "\""
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

-- |
-- >>> parse parseAtom "default" "hello"
-- Right hello
-- >>> parse parseAtom "default" "he7l8lo"
-- Right he7l8lo
-- >>> parse parseAtom "default" "he7l8lo#t"
-- Right he7l8lo#t
-- >>> parse parseAtom "default" "#t"
-- Right #t
-- >>> parse parseAtom "default" "123"
-- Left "default" (line 1, column 1):
-- unexpected "1"
-- expecting letter
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- |
-- >>> parse parseNumber "default" "123"
-- Right 123
-- >>> parse parseNumber "default" "123asd"
-- Right 123
-- >>> parse parseNumber "default" "asd123asd"
-- Left "default" (line 1, column 1):
-- unexpected "a"
-- expecting digit
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

-- |
-- >>> parse parseList "default" "456"
-- Right (456)
-- >>> parse parseList "default" "asdf"
-- Right (asdf)
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- |
-- >>> parse parseQuoted "default" "'456"
-- Right (quote 456)
-- >>> parse parseQuoted "default" "456"
-- Left "default" (line 1, column 1):
-- unexpected "4"
-- expecting "'"
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- |
-- >>> parse parseExpr "default" "\"hello\""
-- Right "hello'
-- >>> parse parseExpr "default" "hello"
-- Right hello
-- >>> parse parseExpr "default" "123"
-- Right 123
-- >>> parse parseExpr "default" "true"
-- Right true
-- >>> parse parseExpr "default" "(1 2 2)"
-- Right (1 2 2)
-- >>> parse parseExpr "default" "'(1 3 (\"this\" \"one\"))"
-- Right (quote (1 3 ("this' "one')))
-- >>> parse parseExpr "default" "'atom"
-- Right (quote atom)
-- >>> parse parseExpr "default" "2"
-- Right 2
-- >>> parse parseExpr "default" "\"a string\""
-- Right "a string'
-- >>> parse parseExpr "default" "(+ 2 2)"
-- Right (+ 2 2)
parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

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

instance Show LispVal where show = showVal

-- |
-- >>> nullEnv >>= (\e -> runExceptT $ eval e (Bool True))
-- Right #t
-- >>> nullEnv >>= (\e -> runExceptT $ eval e (String "Hello"))
-- Right "Hello'
-- >>> nullEnv >>= (\e -> runExceptT $ eval e (List [Atom "if", (Bool True), (String "Hello"), (String "No no")]))
-- Right "Hello'
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
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
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("reminder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params <&> (Number . foldl1 op)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

-- |
-- >>> numBoolBinop (>) [Number 7, Number 4]
-- Right #t
numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

-- |
-- >>> strBoolBinop (==) [String "Hello", String "Hello"]
-- Right #t
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

-- |
-- >>> boolBoolBinop (||) [Bool True, Bool False]
-- Right #t
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

-- |
-- >>> unpackNum (Number 7)
-- Right 7
-- >>> unpackNum (String "7")
-- Right 7
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- |
-- >>> unpackStr (Number 7)
-- Right "7"
-- >>> unpackStr (String "7")
-- Right "7"
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

-- |
-- >>> unpackBool (Number 7)
-- Left Invalid type: expected boolean, found 7
-- >>> unpackBool (Bool True)
-- Right True
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- |
-- >>> car [(List [String "Hello", String "world"])]
-- Right "Hello'
-- >>> car [(DottedList [String "Hello", String "world"] (Bool True))]
-- Right "Hello'
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- |
-- >>> cdr [(List [String "Hello", String "world"])]
-- Right ("world')
-- >>> cdr [(DottedList [String "Hello", String "world"] (Bool True))]
-- Right ("world'.#t)
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch " pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- |
-- >>> cons [(List [String "Hello", String "world"]), List []]
-- Right (("Hello' "world'))
-- >>> cons [(String "ASdf"), (List [String "Hello", String "world"])]
-- Right ("ASdf' "Hello' "world')
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- |
-- >>> eqv [(String "Hello"), (String "Hello")]
-- Right #t
-- >>> eqv [(List [String "Hello", String "world"]), (List [String "Hello", String "world"])]
-- Right #t
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left err -> False
      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return (unpacked1 == unpacked2) `catchError` const (return False)

-- |
-- >>> equal []
-- Left Expected 2 args: found values
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <-
    or
      <$> mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- |
-- >>> primitiveBindings >>= (\e -> evalString e "(+ 2 3)")
-- "5"
-- >>> primitiveBindings >>= (\e -> evalString e "(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))")
-- "(lambda (\"x\") ...)"
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> isJust . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
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

makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeVarargs = makeFunc . Just . showVal

-----------------------------------------------------------------------------
-- Exercises
-- 1. Rewrite parseNumber using
-- (a) do-notation.
parseNumber0 :: Parser LispVal
parseNumber0 = do
  ds <- many1 digit
  pure $ (Number . read) ds

-- (b) explicit sequencing with the >>= operator.
parseNumber1 :: Parser LispVal
parseNumber1 = many1 digit <&> Number . read

-- 2. Our strings aren’t quite R5RS compliant,
-- because they don’t support escaping of internal quotes within the string.
-- Change parseString so that \" gives a literal quote character instead of terminating the string.
-- You may want to replace noneOf "\"" with a new parser action that accepts
-- either a non-quote character or a backslash followed by a quote mark.

-- 3. Modify the previous exercise to support \n, \r, \t, \\, and any other desired escape characters.
-- |
-- >>> parse parseString1 "default" "\"hello\""
-- Right "hello'
-- >>> parse parseString1 "default" "\"hello\nhello\""
-- Right "hello
-- hello'
-- >>> parse parseString1 "default" "\"hello\thello\""
-- Right "hello	hello'
-- >>> parse parseString1 "default" "\"hello\\hello\""
-- Right "hello\hello'
parseString1 :: Parser LispVal
parseString1 = do
  char '"'
  x <- many (noneOf "\"" <|> oneOf "\n\r\t\\")
  char '"'
  return $ String x

-- 4. Change parseNumber to support the Scheme standard for different bases.
-- You may find the readOct and readHex functions useful.

-- 5. Add a Character constructor to LispVal,
-- and create a parser for character literals as described in R5RS.

-- 6. Add a Float constructor to LispVal, and support R5RS syntax for decimals.
-- The Haskell function readFloat may be useful.

-- 7. Add data types and parsers to support the full numeric tower of Scheme numeric types.
-- Haskell has built-in types to represent many of these; check the Prelude.
-- For the others, you can define compound types that represent
-- eg. a Rational as a numerator and denominator,
-- or a Complex as a real and imaginary part (each itself a Real number).
-----------------------------------------------------------------------------
-- Exercises
-- 1. Add support for the backquote syntactic sugar:
-- the Scheme standard details what it should expand into (quasiquote/unquote).

-- 2. Add support for vectors.
-- The Haskell representation is up to you:
-- GHC does have an Array data type, but it can be difficult to use.
-- Strictly speaking, a vector should have constant-time indexing and updating,
-- but destructive update in a purely functional language is difficult.
-- You may have a better idea how to do this after the section on set!,
-- later in this tutorial.

-- 3. Instead of using the try combinator,
-- left-factor the grammar so that the common subsequence is its own parser.
-- You should end up with a parser that matches a string of expressions,
-- and one that matches either nothing or a dot and a single expressions.
-- Combining the return values of these into either a List or a DottedList is left
-- as a (somewhat tricky) exercise for the reader:
-- you may want to break it out into another helper function.
-----------------------------------------------------------------------------
-- Exercises
-- 1. Add primitives to perform the various type-testing functions of R5RS:
-- symbol?, string?, number?, etc.

-- 2. Change unpackNum so that it always returns 0 if the value is not a number,
-- even if it’s a string or list that could be parsed as a number.

-- 3. Add the symbol-handling functions from R5RS.
-- A symbol is what we’ve been calling an Atom in our data constructors.
-----------------------------------------------------------------------------
-- Exercises
-- 1. Instead of treating any non-false value as true,
-- change the definition of if so that the predicate accepts only Bool values and throws an error on any others.

-- 2. equal? has a bug in that a list of values is compared using eqv? instead of equal?.
-- For example, (equal? ’(1 "2") ’(1 2)) = #f, while you’d expect it to be true.
-- Change equal? so that it continues to ignore types as it recurses into list structures.
-- You can either do this explicitly, following the example in eqv?,
-- or factor the list clause into a separate helper function that is parameterized by the equality testing function.

-- 3. Implement cond and case expressions.

-- 4. Add the rest of the string functions.
-- You don’t yet know enough to do string-set!;
-- this is difficult to implement in Haskell, but you’ll have enough information after the next two sections.
-----------------------------------------------------------------------------
-- Exercises
-- 1. Instead of treating any non-false value as true, change the definition of if
-- so that the predicate accepts only Bool values and throws an error on any others.

-- 2. equal? has a bug in that a list of values is compared using eqv? instead of equal?.
-- For example, (equal? ’(1 "2") ’(1 2)) = #f, while you’d expect it to be true.
-- Change equal? so that it continues to ignore types as it recurses into list structures.
-- You can either do this explicitly, following the example in eqv?,
-- or factor the list clause into a separate helper function that is parameterized by the equality testing function.

-- 3. Implement cond and case expressions.

-- 4. Add the rest of the string functions.
-- You don’t yet know enough to do string-set!; this is difficult to implement in Haskell,
-- but you’ll have enough information after the next two sections.
