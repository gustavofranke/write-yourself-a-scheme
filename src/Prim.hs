{-# LANGUAGE ExistentialQuantification #-}

module Prim where

import Control.Monad.Except ( MonadError(catchError, throwError) )
import Data.Functor ( (<&>) )
import LispVal
    ( ThrowsError,
      LispError(TypeMismatch, NumArgs),
      LispVal(Bool, Atom, DottedList, List, String, Number) )

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
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
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
-- >>> car [(List [String "Hello", String "world"])]
-- Right "Hello'
-- >>> car [(DottedList [String "Hello", String "world"] (Bool True))]
-- Right "Hello'
car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- |
-- >>> cdr [(List [String "Hello", String "world"])]
-- Right ("world')
-- >>> cdr [(DottedList [String "Hello", String "world"] (Bool True))]
-- Right ("world'.#t)
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
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
      Left _ -> False
      Right (Bool val) -> val
      Right _ -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

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
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
  unpacked1 <- unpacker arg1
  unpacked2 <- unpacker arg2
  return (unpacked1 == unpacked2) `catchError` const (return False)
