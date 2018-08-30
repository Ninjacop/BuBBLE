{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Types where

-- Problems:
--
-- (if (= 1 2)) returns: "If is an Unbound var", probably because there are no expressions after the condition
-- 
-- Vector is "broken", I don't know how to use it, lol. It's included, but you have to uncomment everything related to it 


-- Import Monads n' Stuff
import Control.Monad (liftM)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Error
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Concurrent (threadDelay) -- Will be used for a `random` function later

import Data.Array (Array (..), listArray)
import Data.Char (toLower)
import Data.Complex (Complex (..))
import Data.IORef
import Data.List
import Data.Ratio (Rational (..), (%))

import Numeric (readOct, readHex)

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import System.Environment
import System.IO 
import System.Process
import System.Exit (ExitCode (..))
import System.Posix.Unistd


-- All the custom data types, which follows the syntax: [Name - Haskell Type]
data Values = Atom String
             | List [Values]
             | DottedList [Values] Values
             | Number Integer
             | ShortNum Int
             | InOut (IO ()) -- Special Type for External Hackage/Haskell Functions
             | InOutNum (IO Int) -- Special Type for External Hackage/Haskell Functions
             | Ratio Rational
             | Float Double
             | Complex (Complex Double)
             | String String
             | Char Char
             | Bool Bool
             {- | Vector (Array Int Values) -- Uncomment this and other functions related to Vectors to enable them -- STILL IN TESTING -}
             | PrimitiveFunc ([Values] -> ThrowsError Values) -- Functions declared through `Primitives.hs`
             | Func { params :: [String] -- Functions declared through `eval`
                    , vararg :: (Maybe String)
                    , body :: [Values]
                    , closure :: Env
                    }
             | IOFunc ([Values] -> IOThrowsError Values) -- "But wait, why isn't this used like `InOut` and `InOutNum`?" - This is used for File and REPL IO
             | Port Handle
             | Seed Int


-- I'm using this instead of doing `deriving (Show)`, because it works better
instance Show Values where show = showVal


-- All custom errors that sometimes takes custom `Values` [Name - Maybe some Values]
data BubbleError = NumArgs Integer [Values]
               | ExpectCondClauses
               | ExpectCaseClauses
               | TypeMismatch String Values
               | Parser ParseError
               | BadSpecialForm String Values
               | NotFunction String String
               | UnboundVar String String
               | Default String


-- Same deal for `Values`
instance Show BubbleError where show = showError


-- `noMsg` is called when the parser doesn't know what happened, and `strMsg` just shows a certain error - these are both apart of  
instance Error BubbleError where
    noMsg = Default "An unknown error has occurred"
    strMsg = Default


type ThrowsError = Either BubbleError -- This is for error handling regular functions that do not need File IO, AKA regular functions
type Env = IORef [(String, IORef Values)] -- Works alongside with `eval`, makes the process of defining those functions much easier
type IOThrowsError = ErrorT BubbleError IO -- This is for error handling functions that need File IO


-- I forget what this specifically does, but iirc it "unpacks" a function such as `=` or `+` (See any of the unpack____ functions for further usage)
data Unpacker = forall a. Eq a => AnyUnpacker (Values -> ThrowsError a)

-- Error Handling
showError :: BubbleError -> String
showError ExpectCondClauses = "Expected at least 1 true cond clause" -- Raised when `cond` cannot return any value, because there's no condition that has been met
showError ExpectCaseClauses = "Expected at least 1 true case clause" -- Same thing with `ExpectCondCases`, except with `case`
showError (UnboundVar varname msg) = msg ++ " is an unbound variable: \nDefine it by typing (def " ++ msg ++ " [values go here])   -" ++ "   (Error: " ++ varname ++ ") \nIt might also be a misspelling\n " -- Raised when a variable or function is undefined or misspelled, and gives a suggestion on what you should do to maybe prevent that 
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form -- Raised when a special function where not 
showError (NotFunction func msg) = func ++ " is not a function \nDefine it by typing (defn " ++ func ++ " [body of function goes here])\n" 


showError (NumArgs expected found) = "Current function should have " ++ show expected
                                   ++ " args; found " ++ unwordsList found 


showError (TypeMismatch expected found) = "Invalid type:  " ++ show found 
                                   ++ ", it should be a " ++ expected 


showError (Parser parseErr) = "Parser error at " ++ show parseErr

-- Show functions
showVal :: Values -> String

-- Use the first showVal (String s) if it becomes annoying to have no double quotes (" ")
-- showVal (String s) = "\"" ++ s ++ "\""
showVal (String s) = s
showVal (Atom name) = name
showVal (Number n) = show n
showVal (ShortNum m) = show m
showVal (Bool True) = "true"
showVal (Bool False) = "false"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
{- showVal (Vector v) = show v -- Uncomment this and other functions related to Vectors to enable them -- STILL IN TESTING -}
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Char c) = ['\'', c, '\'']
showVal (PrimitiveFunc _) = "<primitive>" -- example: doing (cons + 1) will result as (<primitive> . 1)
showVal (Float f) = show f
showVal (Complex i) = show i 
showVal (Ratio r) = show r


-- This is what prints after you declare a function in the REPL
showVal (Func {params=args, vararg=varargs, body=body, closure=env}) =
    "(use (" ++ unwords (map show args) ++
      (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"


-- Same case as `showVal (PrimitiveFunc _)`, but "<port>" is returned when you define a port in the REPL
showVal (Port _) = "<port>"
showVal (IOFunc _) = "<primitive>"

-- Via recursion, it takes input and turns it into a list (see showVal (DottedList _)/(List _))
unwordsList :: [Values] -> String
unwordsList = unwords . map showVal


-- Short for "numeric binary operator", it is the basis of creating primitives (most notably used in `Primitives.hs`) 
numericBinop :: (Integer -> Integer -> Integer) 
             -> [Values] 
             -> ThrowsError Values
numericBinop op single@[_] = throwError $ NumArgs 2 single
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op


-- This defines that the ___BoolBinop corresponds to every "boolean binary operator" or `boolBinop` that takes in a unpacked type, as in `unpack____`
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool
charBoolBinop = boolBinop unpackChar


--- Overarching function that encompasses all unary operators
unaryOp :: (Values -> Values) 
        -> [Values] 
        -> ThrowsError Values
unaryOp func [arg] = return $ func arg


-- Overarching function that encompasses all binary operators that return true/false after the process is completed
boolBinop :: (Values -> ThrowsError a) 
          -> (a -> a -> Bool) 
          -> [Values] 
          -> ThrowsError Values
boolBinop unpacker op [x,y] = do
    left <- unpacker x
    right <- unpacker y
    return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args


-- Takes all the possibilities of what a number can do, and returns a whole number
unpackNum :: Values -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


-- Takes all the possibilities of what a string (or a list of characters) can do, and returns a string
unpackStr :: Values -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr


-- Takes all the ... of a boolean can do, and returns a boolean
unpackBool :: Values -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool


-- Takes all the ... of a character can do, and returns a boolean
unpackChar :: Values -> ThrowsError Char
unpackChar (Char c) = return c 
unpackChar notChar = throwError $ TypeMismatch "char" notChar


-- Takes the ... of the equals operator, and evaluates based upon if the correct number of arguments 
unpackEquals :: Values -> Values -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
  `catchError` (const $ return False)


-- A function that converts binary to whole numbers, used in parsing binary numbers and used in `Parser.hs`
bin2int :: String -> Integer
bin2int s = sum $ map (\(i,x) -> i*(2^x)) $ zip [0..] $ map p (reverse s)
          where p '0' = 0
                p '1' = 1


-- Helps take `showVal` and put it out to the REPL
flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout


-- Used when (read) is invoked, or when *anything* is inputted into the REPL
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


-- REPL function that until a specific string/character is typed, it will stay in the REPL (because the 'L' means Loop, for those who don't remember)
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
       then return ()
       else action result >> until_ pred prompt action


-- A helper function that acts like lift, but for the custom `ThrowsError`
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


-- This executes all the File IO, and throws and error if something goes wrong. "run IO that Throws and Error"
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue


-- This is used as to read Octals and Hexidecimals (used in `Parser.hs`)
readWith :: (t -> [(a, b)]) -> t -> a
readWith f s = fst $ f s !! 0


-- 
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- A definition that finds or "traps" an error, then outputs the error. ("I trapped this error over here, you should probably see this")
trapError action = catchError action (return . show)
