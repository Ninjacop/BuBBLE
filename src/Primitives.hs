module Primitives where 

import Types
import Parser

import Control.Monad (liftM)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Error
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader

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
import System.Random
import Control.Concurrent (threadDelay)


-- Primitive functions lookup table
-- Also serves as a bank of all the functions that are able to be parsed that aren't in `eval`
-- Most custom functions that I (Ninjacop) added are here too
primitives :: [(String, [Values] -> ThrowsError Values)]
primitives = [("+", numericBinop (+)) 
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("gcd", numericBinop gcd)
             ,("lcm", numericBinop lcm)
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quot", numericBinop quot)
             ,("rem", numericBinop rem)
             ,("max", numericBinop max)
             ,("min", numericBinop min)
             ,("exp", numericBinop (^))
             ,("eq", numBoolBinop (==))
             ,("lt", numBoolBinop (<))
             ,("gt", numBoolBinop (>))
             ,("not=", numBoolBinop (/=))
             ,("gte", numBoolBinop (>=))
             ,("lte", numBoolBinop (<=))
             ,("and", boolBoolBinop (&&))
             ,("or", boolBoolBinop (||))
             ,("string?eq", strBoolBinop (==))
             ,("string?lt", strBoolBinop (<))
             ,("string?gt", strBoolBinop (>))
             ,("string?lte", strBoolBinop (<=))
             ,("string?gte", strBoolBinop (>=))
             ,("char?eq", charBoolBinop (==))
             ,("char?lt", charBoolBinop (<))
             ,("char?gt", charBoolBinop (>))
             ,("char?lte", charBoolBinop (<=))
             ,("char?gte", charBoolBinop (>=))
             ,("string-ci?eq", strBoolBinop (ci_help (==))) -- ci means Case Insensitive
             ,("string-ci?lt", strBoolBinop (ci_help (<)))
             ,("string-ci?gt", strBoolBinop (ci_help (>)))
             ,("string-ci?lte", strBoolBinop (ci_help (<=)))
             ,("string-ci?gte", strBoolBinop (ci_help (>=)))
             ,("not", unaryOp not')
             ,("bool?", unaryOp boolP)
             ,("list?", unaryOp listP)
             ,("symbol?", unaryOp symbolP)
             ,("char?", unaryOp charP)
             ,("string?", unaryOp stringP)
             ,("vector?", unaryOp vectorP)
             ,("even?", even')
             ,("odd?", odd')
             ,("symbol2string", unaryOp symbol2string)
             ,("string2symbol", unaryOp string2symbol)
             ,("car", car)
             ,("cdr", cdr)
             ,("cons", cons)
             ,("eq", eqv)
             ,("equal", equal)
             ,("coll", make_string) -- (coll 5 #\r) -> rrrrr
             ,("string", create_string) -- (string #\a #\h) -> ah
             ,("length", string_length) 
             ,("find", char_at) -- (find "asdg" 3) -> 'g'
             ,("substring", substring) -- (substring "hello" 0 3) -> hel
             ,("concat", string_append) -- (concat "hello" "world") -> helloworld
             ,("print", princ)]


-- IO Primitives -- DISCLAMER - openinput/openoutput should to be set to a variable: (def x (openoutput "somefile")), but can go without it
-- Serves as a lookup table/bank of all functions related to File IO/REPL IO
-- Apply acts like `map` from various Lisps
ioPrimitives :: [(String, [Values] -> IOThrowsError Values)]
ioPrimitives = [("apply", applyProc)
               ,("openinput", makePort ReadMode)
               ,("openoutput", makePort WriteMode)
               ,("close", closePort)
               ,("read", readProc)
               ,("write", writeProc)
               ,("readstring", readContents)
               ,("readlist", readAll)]



-- IO Primitive function declarations 

-- Creates a port with a specific file name and a mode (read or write) that writes to files
makePort :: IOMode -> [Values] -> IOThrowsError Values
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode


-- Closes a port to a file, stopping the procedure of writing and reading to a file
closePort :: [Values] -> IOThrowsError Values
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False


-- If no port is defined, it reads the next inputted line (in the REPL, it waits for an input) and returns that
-- If a port is defined, it reads the first line of the file
readProc :: [Values] -> IOThrowsError Values
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr


-- If no port is defined, it acts like a "print" statement, always returning "true" on the next line
writeProc :: [Values] -> IOThrowsError Values
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)


-- Reads all the contents of a file and returns a string - " "
readContents :: [Values] -> IOThrowsError Values
readContents [String filename] = liftM String $ liftIO $ readFile filename


-- Loads a file, ignoring the file extension of it, and only checks for syntax errors
load :: String -> IOThrowsError [Values]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList


-- Reads all the contents of a file and returns a list including all the contents in a string - ("x")
readAll :: [Values] -> IOThrowsError Values
readAll [String filename] = liftM List $ load filename


-- A helper function for Case Insensitive string/char comparison
ci_help :: (String -> String -> Bool) -> String -> String -> Bool
ci_help f a b = f (map toLower a) (map toLower b)


-- The print that doesn't return anything but the value(s) you put in! FINALLY I CAN GO THROUGH A LISP WITHPUT THAT ANNOYING THING ;)
princ :: [Values] -> ThrowsError Values
princ [Number x] = return $ Number x 
princ [Float x] = return $ Float x
princ [Ratio x] = return $ Ratio x 
princ [Complex x] = return $ Complex x
princ [String x] = return $ String x
princ [List x] = return $ List x
princ [DottedList (x) y] = return $ (DottedList x y)


-- Haskell defiinition of the BuBBLE's car function
car :: [Values] -> ThrowsError Values
car [List (x:xs)] = return x 
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "list" badArg
car badArgList = throwError $ NumArgs 1 badArgList


-- Haskell def of BuBBLE equivalent of cdr
cdr :: [Values] -> ThrowsError Values
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [x] y] = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "list" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList


-- Haskell def of BuBBLE equivalent of cons
cons :: [Values] -> ThrowsError Values
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x,y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList


-- Haskell def of BuBBLE equivalent of eq
eqv :: [Values] -> ThrowsError Values
eqv [(Bool b1), (Bool b2)] = (return . Bool) $ b1 == b2
eqv [(Number n1), (Number n2)] = (return . Bool) $ n1 == n2
eqv [(String s1), (String s2)] = (return . Bool) $ s1 == s2
eqv [(Atom a1), (Atom a2)] = (return . Bool) $ a1 == a2

eqv [(DottedList xs x), (DottedList ys y)] = 
    eqv [List $ xs ++ [x], List $ ys ++ [y]]

eqv [(List l1), (List l2)]
    | length l1 /= length l2 = return $ Bool False
    | otherwise = (return . Bool) $ all byPairs $ zip l1 l2
  where byPairs (x,y) = case eqv [x,y] of
                             Left err -> False
                             Right (Bool val) -> val

eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


-- Haskell def of BuBBLE equivalent of equal
equal :: [Values] -> ThrowsError Values
equal [(List l1), (List l2)] = (return . Bool) $ all byPairs $ zip l1 l2
  where byPairs (x,y) = case equal [x,y] of
                             Left err -> False
                             Right (Bool val) -> val

equal [(DottedList xs x), (DottedList ys y)] =
    equal [List $ xs ++ [x], List $ ys ++ [y]]

equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool, AnyUnpacker unpackChar]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList


-- Can only parse numbers, this detremines whether a number is even
even' :: [Values] -> ThrowsError Values
even' [Number n] = do
    if n `rem` 2 == 0 
        then 
            return $ Bool True
        else 
            return $ Bool False

even' [badArg] = throwError $ TypeMismatch "number" $ badArg


-- Can only parse numbers, this determines whether a number is odd 
odd' :: [Values] -> ThrowsError Values
odd' [Number n] = do 
    if n `rem` 2 /= 0 
        then 
            return $ Bool True 
        else 
            return $ Bool False

odd' [badArg] = throwError $ TypeMismatch "number" $ badArg 


-- String primitives

-- Haskell def of BuBBLE equivalent of coll
make_string :: [Values] -> ThrowsError Values
make_string [Number k, Char c] = return $ String $ replicate (fromIntegral k)  c
make_string badArgs = throwError $ TypeMismatch "number char -- The current function takes a number then a character, defined like #\\x" $ List badArgs


-- Haskell def of BuBBLE equivalent of string function, not the type 
create_string :: [Values] -> ThrowsError Values
create_string xs
    | all isChar xs = return $ String $ foldr f "" xs 
    | otherwise = throwError $ TypeMismatch "list of chars -- chars are defined like #\\x " $ List xs
  where
    isChar (Char _) = True
    isChar _ = False
    f (Char c) accum = c : accum


-- Haskell def of BuBBLE equivalent of length (finds string length)    
string_length :: [Values] -> ThrowsError Values
string_length [String s] = (return . Number . fromIntegral . length) s
string_length badArgs = throwError $ TypeMismatch "string" $ List badArgs


-- Haskell def of BuBBLE equivalent of find 
char_at :: [Values] -> ThrowsError Values
char_at [String s, Number n] = (return . Char) (s !! (fromIntegral n))
char_at badArgs = throwError $ TypeMismatch "(string number) -- a list containing a string then a number" $ List badArgs


-- Haskell def of BuBBLE equivalent of substring - finds a string of chars given a range
substring :: [Values] -> ThrowsError Values
substring [String s, Number start, Number end] =
    let start' = fromIntegral start
        end' = fromIntegral end
    in  (return . String) (drop start' $ take end' $ s)
substring badArgs = throwError $ TypeMismatch "(string number number) -- a list containing a string then 2 numbers " $ List badArgs


-- Haskell def of BuBBLE equivalent of concat
string_append :: [Values] -> ThrowsError Values
string_append ss
    | all isString ss = (return . String . concat) $ map (\(String s) -> s) ss
    | otherwise = throwError $ TypeMismatch "list of string" $ List ss
  where
    isString (String _) = True
    isString _ = False


-- Smushes all the parts of each function in the both primitive tables to make them be able to parse them
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

makeFunc varargs env params body = return $ Func (map showVal  params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal  


-- Evaluator 

-- THE BIGGEST BOI OF THE GROUP
eval :: Env -> Values -> IOThrowsError Values
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Float _) = return val
eval env val@(Complex _) = return val
eval env val@(Ratio _) = return val
eval env val@(InOutNum _) = return val
eval env val@(ShortNum _) = return val
eval env val@(InOut _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val


-- if statement, pretty basic
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        otherwise  -> throwError $ TypeMismatch "boolean" result 


-- cond statement, similar to all the other LISPS -- compressed `if` statements
eval env (List (Atom "cond" : expr : rest)) = do
    eval' expr rest
    where eval' (List [cond, value]) (x : xs) = do
              result <- eval env cond
              case result of
                   Bool False -> eval' x xs
                   Bool True  -> eval env value
                   otherwise  -> throwError $ TypeMismatch "boolean" cond
          eval' (List [Atom "else", value]) [] = do
               eval env value
          eval' (List [cond, value]) [] = do
              result <- eval env cond
              case result of
                   Bool True  -> eval env value
                   otherwise  -> throwError $ TypeMismatch "boolean" cond

    
            
-- Case function, like scheme and racket, it has two parts because it is **complex**
eval env (List (Atom "case" : [])) = throwError ExpectCaseClauses

eval env (List (Atom "case" : key : cs)) = do
    keyVal <- eval env key
    evalCaseCases env keyVal cs


-- This is like swap! in clojure, except you don't need atoms 
eval env (List [Atom "set!", Atom var, form]) = 
    eval env form >>= setVar env var


-- This is the function to declare a standard Variable
eval env (List [Atom "def", Atom var, form]) = 
    eval env form >>= defineVar env var

-- Function that is used to declare a function using a regular list -- ex: (defn x (arg1 arg2))
eval env (List (Atom "defn" : Atom var : List (params) : body)) =
    makeNormalFunc env params body >>= defineVar env var

-- Declare a function, but with cons -- ex: (defn x (arg1 . arg2))
eval env (List (Atom "defn" : Atom var : DottedList params varargs : body)) =
    makeVarargs varargs env params body  >>= defineVar env var

-- Lambda function using a regular list -- ex: (use (x y) (+ x y) 3 4)
eval env (List (Atom "use" : List params : body)) =
    makeNormalFunc env params body

-- Lambda function using a dotted list -- ex: (use (x . y) (+ x y) 3 4)
eval env (List (Atom "use" : DottedList params varargs : body)) =
    makeVarargs varargs env params body

-- Lambda function using one parameter without a list (use x y (+ x y) 3 4)
eval env (List (Atom "use" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body

-- Load a file with any extension, as long as it has correct grammar 
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)

-- This is used when a function is defined, but no name is given
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

-- This evaluates when the function/expression has has an unrecognized special form (like in functions like cond or case)
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- Evaluator Helpers

-- Used to evaluate the expressions of `case`
evalCaseCases :: Env -> Values -> [Values] -> IOThrowsError Values
evalCaseCases _ _ [] = throwError ExpectCaseClauses
evalCaseCases env _ [List (Atom "else" : cExprs)] = evalToLast env cExprs
evalCaseCases env key ((List ((List cKeys) : cExprs)) : cs) = do
    let result = any anyOf $ map (\x -> eqv [key, x]) cKeys
    case result of
        False -> evalCaseCases env key cs
        True -> evalToLast env cExprs
  where
    anyOf (Right (Bool True)) = True
    anyOf _ = False
evalCaseCases _ _ _ = throwError ExpectCaseClauses


-- Used to recursively evaluate a function (like cond or case, where there's multiple expressions) -- evaluate the last part of the function
evalToLast :: Env -> [Values] -> IOThrowsError Values
evalToLast _ [] = throwError $ NumArgs 1 []
evalToLast env xs = liftM last $ mapM (eval env) xs


-- Get a defined variable, or throw an error if it's getting an unknown/undefined variable
getVar :: Env -> String -> IOThrowsError Values
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)


-- Set a variable (through `def`), or throw an error if it's setting a value to an unknown/undefined variable
setVar :: Env -> String -> Values -> IOThrowsError Values
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value))
          (lookup var env)
    return value


-- Simply define a variable
defineVar :: Env -> String -> Values -> IOThrowsError Values
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var value >> return value
       else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value


-- Function definition for `apply`, the faux `map`
applyProc :: [Values] -> IOThrowsError Values
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args


-- Kind of like `apply`, it binds values/variables to symbols
bindVars :: Env -> [(String, Values)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
            ref <- newIORef value
            return (var, ref)


-- A test that checks if a variable is associated to a value, and returns true/false depending on that test
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var


-- Underlying `apply` to the BuBBLE function `apply` - apply-ception!
apply :: Values -> [Values] -> IOThrowsError Values
apply (PrimitiveFunc func) args = liftThrows $ func args

apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>=
            bindVarArgs varargs >>=
            evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env

apply (IOFunc func) args = func args


-- When the REPL starts up, it can either go into a REPL, or can evaluate a file one time, which is runOne 
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
      >>= hPutStrLn stderr


-- REPL config

-- The function that starts the REPL -- "the keys to the car"
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== ":q") (readPrompt "Bubble> ") . evalAndPrint


-- Evaluates a "string", or the input of the user interpreted as a string, then the REPL dissects it and changes the typing(s) it accordingly 
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env


-- A step up from evalString, it takes the input, changes it, and prints the end result out to the user and the REPL
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn