{-# LANGUAGE ExistentialQuantification #-}
module Parser where

-- Errors
--
-- (if (= 1 2)) returns: "If is an Unbound var"
-- 
-- Vector is unsupported


-- Import Monads n' Stuff
import Control.Monad (liftM)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Error
import Control.Monad.Fix
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
import System.Console.ANSI
import System.Random
import System.IO 





-- Datatypes & Errors
data Values = Atom String
             | List [Values]
             | DottedList [Values] Values
             | Number Integer
             | Ratio Rational
             | Float Double
             | Complex (Complex Double)
             | String String
             | Char Char
             | Bool Bool
             | Vector (Array Int Values)
             | PrimitiveFunc ([Values] -> ThrowsError Values)
             | Func { params :: [String]
                    , vararg :: (Maybe String)
                    , body :: [Values]
                    , closure :: Env
                    }
             | IOFunc ([Values] -> IOThrowsError Values)
             | Port Handle


instance Show Values where show = showVal


data BubbleError = NumArgs Integer [Values]
               | ExpectCondClauses
               | ExpectCaseClauses
               | TypeMismatch String Values
               | Parser ParseError
               | BadSpecialForm String Values
               | NotFunction String String
               | UnboundVar String String
               | Default String


instance Show BubbleError where show = showError


instance Error BubbleError where
    noMsg = Default "An unknown error has occurred"
    strMsg = Default


type ThrowsError = Either BubbleError
type Env = IORef [(String, IORef Values)]
type IOThrowsError = ErrorT BubbleError IO



data Unpacker = forall a. Eq a => AnyUnpacker (Values -> ThrowsError a)



-- Parsing
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "BuBBLE" input of
    Left err -> throwError $ Parser err
    Right val -> return val


readExpr :: String -> ThrowsError Values
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)


runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
      >>= hPutStrLn stderr


-- REPL config
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== ":q") (readPrompt "Bubble> ") . evalAndPrint



-- Value Parsing
parseExpr :: Parser Values
parseExpr = parseAtom
          <|> parseString
          <|> try parseChar
          <|> try parseComplex
          <|> try parseFloat
          <|> try parseRatio
          <|> try parseNumber
          <|> parseBool
          <|> parseQuoted
          <|> parseQuasiquote
          <|> try parseUnquoteSplicing
          <|> parseUnquote
          <|> parseList
          <|> parseVector


parseAtom :: Parser Values
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               (return . Atom) (first:rest)


parseList :: Parser Values
parseList = char '(' >> parseList1


parseList1 :: Parser Values
parseList1 = (char ')' >> (return . List) []) 
               <|> do expr <- parseExpr
                      parseList2 [expr]


parseList2 :: [Values] -> Parser Values
parseList2 expr = (char ')' >> (return . List) (reverse expr)) 
                    <|> (spaces >> parseList3 expr)


parseList3 :: [Values] -> Parser Values
parseList3 expr = do char '.' >> spaces
                     dotted <- parseExpr
                     char ')'
                     return $ DottedList expr dotted
                  <|> do next <- parseExpr
                         parseList2 (next:expr)


parseQuoted :: Parser Values
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]


parseNumber :: Parser Values
parseNumber = parsePlainNumber <|> parseRadixNumber


parsePlainNumber :: Parser Values
parsePlainNumber = many1 digit >>= return . Number . read


parseRadixNumber :: Parser Values
parseRadixNumber = char '#' >> 
                   (
                        parseDecimal 
                        <|> parseBinary
                        <|> parseOctal
                        <|> parseHex
                   )


parseDecimal :: Parser Values
parseDecimal = do char 'd'
                  n <- many1 digit
                  (return . Number . read) n


parseBinary :: Parser Values
parseBinary = do char 'b'
                 n <- many $ oneOf "01"
                 (return . Number . bin2int) n


parseOctal :: Parser Values
parseOctal = do char 'o'
                n <- many $ oneOf "01234567"
                (return . Number . (readWith readOct)) n


parseHex :: Parser Values
parseHex = do char 'x'
              n <- many $ oneOf "0123456789abcdefABCDEF"
              (return . Number . (readWith readHex)) n


parseRatio :: Parser Values
parseRatio = do num <- fmap read $ many1 digit
                char '/'
                denom <- fmap read $ many1 digit
                (return . Ratio) (num % denom)


parseFloat :: Parser Values
parseFloat = do whole <- many1 digit
                char '.'
                decimal <- many1 digit
                return $ Float (read (whole++"."++decimal))


parseComplex :: Parser Values
parseComplex = do r <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char '+'
                  i <- fmap toDouble (try parseFloat <|> parsePlainNumber)
                  char 'i'
                  (return . Complex) (r :+ i)
               where toDouble (Float x) = x
                     toDouble (Number x) = fromIntegral x


parseString :: Parser Values
parseString = do char '"'
                 s <- many (escapedChars <|> (noneOf ['\\', '"']))
                 char '"'
                 (return . String) s


parseChar :: Parser Values
parseChar = do string "#\\"
               s <- many1 letter
               return $ case (map toLower s) of
                      "space" -> Char ' '
                      "newline" -> Char '\n'
                      [x] -> Char x


parseBool :: Parser Values
parseBool = do char '#'
               c <- oneOf "tf"
               return $ case c of
                      't' -> Bool True
                      'f' -> Bool False


parseQuasiquote :: Parser Values
parseQuasiquote = do char '`'
                     expr <- parseExpr
                     return $ List [Atom "quasiquote", expr]


-- Bug: this allows the unquote to appear outside of a quasiquoted list
parseUnquote :: Parser Values
parseUnquote = do char ','
                  expr <- parseExpr
                  return $ List [Atom "unquote", expr]


-- Bug: this allows unquote-splicing to appear outside of a quasiquoted list
parseUnquoteSplicing :: Parser Values
parseUnquoteSplicing = do string ",@"
                          expr <- parseExpr
                          return $ List [Atom "unquote-splicing", expr]


parseVector :: Parser Values
parseVector = do string "{"
                 elems <- sepBy parseExpr spaces
                 char '}'
                 return $ Vector (listArray (0, (length elems)-1) elems)


flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
       then return ()
       else action result >> until_ pred prompt action


-- Misc Helpers
escapedChars :: Parser Char
escapedChars = do
             char '\\'
             c <- oneOf ['\\','"', 'n', 'r', 't']
             return $ case c of
                    '\\' -> c
                    '"'  -> c
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space



-- Error Handling
showError :: BubbleError -> String
showError ExpectCondClauses = "Expect at least 1 true cond clause"
showError ExpectCaseClauses = "Expect at least 1 true case clause"
showError (UnboundVar varname msg) = msg ++ " is an unbound variable: define it by typing (def " ++ msg ++ " value)" ++ " (Error: " ++ varname ++ ") "
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction func msg) = func ++ " is not a function " 


showError (NumArgs expected found) = "Current function should have " ++ show expected
                                   ++ " args; found " ++ unwordsList found 


showError (TypeMismatch expected found) = "Invalid type:  " ++ show found 
                                   ++ ", it should be a " ++ expected 


showError (Parser parseErr) = "Parser error at " ++ show parseErr

-- Show functions
showVal :: Values -> String
-- Use the first string showVal if stuff gets in the way
-- showVal (String s) = "\"" ++ s ++ "\""
showVal (String s) = s
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "true"
showVal (Bool False) = "false"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
--showVal (Vector v) = "{" ++ unwordsList v ++ "}"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Char c) = ['\'', c, '\'']
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Float f) = show f
showVal (Complex i) = show i 
showVal (Ratio r) = show r


-- This is what prints after 
showVal (Func {params=args, vararg=varargs, body=body, closure=env}) =
    "(lambda (" ++ unwords (map show args) ++
      (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"


showVal (Port _) = "<port>"
showVal (IOFunc _) = "<primitive>"


unwordsList :: [Values] -> String
unwordsList = unwords . map showVal


nullEnv :: IO Env
nullEnv = newIORef []


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var


extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- Evaluator
eval :: Env -> Values -> IOThrowsError Values
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Float _) = return val
eval env val@(Complex _) = return val
eval env val@(Ratio _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val

-- if statement
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        otherwise  -> throwError $ TypeMismatch "boolean" result


eval env (List (Atom "cond" : [])) = throwError ExpectCondClauses
eval env (List (Atom "cond" : cs)) = evalConds env cs
eval env (List (Atom "case" : [])) = throwError ExpectCaseClauses



eval env (List (Atom "case" : key : cs)) = do
    keyVal <- eval env key
    evalCaseCases env keyVal cs


-- This is like swap! in clojure
eval env (List [Atom "set!", Atom var, form]) = 
    eval env form >>= setVar env var

-- Variable
eval env (List [Atom "def", Atom var, form]) = 
    eval env form >>= defineVar env var

-- Function
eval env (List (Atom "defn" : Atom var : List (params) : body)) =
    makeNormalFunc env params body >>= defineVar env var

-- Function, but with cons
eval env (List (Atom "defn" : Atom var : DottedList params varargs : body)) =
    makeVarargs varargs env params body  >>= defineVar env var

-- Lambda function
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body


eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body


eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body


eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)


eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals


eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- Evaluator Helpers
evalConds :: Env -> [Values] -> IOThrowsError Values
evalConds env (List (Atom "else" : xs) : []) = evalCondElse env xs
evalConds _ [] = throwError ExpectCondClauses
evalConds env (List clause : cs) = evalCondClause env clause cs
evalConds _ badClauses = throwError $ TypeMismatch "cond clauses" $ List badClauses


evalCondClause env (test : xs) rest = do
    result <- eval env test
    case test of
         Bool False -> evalConds env rest
         Bool True -> trueDo xs
         otherwise -> throwError $ TypeMismatch "boolean" result
  where 
    trueDo [] = return $ Bool True
    trueDo xs = evalToLast env xs


evalCondElse :: Env -> [Values] -> IOThrowsError Values
evalCondElse _ [] = throwError ExpectCondClauses
evalCondElse env xs = evalToLast env xs


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


evalToLast :: Env -> [Values] -> IOThrowsError Values
evalToLast _ [] = throwError $ NumArgs 1 []
evalToLast env xs = liftM last $ mapM (eval env) xs


getVar :: Env -> String -> IOThrowsError Values
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)


setVar :: Env -> String -> Values -> IOThrowsError Values
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value))
          (lookup var env)
    return value


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


bindVars :: Env -> [(String, Values)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
            ref <- newIORef value
            return (var, ref)


-- Boolean Testing
-- Unary primitive defs all have type
-- Values -> Values
not' (Bool x) = (Bool . not) x
not' _ = Bool False


boolP (Bool _) = Bool True
boolP _ = Bool False


listP (List _) = Bool True
listP (DottedList _ _) = Bool True
listP _ = Bool False


symbolP (Atom _) = Bool True
symbolP _ = Bool False


charP (Char _) = Bool True
charP _ = Bool False


stringP (String _) = Bool True
stringP _ = Bool False


vectorP (Vector _) = Bool True
vectorP _ = Bool False


symbol2string (Atom s) = String s
symbol2string _ = error "Expecting an Atom"


string2symbol (String s) = Atom s
string2symbol _ = error "Expecting a String"

trapError action = catchError action (return . show)


-- Primitive functions lookup table
--
primitives :: [(String, [Values] -> ThrowsError Values)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("gcd", numericBinop gcd)
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quot", numericBinop quot)
             ,("rem", numericBinop rem)
             ,("max", numericBinop max)
             ,("min", numericBinop min)
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
             ,("string-ci?eq", strBoolBinop (ci_help (==)))
             ,("string-ci?lt", strBoolBinop (ci_help (<)))
             ,("string-ci?gt", strBoolBinop (ci_help (>)))
             ,("string-ci?lte", strBoolBinop (ci_help (<=)))
             ,("string-ci?gte", strBoolBinop (ci_help (>=)))
             ,("not", unaryOp not')
             ,("boolean?", unaryOp boolP)
             ,("list?", unaryOp listP)
             ,("symbol?", unaryOp symbolP)
             ,("char?", unaryOp charP)
             ,("string?", unaryOp stringP)
             ,("vector?", unaryOp vectorP)
             ,("symbol2string", unaryOp symbol2string)
             ,("string2symbol", unaryOp string2symbol)
             ,("car", car)
             ,("cdr", cdr)
             ,("cons", cons)
             ,("eq", eqv)
             ,("equal", equal)
             ,("coll", make_string)
             ,("string", create_string)
             ,("length", string_length)
             ,("find", char_at)
             ,("substring", substring)
             ,("concat", string_append)
             ,("print", princ)
             ]


-- IO Primitives
--
ioPrimitives :: [(String, [Values] -> IOThrowsError Values)]
ioPrimitives = [("apply", applyProc)
               ,("openinput", makePort ReadMode)
               ,("openoutput", makePort WriteMode)
               ,("closeinput", closePort)
               ,("closeoutput", closePort)
               ,("read", readProc)
               ,("write", writeProc)
               ,("readcontents", readContents)
               ,("readall", readAll)
               ]



-- IO Primitive helpers
--
applyProc :: [Values] -> IOThrowsError Values
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [Values] -> IOThrowsError Values
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [Values] -> IOThrowsError Values
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [Values] -> IOThrowsError Values
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [Values] -> IOThrowsError Values
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [Values] -> IOThrowsError Values
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [Values]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [Values] -> IOThrowsError Values
readAll [String filename] = liftM List $ load filename


ci_help :: (String -> String -> Bool) -> String -> String -> Bool
ci_help f a b = f (map toLower a) (map toLower b)

princ :: [Values] -> ThrowsError Values
princ [Number x] = return $ Number x 
princ [Float x] = return $ Float x
princ [Ratio x] = return $ Ratio x 
princ [Complex x] = return $ Complex x
princ [String x] = return $ String x
princ [List x] = return $ List x
princ [DottedList (x) y] = return $ (DottedList x y)


car :: [Values] -> ThrowsError Values
car [List (x:xs)] = return x 
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [Values] -> ThrowsError Values
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [x] y] = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [Values] -> ThrowsError Values
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs last] = return $ DottedList (x:xs) last
cons [x,y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

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


--
-- String primitives
--

make_string :: [Values] -> ThrowsError Values
make_string [Number k, Char c] = return $ String $ replicate (fromIntegral k)  c
make_string badArgs = throwError $ TypeMismatch "int char" $ List badArgs

create_string :: [Values] -> ThrowsError Values
create_string xs
    | all isChar xs = return $ String $ foldr f "" xs 
    | otherwise = throwError $ TypeMismatch "list of chars" $ List xs
  where
    isChar (Char _) = True
    isChar _ = False
    f (Char c) accum = c : accum

string_length :: [Values] -> ThrowsError Values
string_length [String s] = (return . Number . fromIntegral . length) s
string_length badArgs = throwError $ TypeMismatch "string" $ List badArgs

char_at :: [Values] -> ThrowsError Values
char_at [String s, Number n] = (return . Char) (s !! (fromIntegral n))
char_at badArgs = throwError $ TypeMismatch "(string number)" $ List badArgs

substring :: [Values] -> ThrowsError Values
substring [String s, Number start, Number end] =
    let start' = fromIntegral start
        end' = fromIntegral end
    in  (return . String) (drop start' $ take end' $ s)
substring badArgs = throwError $ TypeMismatch "(string number number)" $ List badArgs

string_append :: [Values] -> ThrowsError Values
string_append ss
    | all isString ss = (return . String . concat) $ map (\(String s) -> s) ss
    | otherwise = throwError $ TypeMismatch "list of string" $ List ss
  where
    isString (String _) = True
    isString _ = False 

--
-- Primitive helpers
--

numericBinop :: (Integer -> Integer -> Integer) 
             -> [Values] 
             -> ThrowsError Values
numericBinop op single@[_] = throwError $ NumArgs 2 single
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool
charBoolBinop = boolBinop unpackChar

unaryOp :: (Values -> Values) 
        -> [Values] 
        -> ThrowsError Values
unaryOp func [arg] = return $ func arg

boolBinop :: (Values -> ThrowsError a) 
          -> (a -> a -> Bool) 
          -> [Values] 
          -> ThrowsError Values
boolBinop unpacker op [x,y] = do
    left <- unpacker x
    right <- unpacker y
    return $ Bool $ left `op` right
boolBinop _ _ args = throwError $ NumArgs 2 args

unpackNum :: Values -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: Values -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: Values -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackChar :: Values -> ThrowsError Char
unpackChar (Char c) = return c 
unpackChar notChar = throwError $ TypeMismatch "char" notChar

unpackEquals :: Values -> Values -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
  `catchError` (const $ return False)


bin2int :: String -> Integer
bin2int s = sum $ map (\(i,x) -> i*(2^x)) $ zip [0..] $ map p (reverse s)
          where p '0' = 0
                p '1' = 1

readWith :: (t -> [(a, b)]) -> t -> a
readWith f s = fst $ f s !! 0


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



primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal
