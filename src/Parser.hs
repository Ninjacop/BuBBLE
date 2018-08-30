module Parser where

import Types

import Control.Monad (liftM)
import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Error
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Concurrent (threadDelay)

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

-- Reads the input, parses it, then depending on if the syntax is A-OK, it throws an error
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "BuBBLE" input of
    Left err -> throwError $ Parser err
    Right val -> return val


-- Simply reads and expression that can throw errors and ends by parsing a space
readExpr :: String -> ThrowsError Values
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)


-- Value Parsing

-- I'm not going to go through all of them individually, because that's redundant, but just know that they all parse a particular type -- ex: parseString
parseExpr :: Parser Values
parseExpr = do 
          skipMany (comments <|> spaces)
          parseAtom
          <|> parseString
          {- <|> parseVector --  Uncomment this and other functions related to Vectors to enable them -- STILL IN TESTING -}
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


comments :: Parser ()
comments = skipMany1 (blockComment <|> inlineComment)


inlineComment :: Parser String
inlineComment = try $ string ";" >> manyTill anyChar (try newline)


blockComment :: Parser String
blockComment = try $ string ":=" >> manyTill (anyChar <|> newline) (try (string "=:"))


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


-- Uncomment this and other functions related to Vectors to enable them -- STILL IN TESTING
{- parseVector :: Parser Values
parseVector = do string "#("
                 elems <- sepBy parseExpr spaces
                 char ')'
                 return $ Vector (listArray (0, (length elems)-1) elems) -}


-- Misc Helpers 

-- The ubiquitous escaped characters that act the same across programming languages that use them
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


-- Null environment variable
nullEnv :: IO Env
nullEnv = newIORef []



-- Boolean Testing
-- Unary primitive defs all have type
-- Not going over them one-by-one, but they all have to do with comparision -- ex: `boolP` can be re-written as `bool?`

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


