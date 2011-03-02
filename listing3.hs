import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

-- Simple Parsers

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do 
            char '"'
            x <- many ( (char '\\' >> oneOf "\\n\"rt") <|> noneOf "\"" )
            char '"'
            return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
          first <- letter <|> symbol
          rest <- many (letter <|> digit <|> symbol)
          let atom = first:rest
          return $ case atom of
              "#t" -> Bool True
              "#f" -> Bool False
              _    -> Atom atom

recbin :: (Integral a) => a -> String -> a
recbin n [] = n
recbin n (x:l) = case x of
            '0' -> recbin (n*2) l
            '1' -> recbin (n*2 + 1) l

readBin :: (Integral a) => String -> a
readBin = recbin 0

parseSimpleNumber :: Parser LispVal
parseSimpleNumber = do
                x <- many1 digit
                return $ Number $ read x

parseSpecificNumber :: Parser LispVal
parseSpecificNumber = do
                prefix <- char '#'
                base   <- oneOf "bodx"
                x      <- many1 digit
                return $ case base of
                    'd' -> Number $ read x
                    'b' -> Number $ readBin x
                    'o' -> Number $ fst ( head ( readOct x ) )
                    'x' -> Number $ fst ( head ( readHex x ) )

parseNumber :: Parser LispVal
parseNumber = parseSpecificNumber <|> parseSimpleNumber

-- TODO
--
-- 5 Add a Character constructor to LispVal, and create a parser for character literals as described in R5RS. 
-- [http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.4]
--
-- 6 Add a Float constructor to LispVal, and support R5RS syntax for decimals. The Haskell function readFloat may be useful.
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.4
-- http://www.haskell.org/onlinereport/numeric.html#sect14
--
-- # Add data types and parsers to support the full numeric tower of Scheme numeric types. Haskell has built-in types to represent many of these; check the Prelude. For the others, you can define compound types that represent eg. a Rational as a numerator and denominator, or a Complex as a real and imaginary part (each itself a Real number).
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.2.1
-- http://www.haskell.org/onlinereport/standard-prelude.html#$tNum

-- Recursive Parser: Adding lists, dotted lists, and quoted datums

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
                char '\''
                x <- parseExpr
                return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do 
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

-- Main

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
    Left  err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do 
        args <- getArgs
        putStrLn (readExpr (args !! 0))

