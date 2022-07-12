--
-- EPITECH PROJECT, 2020
-- B-FUN-501-RUN-5-1-HAL-abdillah-kael.madi
-- File description:
-- Main
--

{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Char
import Control.Applicative
import Data.Either
import System.Environment
import System.Exit

-- Parser --

data Error = Error String deriving (Show)

instance Alternative (Either Error) where
    empty = Left $ Error "empty"
    Left _ <|> e2 = e2
    e1 <|> _ = e1

newtype Parser a = Parser { runParser :: String -> Either Error (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (x, input') <- p input
        Right (f x, input')

instance Applicative Parser where
    pure x = Parser $ \input -> Right (x, input)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (f, input')  <- p1 input
        (a, input'') <- p2 input'
        Right (f a, input'')

instance Alternative Parser where
    empty = Parser $ \_ -> empty
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input -> case input of
    (x:xs) | x == c -> Right (x, xs)
    (x:_)           -> Left $ Error ("Could not parse char: expected '" ++ [c] ++ "' but got '" ++ [x] ++ "'")
    []              -> Left $ Error ("Could not parse char: expected '" ++ [c] ++ "' but got ''")

parseString :: String -> Parser String
parseString = sequenceA . map parseChar

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser $ \input -> Right (span f input)

notNull :: String -> Parser [a] -> Parser [a]
notNull error (Parser p) = Parser $ \input -> do
                         (x, input') <- p input
                         if null x
                             then Left $ Error error
                             else Right (x, input')

parseNumber :: Parser Integer
parseNumber = f <$> notNull error (parseSpan isDigit)
            where f ds  = read ds
                  error = "Could not parse number"

ws :: Parser String
ws = parseSpan isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

takeSepBy :: Integer -> Parser a -> Parser b -> Parser [b]
takeSepBy 0 _ _ = pure []
takeSepBy count sep element = (:) <$> element <*> (sep *> takeSepBy (count - 1) sep element)

-- Lisp --

data LispValue = LispNumber Integer
               | LispString String
               | LispQuote  String
               | LispList   [LispValue]
               | LispOpAdd  [LispValue]
               | LispOpSub  [LispValue]
               | LispOpMul  [LispValue]
               | LispMul    [LispValue]
               | LispOpDiv  [LispValue]
               | LispDiv    [LispValue]
               | LispCons   [LispValue]
               | LispCar    LispValue
               | LispCdr    LispValue
               | LispEq     [LispValue]
               | LispLower  [LispValue]
               deriving (Show, Eq)

lispNumber :: Parser LispValue
lispNumber = LispNumber <$> parseNumber

lispString :: Parser LispValue
lispString = LispString <$> (parseChar '"' *> parseSpan (/= '"') <* parseChar '"')

lispArgument :: Parser LispValue
lispArgument = lispValue <|> lispNumber <|> lispString <|> lispQuote'

lispList :: Parser LispValue
lispList = LispList <$> (parseChar '(' *> ws *> parseString "list" *> ws *> elements <* ws <* parseChar ')')
         where elements = sepBy ws lispArgument

lispQuote'' :: Parser String
lispQuote'' = (parseChar '(' *> ws *> parseSpan f <* ws <* parseChar ')') <|> parseSpan f
            where f = \c -> if isSpace c || c == ')'
                    then False
                    else True

lispQuote' :: Parser LispValue
lispQuote' = LispQuote <$> (f *> ws *> ((parseChar '(' *> ws *> parseSpan (/= ')') <* ws <* parseChar ')') <|> lispQuote''))
           where f = parseChar '`' <|> parseChar '\''

lispQuote :: Parser LispValue
lispQuote = LispQuote <$> (parseChar '(' *> ws *> parseString "quote" *> ws *> f <* ws <* parseChar ')')
          where f  = (parseChar '(' *> ws *> f' <* ws <* parseChar ')') <|> lispQuote''
                f' = parseSpan (/= ')')

lispCons :: Parser LispValue
lispCons = LispCons <$> (parseChar '(' *> ws *> parseString "cons" *> ws *> elements <* ws <* parseChar ')')
         where elements = takeSepBy 2 ws lispArgument

lispCar :: Parser LispValue
lispCar = LispCar <$> (parseChar '(' *> ws *> parseString "car" *> ws *> lispCons <* ws <* parseChar ')')

lispCdr :: Parser LispValue
lispCdr = LispCdr <$> (parseChar '(' *> ws *> parseString "cdr" *> ws *> elements <* ws <* parseChar ')')
        where elements = lispCons <|> lispQuote <|> lispQuote'

lispEq :: Parser LispValue
lispEq = LispEq <$> (parseChar '(' *> ws *> parseString "eq?" *> ws *> elements <* ws <* parseChar ')')
       where elements = takeSepBy 2 ws lispArgument

lispLower :: Parser LispValue
lispLower = LispLower <$> (parseChar '(' *> ws *> parseChar '<' *> ws *> elements <* ws <* parseChar ')')
          where elements = takeSepBy 2 ws lispArgument

lispOpAdd :: Parser LispValue
lispOpAdd = LispOpAdd <$> (parseChar '(' *> ws *> parseChar '+' *> ws *> elements <* ws <* parseChar ')')
          where elements = sepBy ws lispNumber

lispOpSub :: Parser LispValue
lispOpSub = LispOpSub <$> (parseChar '(' *> ws *> parseChar '-' *> ws *> elements <* ws <* parseChar ')')
          where elements = sepBy ws lispNumber

lispOpMul :: Parser LispValue
lispOpMul = LispOpMul <$> (parseChar '(' *> ws *> parseChar '*' *> ws *> elements <* ws <* parseChar ')')
          where elements = sepBy ws lispNumber

lispMul :: Parser LispValue
lispMul = LispMul <$> (parseChar '(' *> ws *> parseString "mul" *> ws *> elements <* ws <* parseChar ')')
        where elements = takeSepBy 2 ws lispNumber

lispOpDiv :: Parser LispValue
lispOpDiv = LispOpDiv <$> (parseChar '(' *> ws *> parseChar '/' *> ws *> elements <* ws <* parseChar ')')
          where elements = sepBy ws lispNumber

lispDiv :: Parser LispValue
lispDiv = LispDiv <$> (parseChar '(' *> ws *> parseString "div" *> ws *> elements <* ws <* parseChar ')')
        where elements = takeSepBy 2 ws lispNumber

lispOperator :: Parser LispValue
lispOperator = lispOpAdd <|> lispOpSub <|> lispOpMul <|> lispOpDiv <|> lispMul <|> lispDiv

lispValue :: Parser LispValue
lispValue = lispList <|> lispQuote <|> lispQuote'   <|> lispOperator <|> lispCons <|>
            lispCar  <|> lispCdr   <|> lispEq       <|> lispLower

-- Interpreter --

type Interpreter a = a -> String

lispDisplay :: Interpreter LispValue
lispDisplay (LispNumber input)                 = show input
lispDisplay (LispString input)                 = show input
lispDisplay (LispQuote  [])                    = "()"
lispDisplay (LispQuote  input)                 = input
lispDisplay (LispCons   (x@(LispList _):y:[])) = lispListI x ++ " . " ++ lispDisplay y
lispDisplay (LispCons   (x:y@(LispList _):[])) = lispDisplay x ++ [' '] ++ lispListI' y
lispDisplay (LispCons   (x:y@(LispCons _):[])) = lispDisplay x ++ [' '] ++ lispDisplay y
lispDisplay (LispCons   (x:(LispQuote []):[])) = lispDisplay x
lispDisplay (LispCons   (x:(LispQuote y):[]))  = lispDisplay x ++ f
                                               where f = if elem ' ' y
                                                        then [' '] ++ y
                                                        else " . " ++ y
lispDisplay (LispCons   (x:y:[]))              = lispDisplay x ++ " . " ++ lispDisplay y

lispOpAddI :: Interpreter LispValue
lispOpAddI (LispOpAdd (x:[])) = lispDisplay x
lispOpAddI (LispOpAdd (x:xs)) = show $ (+) element (read $ lispOpAddI rest)
                              where (LispNumber element) = x
                                    rest                 = LispOpAdd xs

lispOpSubI :: Interpreter LispValue
lispOpSubI (LispOpSub (x:[])) = lispDisplay x
lispOpSubI (LispOpSub (x:xs)) = show $ (-) element (read $ lispOpSubI rest)
                              where (LispNumber element) = x
                                    rest                 = LispOpSub xs

lispOpMulI :: Interpreter LispValue
lispOpMulI (LispOpMul (x:[])) = lispDisplay x
lispOpMulI (LispOpMul (x:xs)) = show $ (*) element (read $ lispOpMulI rest)
                              where (LispNumber element) = x
                                    rest                 = LispOpMul xs

lispMulI :: Interpreter LispValue
lispMulI (LispMul ((LispNumber x):(LispNumber y):[])) = show $ (*) x y

lispOpDivI :: Interpreter LispValue
lispOpDivI (LispOpDiv (x:[])) = lispDisplay x
lispOpDivI (LispOpDiv (x:xs)) = show $ element `div` (read $ lispOpDivI rest)
                              where (LispNumber element) = x
                                    rest                 = LispOpDiv xs

lispDivI :: Interpreter LispValue
lispDivI (LispDiv ((LispNumber x):(LispNumber y):[])) = show $ x `div` y

lispListI' :: Interpreter LispValue
lispListI' (LispList (x:[])) = lispDisplay x
lispListI' (LispList (x:xs)) = lispDisplay x ++ [' '] ++ lispListI' rest
                             where rest = LispList xs

lispListI :: Interpreter LispValue
lispListI input = ['('] ++ lispListI' input ++ [')']

lispQuoteI :: Interpreter LispValue
lispQuoteI input@(LispQuote x) = case elem ' ' x of
    True  -> ['('] ++ lispDisplay input ++ [')']
    False -> lispDisplay input

lispConsI :: Interpreter LispValue
lispConsI input@(LispCons _) = ['('] ++ lispDisplay input ++ [')']

lispCarI :: Interpreter LispValue
lispCarI (LispCar (LispCons (x:_:[]))) = lispDisplay x
lispCarI (LispCar (LispQuote x))       = head $ words x

lispCdrI' :: [String] -> String
lispCdrI' []     = []
lispCdrI' (x:[]) = x
lispCdrI' (x:xs) = x ++ [' '] ++ lispCdrI' xs

lispCdrI :: Interpreter LispValue
lispCdrI (LispCdr (LispCons (_:y:[]))) = lispDisplay y
lispCdrI (LispCdr (LispQuote x))       = ['('] ++ (lispCdrI' $ tail $ words x) ++ [')']

lispEqI :: Interpreter LispValue
lispEqI (LispEq (x:y:[])) | lispValueI x == lispValueI y = "#t"
                          | otherwise                    = "#f"

lispLowerI :: Interpreter LispValue
lispLowerI (LispLower (x:y:[])) | lispValueI x < lispValueI y = "#t"
                                | otherwise                   = "#f"

lispValueI :: Interpreter LispValue
lispValueI input@(LispOpAdd _) = lispOpAddI input
lispValueI input@(LispOpSub _) = lispOpSubI input
lispValueI input@(LispOpMul _) = lispOpMulI input
lispValueI input@(LispMul _)   = lispMulI   input
lispValueI input@(LispOpDiv _) = lispOpDivI input
lispValueI input@(LispDiv _)   = lispDivI   input
lispValueI input@(LispList _)  = lispListI  input
lispValueI input@(LispQuote _) = lispQuoteI input
lispValueI input@(LispCons _)  = lispConsI  input
lispValueI input@(LispCar _)   = lispCarI   input
lispValueI input@(LispCdr _)   = lispCdrI   input
lispValueI input@(LispEq _)    = lispEqI    input
lispValueI input@(LispLower _) = lispLowerI input
lispValueI (LispNumber input)  = show input
lispValueI (LispString input)  = input

-- Main --

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

displayError :: Error -> IO ()
displayError err = putStrLn $ id $ show err

displayInterpretation :: Either Error String -> IO ()
displayInterpretation (Left err) = displayError err >>
                                   exitWith (ExitFailure 84)
displayInterpretation (Right x)  = putStrLn $ id x

buildTreeAndInterprete :: [String] -> IO ()
buildTreeAndInterprete []     = return ()
buildTreeAndInterprete (x:xs) = displayInterpretation (lispValueI <$> fst <$> runParser lispValue x) >>
                                buildTreeAndInterprete xs

run :: [String] -> IO ()
run []     = return ()
run (x:xs) = do
    input <- readLines x
    buildTreeAndInterprete input
    run xs

main :: IO ()
main = do
    args <- getArgs
    run args

-- Test

parseCharTest :: Bool
parseCharTest = if f == 'a'
                    then True
                    else error $ "got: " ++ [f]
               where (Right f) = fst <$> runParser (parseChar 'a') "abc"

parseStringTest :: Bool
parseStringTest = if f == "hello"
                    then True
                    else error $ "got: " ++ f
                where (Right f) = fst <$> runParser (parseString "hello") "hello, world!"

lispNumberTest :: Bool
lispNumberTest = if f == "42"
                    then True
                    else error $ "got: " ++ f
               where (Right f) = lispValueI <$> fst <$> runParser lispNumber "42 + 21"

lispStringTest :: Bool
lispStringTest = if f == "hello"
                    then True
                    else error $ "got: " ++ f
               where (Right f) = lispValueI <$> fst <$> runParser lispString "\"hello\" \"world\""

lispQuoteTest1 :: Bool
lispQuoteTest1 = if f == "toto"
                    then True
                    else error $ "got: " ++ f
               where (Right f) = lispValueI <$> fst <$> runParser lispQuote "(quote toto)"

lispQuoteTest2 :: Bool
lispQuoteTest2 = if f == "(+ 1 2)"
                    then True
                    else error $ "got: " ++ f
               where (Right f) = lispValueI <$> fst <$> runParser lispQuote "(quote (+ 1 2))"

lispQuoteTest3 :: Bool
lispQuoteTest3 = if f == "()"
                    then True
                    else error $ "got: " ++ f
               where (Right f) = lispValueI <$> fst <$> runParser lispQuote "(quote ())"

lispQuote'Test1 :: Bool
lispQuote'Test1 = if f == "toto"
                    then True
                    else error $ "got: " ++ f
               where (Right f) = lispValueI <$> fst <$> runParser lispQuote' "'toto"

lispQuote'Test2 :: Bool
lispQuote'Test2 = if f == "(+ 1 2)"
                    then True
                    else error $ "got: " ++ f
               where (Right f) = lispValueI <$> fst <$> runParser lispQuote' "'(+ 1 2)"

lispQuote'Test3 :: Bool
lispQuote'Test3 = if f == "()"
                    then True
                    else error $ "got: " ++ f
               where (Right f) = lispValueI <$> fst <$> runParser lispQuote' "'()"

lispConsTest1 :: Bool
lispConsTest1 = if f == "(1 . 2)"
                    then True
                    else error $ "got: " ++ f
              where (Right f) = lispValueI <$> fst <$> runParser lispCons "(cons 1 2)"

lispConsTest2 :: Bool
lispConsTest2 = if f == "(1 2 . 3)"
                    then True
                    else error $ "got: " ++ f
              where (Right f) = lispValueI <$> fst <$> runParser lispCons "(cons 1 (cons 2 3))"

lispConsTest3 :: Bool
lispConsTest3 = if f == "(1 2 3)"
                    then True
                    else error $ "got: " ++ f
              where (Right f) = lispValueI <$> fst <$> runParser lispCons "(cons 1 (cons 2 (cons 3 '())))"

lispCarTest :: Bool
lispCarTest = if f == "1"
                    then True
                    else error $ "got: " ++ f
            where (Right f) = lispValueI <$> fst <$> runParser lispCar "(car (cons 1 2))"

lispCdrTest1 :: Bool
lispCdrTest1 = if f == "2"
                    then True
                    else error $ "got: " ++ f
             where (Right f) = lispValueI <$> fst <$> runParser lispCdr "(cdr (cons 1 2))"

lispCdrTest2 :: Bool
lispCdrTest2 = if f == "(2 3)"
                    then True
                    else error $ "got: " ++ f
             where (Right f) = lispValueI <$> fst <$> runParser lispCdr "(cdr '(1 2 3))"

lispEqTest1 :: Bool
lispEqTest1 = if f == "#t"
                    then True
                    else error $ "got: " ++ f
            where (Right f) = lispValueI <$> fst <$> runParser lispEq "(eq? 1 1)"

lispEqTest2 :: Bool
lispEqTest2 = if f == "#t"
                    then True
                    else error $ "got: " ++ f
            where (Right f) = lispValueI <$> fst <$> runParser lispEq "(eq? (+ 1 1) 2)"

lispEqTest3 :: Bool
lispEqTest3 = if f == "#f"
                    then True
                    else error $ "got: " ++ f
            where (Right f) = lispValueI <$> fst <$> runParser lispEq "(eq? 'foo 'bar)"

lispEqTest4 :: Bool
lispEqTest4 = if f == "#t"
                    then True
                    else error $ "got: " ++ f
            where (Right f) = lispValueI <$> fst <$> runParser lispEq "(eq? '() '())"

test :: IO ()
test = (putStrLn $ "Test parseChar 'a' \"abc\" -> " ++ (show parseCharTest))                        >>
       (putStrLn $ "Test parseString \"hello\" \"hello, world!\" -> " ++ (show parseCharTest))      >>
       (putStrLn $ "Test lispNumber \"42 + 21\" -> " ++ (show lispNumberTest))                      >>
       (putStrLn $ "Test lispString \"\"hello\" \"world\"\" -> " ++ (show lispStringTest))          >>
       (putStrLn $ "Test lispQuote \"(quote toto)\" -> " ++ (show lispQuoteTest1))                  >>
       (putStrLn $ "Test lispQuote \"(quote (+ 1 2))\" -> " ++ (show lispQuoteTest2))               >>
       (putStrLn $ "Test lispQuote \"(quote ())\" -> " ++ (show lispQuoteTest3))                    >>
       (putStrLn $ "Test lispQuote' \"'toto\" -> " ++ (show lispQuote'Test1))                       >>
       (putStrLn $ "Test lispQuote' \"'(+ 1 2)\" -> " ++ (show lispQuote'Test2))                    >>
       (putStrLn $ "Test lispQuote' \"'()\" -> " ++ (show lispQuote'Test3))                         >>
       (putStrLn $ "Test lispCons \"(cons 1 2)\" -> " ++ (show lispConsTest1))                      >>
       (putStrLn $ "Test lispCons \"(cons 1 (cons 2 3))\" -> " ++ (show lispConsTest2))             >>
       (putStrLn $ "Test lispCons \"(cons 1 (cons 2 (cons 3 '())))\" -> " ++ (show lispConsTest3))  >>
       (putStrLn $ "Test lispCar \"(car (cons 1 2))\" -> " ++ (show lispCarTest))                   >>
       (putStrLn $ "Test lispCar \"(cdr (cons 1 2))\" -> " ++ (show lispCdrTest1))                  >>
       (putStrLn $ "Test lispCar \"(cdr '(1 2 3))\" -> " ++ (show lispCdrTest2))                    >>
       (putStrLn $ "Test lispEq \"(eq? 1 1)\" -> " ++ (show lispEqTest1))                           >>
       (putStrLn $ "Test lispEq \"(eq? (+ 1 1) 1)\" -> " ++ (show lispEqTest2))                     >>
       (putStrLn $ "Test lispEq \"(eq? 'foo 'bar)\" -> " ++ (show lispEqTest3))                     >>
       (putStrLn $ "Test lispEq \"(eq? '() '())\" -> " ++ (show lispEqTest4))
