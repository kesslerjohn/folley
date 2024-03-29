{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Char
import Control.Applicative
import GHC.Natural

-- TODO: How to get values into here? 
-- parametrize this type or use a specific var data type?

-- TODO: this definition of quantifiers allows for degenerate constructions
-- how to do it better?

-- TODO: Is it possible to simplify connectives? I'd like to have intermediate 
-- representations of &, |, =>, ~, < that are easy to operate on
-- actually can just push these into the Haskell implementations, but
-- do need some way to simplify and track formulae with free variables

data FOLFormula 
    = Conj FOLFormula FOLFormula
    | Not FOLFormula
    | All
    | Ex
    | FOLNat Natural
    | FOLBool Bool
    | Var String
    deriving (Show, Eq)

-- TODO: May want to change type to Either (String, Int, Int) (String, a)
-- so that if parsing fails, then it can give a description of the error and 
-- where it occurred

newtype Parser a = Parser { 
    runParser :: String -> Maybe (String, a)
}

-- in order to apply traverse or sequenceA . map to charP, you need to prove that
-- Parser is applicative. there are two parts to this - 1, show that it is a functor
-- by implementing fmap:

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (input', x) <- p input
        Just (input', f x)

-- 2, show that it is an applicative by implementing pure and (<*>):

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = 
        Parser $ \input -> do 
                        (input', f) <- p1 input
                        (input'', x) <- p2 input'
                        Just (input'', f x)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    Parser p1 <|> Parser p2 = 
        Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char 
charP x = Parser f 
    where 
        f (y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

folBool :: Parser FOLFormula
folBool = f <$> (stringP "T" <|> stringP "F")
        where
            f "T" = FOLBool True
            f "F" = FOLBool False
            f _ = undefined

folReader :: Maybe (String, FOLFormula) -> Maybe (String, FOLFormula)
folReader input = undefined

-- variables have to be all lowercase roman alphabet or '_'
-- wrapping fairly short predicate in this name for convenience
isVarChar :: Char -> Bool
isVarChar c = 95 <= ord c && 122 >= ord c

-- I want to take a conjunction of two variable names and make a Conj a b with them
-- so if I:

spanP :: (Char -> Bool) -> Parser String
spanP f = 
    Parser $ \ input -> 
        let (token, rest) = span f input in 
            Just (rest, token)

folVar :: Parser FOLFormula
folVar = f <$> spanP isVarChar where
    f s = Var s

folQuantifier :: Parser FOLFormula
folQuantifier = f <$> (stringP "A" <|> stringP "E") where
    f "A" = All
    f "E" = Ex
    f _ = undefined


-- >>> runParser folQuantifier "Ax:T.P(x)"
-- Just ("x:T.P(x)",All)

conj :: (String, String) -> Maybe (String, FOLFormula)
conj ("", _) = Nothing
conj (a, b) = case span (== '&') b of 
    ("&", str) ->  Just ("", Conj (Var a) (Var str))
    _ -> Nothing

folConj :: String -> Maybe (String, FOLFormula)
folConj s = conj $ span isVarChar s

-- >>> folConj "one&var_two"
-- Just ("",Conj (Var "one") (Var "var_two"))

a = Just ("&b", Var "a")

-- >>> fst <$> Just ("&b", Var "a")
-- Just "&b"

main :: IO ()
main = undefined
