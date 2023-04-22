---
layout: post
title: Lexer in Haskell(Work in Progress)
published: true
---


# Crafting Interpreters

This is the book I am reading and I started to code the interpreter using JDK19's preview features.
These features introduce _record_ pattern matching and I realized languages like Haskell and OCaml
were built with such pattern matching in mind.

So I decided to code part of it using Haskell.

![image-title-here](../images/Craftinginterpreters.jpg){:class="img-responsive"}

# Explanation

TODO

# Code

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Char ( isSpace, isDigit, isAlpha, isAlphaNum )
import Data.Either
import Data.Maybe

data Operator i = Literal i| Plus | Minus | Div
  | InvalidChar i
    deriving (Show, Eq) 

data Error i e = Error
  { errorOffset :: Offset
  , error :: ErrorType i e
  } deriving (Eq, Show)


data ErrorType i e
  = EndOfInput
  | Unexpected i
  | Expected i e
  | ExpectedEndOfFile i
  | BeginningOfInput i
  | Empty
  deriving (Eq, Show)

type Offset = Int

operator :: Char -> Operator Char
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '/' = Div
           
scanner :: Offset -> ErrorType Offset Char-> String -> Either [Error Offset (ErrorType Offset Char)]  [Operator Char]
scanner offset error (x:xs) 
  | isDigit x = tokenize offset error (Literal x) 
  | x == '+' = tokenize offset error (operator x)
  | otherwise = tokenize offset error (InvalidChar x) 
  where
      tokenize offset error t = 
          case scanner offset error xs of
              Left err -> Left err
              Right tokens -> Right (t : tokens)
scanner offset error "" = Right []


main :: IO ()
main = do
    let result = scanner 0 (BeginningOfInput 0) "1+x2"
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right tokens -> putStrLn $ "Tokens: " ++ show tokens

{% endhighlight %}
