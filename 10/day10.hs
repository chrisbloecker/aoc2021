#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.List                         (sort)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

data Parenthesis = RoundOpen  | RoundClose
                 | SquareOpen | SquareClose
                 | CurlyOpen  | CurlyClose
                 | PointyOpen | PointyClose
    deriving (Eq)

instance Show Parenthesis where
    show RoundOpen   = "("
    show RoundClose  = ")"
    show SquareOpen  = "["
    show SquareClose = "]"
    show CurlyOpen   = "{"
    show CurlyClose  = "}"
    show PointyOpen  = "<"
    show PointyClose = ">"

type Parentheses = [Parenthesis]

data CheckResult = Ok
                 | Mismatch   Parenthesis
                 | Incomplete Parentheses

instance Show CheckResult where
    show Ok              = "Ok"
    show (Mismatch   p ) = "Mismatch " ++ show p
    show (Incomplete ps) = "Incomplete " ++ concatMap show ps

type Parser = Parsec Void Text

parseInput :: Parser [Parentheses]
parseInput = do 
    many (many parenthesis <* eol) <* eof

    where
        parenthesis :: Parser Parenthesis
        parenthesis = choice [ char '(' >> return RoundOpen
                             , char ')' >> return RoundClose
                             , char '[' >> return SquareOpen
                             , char ']' >> return SquareClose
                             , char '{' >> return CurlyOpen
                             , char '}' >> return CurlyClose
                             , char '<' >> return PointyOpen
                             , char '>' >> return PointyClose
                             ]

--------------------------------------------------------------------------------

isIncomplete :: CheckResult -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _              = False

hasMismatch :: CheckResult -> Bool
hasMismatch (Mismatch _) = True
hasMismatch _            = False

check :: Parentheses -> CheckResult
check = check' []
    where
        check' :: Parentheses -> Parentheses -> CheckResult
        check' []             []                = Ok
        check' stack          []                = Incomplete stack
        check' stack          (RoundOpen  : ps) = check' (RoundClose  : stack) ps
        check' stack          (SquareOpen : ps) = check' (SquareClose : stack) ps
        check' stack          (CurlyOpen  : ps) = check' (CurlyClose  : stack) ps
        check' stack          (PointyOpen : ps) = check' (PointyClose : stack) ps
        check' (expect:stack) (seen       : ps) = if expect == seen
                                                      then check' stack ps
                                                      else Mismatch seen

toScore :: CheckResult -> Int
toScore (Mismatch RoundClose)  = 3
toScore (Mismatch SquareClose) = 57
toScore (Mismatch CurlyClose)  = 1197
toScore (Mismatch PointyClose) = 25137
toScore (Incomplete ps) = toScore' 0 ps
    where
        toScore' :: Int -> Parentheses -> Int
        toScore' accum []               = accum
        toScore' accum (RoundClose:ps)  = toScore' (1 + 5*accum) ps
        toScore' accum (SquareClose:ps) = toScore' (2 + 5*accum) ps
        toScore' accum (CurlyClose:ps)  = toScore' (3 + 5*accum) ps
        toScore' accum (PointyClose:ps) = toScore' (4 + 5*accum) ps
toScore result = error $ "Cannot convert result " ++ show result

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left err -> print err
        Right parenthesess -> do
            let checkResults = map check parenthesess
                mismatches   = filter hasMismatch  checkResults
                incompletes  = filter isIncomplete checkResults

            putStrLn $ "Part one: " ++ show (sum    . map toScore $ mismatches)
            putStrLn $ "Part two: " ++ show (median . map toScore $ incompletes)