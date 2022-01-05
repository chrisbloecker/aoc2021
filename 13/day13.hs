#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Set                          (Set)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.Set         as S
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

data Instruction = FoldX Int
                 | FoldY Int
    deriving (Show)

type Mark = (Int, Int)

data Origami = Origami { marks        :: Set Mark
                       , instructions :: [Instruction]
                       , width        :: Int
                       , height       :: Int
                       }
    deriving (Show)

type Parser = Parsec Void Text

parseInput :: Parser Origami
parseInput = do 
    marks <- S.fromList <$> many (mark <* eol)
    eol
    instructions <- many (instruction <* eol)
    eof
    let width  = maximum . map fst . S.toList $ marks
        height = maximum . map snd . S.toList $ marks
    return Origami{..}

    where
        mark :: Parser Mark
        mark = do u <- decimal
                  char ','
                  v <- decimal
                  return (u,v)
        
        instruction :: Parser Instruction
        instruction = choice [ FoldX <$> (string "fold along x=" *> decimal)
                             , FoldY <$> (string "fold along y=" *> decimal)
                             ]

--------------------------------------------------------------------------------

foldX :: Int -> Set Mark -> Set Mark
foldX at ms =
    let leftOf  = S.filter (\(x,_) -> x < at) ms
        rightOf = S.filter (\(x,_) -> x > at) ms
    in leftOf `S.union` S.map (\(x,y) -> (2*at-x, y)) rightOf

foldY :: Int -> Set Mark -> Set Mark
foldY at ms =
    let aboveOf = S.filter (\(_,y) -> y < at) ms
        belowOf = S.filter (\(_,y) -> y > at) ms
    in aboveOf `S.union` S.map (\(x,y) -> (x, 2*at-y)) belowOf


step :: Origami -> Origami
step o@Origami{..} =
    case instructions of
        []     -> o
        (FoldX x:is) -> Origami { marks = foldX x marks, instructions = is, width = x,     height = height }
        (FoldY y:is) -> Origami { marks = foldY y marks, instructions = is, width = width, height = y      }

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l  = take n l : chunks n (drop n l)

plot :: Origami -> String
plot Origami{..} = 
    let grid = [ if (x,y) `S.member` marks then '#' else '.'
               | y <- [0 .. height-1]
               , x <- [0 .. width-1 ]
               ]
    in unlines . chunks width $ grid

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right origami -> do
            putStrLn $ "Part one: "  ++ show (length . marks . step $ origami)
            putStrLn $ "Part two:\n" ++ plot (foldr (.) id (replicate (length . instructions $ origami) step) origami)