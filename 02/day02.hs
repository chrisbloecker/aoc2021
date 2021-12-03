#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString        ((<?>), Parser, parseOnly, choice)
import Data.Attoparsec.ByteString.Char8 
import Data.ByteString                   (ByteString)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
--------------------------------------------------------------------------------

data Position = Position { horizontal :: Int
                         , depth      :: Int
                         , aim        :: Int
                         }
    deriving (Show)

data Instruction = Forward Int
                 | Up      Int
                 | Down    Int
    deriving (Show)

(<+) :: Position -> Instruction -> Position
(<+) p@Position{..} = \case
                        Forward f -> p { horizontal = horizontal + f }
                        Up      u -> p { depth      = depth      - u }
                        Down    d -> p { depth      = depth      + d }

(<@) :: Position -> Instruction -> Position
(<@) p@Position{..} = \case
                        Forward f -> p { horizontal = horizontal + f, depth = depth + f * aim }
                        Up      u -> p { aim        = aim - u }
                        Down    d -> p { aim        = aim + d }

parseInput :: ByteString -> Either String [Instruction]
parseInput = parseOnly $ do 
    xs <- many' $ choice [forward, up, down] <* endOfLine
    endOfInput
    return xs

    where
        forward :: Parser Instruction
        forward = string "forward" >> skipSpace >> Forward <$> decimal
        
        up :: Parser Instruction
        up = string "up" >> skipSpace >> Up <$> decimal

        down :: Parser Instruction
        down = string "down" >> skipSpace >> Down <$> decimal

--------------------------------------------------------------------------------

main :: IO ()
main = do
    input <- parseInput <$> BS.readFile "input.txt"

    case input of
      Left err -> print err
      Right instructions -> do
        let startPosition = Position { horizontal = 0, depth = 0, aim = 0 }
            Position{..}  = foldl (<+) startPosition instructions
        
        putStrLn $ "Part one: " ++ show (horizontal * depth)
        
        let Position{..} = foldl (<@) startPosition instructions

        putStrLn $ "Part two: " ++ show (horizontal * depth)