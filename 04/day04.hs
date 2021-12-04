#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                     (replicateM)
import Data.Attoparsec.ByteString        ((<?>), Parser, parseOnly, choice)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString                   (ByteString)
import Data.List                         (transpose)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
--------------------------------------------------------------------------------

newtype Board = Board [[Cell]]
    deriving (Show)

data Cell = Unmarked Int
          | Marked   Int
    deriving (Show)

fromCell :: Cell -> Int
fromCell (Unmarked n) = n
fromCell (Marked   n) = n

parseInput :: ByteString -> Either String ([Int], [Board])
parseInput = parseOnly $ do 
    numbers <- sepBy decimal (char ',') <* endOfLine
    endOfLine
    boards  <- many' $ board <* endOfLine
    endOfInput
    return (numbers, boards)

    where
        board :: Parser Board
        board = Board <$> replicateM 5 row
    
        row :: Parser [Cell]
        row = replicateM 5 cell

        cell :: Parser Cell
        cell = do skipSpace
                  Unmarked <$> decimal

--------------------------------------------------------------------------------

isMarked :: Cell -> Bool
isMarked (Unmarked _) = False
isMarked (Marked   _) = True

-- check if there's a bingo
check :: Board -> Bool
check (Board cellss) = 
    let rowss = transpose cellss
    in any (all isMarked) (cellss ++ rowss)

-- mark a number
mark :: Int -> Board -> Board
mark number (Board cellss) = Board $ map (map mark') cellss
    where
        mark' :: Cell -> Cell
        mark' c@(Unmarked x) | x == number = Marked x
                             | otherwise   = c
        mark' c@(Marked   _) = c


--------------------------------------------------------------------------------

-- does the marking and selects the winning board
pickWinner :: [Int] -> [Board] -> (Int, Board)
pickWinner (n:ns) boards =
    let marked = map (mark n) boards
    in case filter check marked of
        []      -> pickWinner ns marked
        [board] -> (n, board)
        _       -> error "There is more than one winning board"

-- does the marking and selects the losing board,
-- including the numbers that are left to be called
pickLoser :: [Int] -> [Board] -> ([Int], Board)
pickLoser (n:ns) boards =
    let marked = map (mark n) boards
    in case filter (not . check) marked of
        []      -> error "There is no single board that loses"
        [board] -> (ns, board)
        boards' -> pickLoser ns boards'

main :: IO ()
main = do
    input <- parseInput <$> BS.readFile "input.txt"

    case input of
      Left err -> print err
      Right (numbers, boards) -> do
        let (calledNumber, Board winner) = pickWinner numbers boards
            unmarkedSum = sum . map fromCell . filter (not . isMarked) . concat $ winner
        
        putStrLn $ "Part one: " ++ show (calledNumber * unmarkedSum)

        let (leftoverNumbers, loser) = pickLoser numbers boards
            (calledNumber, Board b) = pickWinner leftoverNumbers [loser]
            unmarkedSum = sum . map fromCell . filter (not . isMarked) . concat $ b
        
        putStrLn $ "Part two: " ++ show (calledNumber * unmarkedSum)