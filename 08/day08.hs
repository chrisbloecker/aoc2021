#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                     (replicateM, forM_)
import Data.Maybe                        (fromJust)
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

type Segment = Set Char

type Display = [Segment]

data Observation = Observation { segments :: [Segment]
                               , display  :: Display
                               }
    deriving (Show)

type Parser = Parsec Void Text

parseInput :: Parser [Observation]
parseInput = do 
    observations <- many observation
    eof
    return observations

observation :: Parser Observation
observation = do
    segments <- replicateM 10 segment
    string "| "
    display  <- replicateM  4 segment
    return Observation{..}

segment :: Parser Segment
segment = S.fromList <$> many letterChar <* space

--------------------------------------------------------------------------------

is1 :: Segment -> Bool
is1 s = S.size s == 2

is4 :: Segment -> Bool
is4 s = S.size s == 4

is7 :: Segment -> Bool
is7 s = S.size s == 3

is8 :: Segment -> Bool
is8 s = S.size s == 7

is1478 :: Segment -> Bool
is1478 s = S.size s `elem` [2,3,4,7]

decode :: Observation -> Int
decode Observation{..} =
    let one    = head . filter is1 $ segments
        four   = head . filter is4 $ segments
        seven  = head . filter is7 $ segments
        eight  = head . filter is8 $ segments

        rest   = [ s | s <- segments, not (is1478 s)]

        a      = seven `S.difference` one
        bd     = four  `S.difference` one
        abd    = a     `S.union`      bd

        two    = case [ s | s <- rest, not (abd `S.isSubsetOf` s), not (one `S.isSubsetOf` s)] of
                    [s] -> s
                    ss  -> error $ "two: " ++ show ss

        acdefg = two `S.union` one
        three  = case [ s | s <- rest, s `S.isSubsetOf` acdefg, s /= two ] of
                    [s] -> s
                    ss  -> error $ "three: " ++ show ss

        e      = two `S.difference` three
        f      = three `S.difference` two

        nine   = eight `S.difference` e
        g      = nine `S.difference` four `S.difference` a

        zeroFiveSix = [ s | s <- rest, s `notElem` [one, two, three, four, seven, eight, nine] ]

        five   = head [ s | s <- zeroFiveSix, S.size (s `S.difference` a `S.difference` g) == 3 ]
        six    = five `S.union` e
        zero   = head [ s | s <- zeroFiveSix, s `notElem` [five, six]]

        m = [zero, one, two, three, four, five, six, seven, eight, nine] `zip` [0..]
    
        digits = map (\d -> fromJust . lookup d $ m) display
    in sum . zipWith (*) [1000, 100, 10, 1] $ digits

main :: IO ()
main = do
    input <- parse parseInput "input.txt" <$> T.readFile "input.txt"

    case input of
        Left err -> print err
        Right observations -> do
            let _1478 = length . concatMap (filter is1478 . display) $ observations

            putStrLn $ "Part one: " ++ show _1478
            putStrLn $ "Part two: " ++ show (sum . map decode $ observations)