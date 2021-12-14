#!/usr/bin/env stack 
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Map                          (Map)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.List        as L
import qualified Data.Map         as M
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

type Monomer = Char
type Polymer = [Monomer]

type Rule = (Polymer, Monomer)

type Parser = Parsec Void Text

parseInput :: Parser (Polymer, Map Polymer Monomer)
parseInput = do 
    poly  <- many upperChar <* eol
    eol
    rules <- M.fromList <$> many (rule <* eol)
    eof
    return (poly, rules)

    where        
        rule :: Parser Rule
        rule = do poly <- many upperChar
                  string " -> "
                  mono <- upperChar
                  return (poly, mono)

--------------------------------------------------------------------------------

apply :: Map Polymer Monomer -> Polymer -> Polymer
apply rules []         = []
apply rules [m]        = [m]
apply rules (m1:m2:ms) = [m1, rules M.! [m1,m2]] ++ apply rules (m2:ms)

countMonomeres :: Polymer -> Map Monomer Int
countMonomeres = foldr (\monomer m -> M.insertWith (+) monomer 1 m) M.empty

segment :: Polymer -> [Polymer]
segment []         = []
segment [m]        = []
segment (m1:m2:ms) = [m1,m2] : segment (m2:ms)

applySegments :: Map Polymer Monomer -> Map Polymer Int -> Map Polymer Int
applySegments rules = foldr (M.unionWith (+) . uncurry go) M.empty . M.toList
    where
        go :: Polymer -> Int -> Map Polymer Int
        go poly count = M.fromList [ (poly', count)
                                   | poly' <- segment . apply rules $ poly
                                   ]

getCount :: Map Polymer Int -> Map Monomer Int
getCount = foldr (M.unionWith (+) . (\(poly, count) -> M.singleton (head poly) count)) M.empty . M.toList 

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right (poly, rules) -> do
            let _10        = foldr (.) id (replicate 10 (apply rules))
                monoCounts = countMonomeres . _10 $ poly
                minCount   = snd . L.minimumBy (\(_,m1) (_,m2) -> compare m1 m2) . M.toList $ monoCounts
                maxCount   = snd . L.minimumBy (\(_,m1) (_,m2) -> compare m2 m1) . M.toList $ monoCounts

            putStrLn $ "Part one: " ++ show (maxCount - minCount)

            let segments   = foldr (M.unionWith (+) . (`M.singleton` 1)) M.empty . segment $ poly
                _40        = foldr (.) id (replicate 40 (applySegments rules))
                monoCounts = M.insertWith (+) (last poly) 1 . getCount . _40 $ segments
                minCount   = snd . L.minimumBy (\(_,m1) (_,m2) -> compare m1 m2) . M.toList $ monoCounts
                maxCount   = snd . L.minimumBy (\(_,m1) (_,m2) -> compare m2 m1) . M.toList $ monoCounts

            putStrLn $ "Part two: " ++ show (maxCount - minCount)