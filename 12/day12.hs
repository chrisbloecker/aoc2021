#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                     (replicateM)
import Data.Map.Strict                   (Map)
import Data.Set                          (Set)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

data Node = Small String
          | Large String
    deriving (Eq, Ord, Show)

type Edge = (Node, Node)

type Path = [Node]

type Network = Map Node (Set Node)

type Parser = Parsec Void Text

parseInput :: Parser Network
parseInput = do 
    edges <- many (edge <* eol)
    eof
    return . foldr (M.unionWith S.union . (\(u,v) -> M.fromList [ (u, S.singleton v), (v, S.singleton u) ])) M.empty $ edges

    where
        edge :: Parser Edge
        edge = do u <- node
                  char '-'
                  v <- node
                  return (u,v)

        node :: Parser Node
        node = choice [ Small <$> some lowerChar
                      , Large <$> some upperChar
                      ]

--------------------------------------------------------------------------------

findPaths :: Network -> Path -> [Path]
findPaths network path@(previous:rest) = 
    if previous == Small "end"
        then [ path ]
        else concat [ findPaths network (neighbour : path)
                    | neighbour <- S.toList (network M.! previous)
                    , case neighbour of
                          s@(Small _) -> s `notElem` path
                          l@(Large _) -> True
                    ]

findPaths2 :: Network -> Path -> [Path]
findPaths2 network path@(previous:rest) =
    if previous == Small "end"
        then [ path ]
        else concat [ case neighbour of
                          s@(Small _) -> case length (filter (== s) path) of
                                             0 -> findPaths2 network (neighbour : path)
                                             1 -> findPaths  network (neighbour : path)
                                             _ -> []
                          l@(Large _) -> findPaths2 network (neighbour : path)
                    | neighbour <- S.toList (network M.! previous)
                    , neighbour /= Small "start"
                    ]

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right network -> do
            putStrLn $ "Part one: " ++ show (length . findPaths  network $ [Small "start"])
            putStrLn $ "Part two: " ++ show (length . findPaths2 network $ [Small "start"])