#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                     (replicateM)
import Data.List                         (sort)
import Data.Map.Strict                   (Map)
import Data.Maybe                        (catMaybes, fromJust, fromMaybe)
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

type Position   = (Int, Int)
type Height     = Int
type Topography = Map Position Height

type Parser = Parsec Void Text

parseInput :: Parser Topography
parseInput = do 
    xss <- many (digits <* eol)
    eof
    return . M.fromList $ [ ((i,j), x)
                          | (j, xs) <- [0..] `zip` xss
                          , (i, x ) <- [0..] `zip` xs
                          ]

    where
        digits :: Parser [Height]
        digits = fmap (read . (:[])) <$> many digitChar

--------------------------------------------------------------------------------

neighbours :: Position -> [Position]
neighbours (x,y) = [ (x-1, y  )
                   , (x+1, y  )
                   , (x,   y-1)
                   , (x,   y+1)
                   ]

findMinima :: Topography -> Topography
findMinima t = M.filterWithKey (\position height -> all (>height) . catMaybes $ [ M.lookup neighbour t | neighbour <- neighbours position ]) t

findBasin :: Topography -> Set Position -> Set Position
findBasin topography basin =
    let expanded = S.fromList [ neighbour
                              | position  <- S.toList basin
                              , neighbour <- neighbours position
                              , fromMaybe 9 (neighbour `M.lookup` topography) < 9
                              ] `S.union` basin
    in if expanded == basin
           then basin
           else findBasin topography expanded

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left err -> print err
        Right topography -> do
            let minima = findMinima topography
            putStrLn $ "Part one: " ++ show (sum . fmap (+1) . M.elems $ minima)

            let basins = fmap (findBasin topography . S.singleton) (M.keys minima)
            putStrLn $ "Part two: " ++ show (product . take 3 . reverse . sort . fmap length $ basins)