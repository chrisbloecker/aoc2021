#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import qualified Data.List        as L
import qualified Data.Map.Strict  as M
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

type Position = (Int, Int)

data Octopus = Flashed
             | Charging Int
    deriving (Show)

data Topography = Topography { octopusses :: Map Position Octopus
                             , flashcount :: Int
                             }
    deriving (Show)

type Parser = Parsec Void Text

parseInput :: Parser Topography
parseInput = do 
    xss <- many (digits <* eol)
    eof
    let octopusses = M.fromList $ [ ((i,j), Charging x)
                                  | (j, xs) <- [0..] `zip` xss
                                  , (i, x ) <- [0..] `zip` xs
                                  ]
        flashcount = 0
    return Topography{..}

    where
        digits :: Parser [Int]
        digits = fmap (read . (:[])) <$> many digitChar

--------------------------------------------------------------------------------

neighbours :: Position -> [Position]
neighbours (x,y) = [ (x-1, y-1)
                   , (x,   y-1)
                   , (x+1, y-1)
                   , (x-1, y  )
                   , (x+1, y  )
                   , (x-1, y+1)
                   , (x,   y+1)
                   , (x+1, y+1)
                   ]

activateOctopus :: Octopus -> Octopus
activateOctopus Flashed = Charging 0
activateOctopus octopus = octopus

tickOctopus :: Octopus -> Octopus
tickOctopus Flashed      = Flashed
tickOctopus (Charging n) = Charging (n+1)

canFlash :: Octopus -> Bool
canFlash Flashed      = False
canFlash (Charging n) = n > 9

hasFlashed :: Octopus -> Bool
hasFlashed Flashed = True
hasFlashed _       = False

flashOctopusses :: Topography -> Topography
flashOctopusses t@Topography{..} =
    case M.toList (M.filter canFlash octopusses) of
        []               -> t
        ((position,_):_) -> let newNeighbours = tickOctopus <$> M.filterWithKey (\k _ -> k `L.elem` neighbours position) octopusses
                                octopusses'   = M.singleton position Flashed `M.union` newNeighbours `M.union` octopusses
                            in flashOctopusses Topography { octopusses = octopusses', flashcount = flashcount + 1 }

tick :: Topography -> Topography
tick Topography{..} =
    let octopusses' = fmap (tickOctopus . activateOctopus) octopusses
    in flashOctopusses Topography{ octopusses = octopusses', flashcount = flashcount }

detectSynchronisation :: Topography -> Int
detectSynchronisation t@Topography{..} =
    if all hasFlashed (M.elems octopusses)
        then 0
        else 1 + detectSynchronisation (tick t)

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left err -> print err
        Right topography -> do
            let _100 = foldr (.) id (replicate 100 tick)
            putStrLn $ "Part one: " ++ show (flashcount . _100 $ topography)
            putStrLn $ "Part two: " ++ show (detectSynchronisation topography)