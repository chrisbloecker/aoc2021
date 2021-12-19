#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                     (replicateM)
import Data.Map.Strict                   (Map)
import Data.Maybe                        (catMaybes)
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec            
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.List        as L
import qualified Data.Map.Strict  as M
import qualified Data.Set         as S
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

data Position = Position { x :: Int
                         , y :: Int
                         }
    deriving (Show)

data Speed = Speed { vx :: Int
                   , vy :: Int
                   }
    deriving (Show)

data Rectangle = Rectangle { xmin :: Int
                           , xmax :: Int
                           , ymin :: Int
                           , ymax :: Int
                           }
    deriving (Show)

data Probe = Probe { position :: Position
                   , speed    :: Speed
                   }
    deriving (Show)

type Parser = Parsec Void Text

parseInput :: Parser Rectangle
parseInput = do 
    string "target area: x="
    xmin <- signedInt
    string ".."
    xmax <- signedInt
    string ", y="
    ymin <- signedInt
    string ".."
    ymax <- signedInt
    eol
    eof
    return Rectangle{..}

    where
        signedInt :: Parser Int
        signedInt = choice [ char '-' >> decimal >>= \x -> return (-x)
                           , decimal
                           ]

--------------------------------------------------------------------------------

step :: Probe -> Probe
step (Probe Position{..} Speed{..}) = 
    Probe { position = Position (x+vx) (y+vy)
          , speed    = Speed (if vx /= 0 then vx - signum vx else vx) (vy-1)
          }

getTrajectory :: Rectangle -> [Position] -> Maybe [Position]
getTrajectory Rectangle{..} ps = let ps' = takeWhile (\Position{..} -> x <= xmax && y >= ymin) ps
                                 in case filter (\Position{..} -> x >= xmin && y <= ymax) ps' of
                                     [] -> Nothing
                                     _  -> Just ps'

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right area -> do
            print area
            let ts    = [ getTrajectory area ps
                        | vx    <- [        1 .. xmax area]
                        , vy    <- [ymin area .. 150]
                        , probe <- [Probe (Position 0 0) Speed{..}]
                        , ps    <- [map position . iterate step $ probe]
                        ]

            putStrLn $ "Part one: " ++ show (maximum . map (maximum . map y) . catMaybes $ ts)
            putStrLn $ "Part two: " ++ show (length . catMaybes $ ts)