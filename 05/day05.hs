#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString        ((<?>), Parser, parseOnly, choice)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString                   (ByteString)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
import qualified Data.Map.Strict as M
--------------------------------------------------------------------------------

data Vent = Vent { x1 :: Int
                 , x2 :: Int
                 , y1 :: Int
                 , y2 :: Int
                 }
    deriving (Eq, Ord, Show)

parseInput :: ByteString -> Either String [Vent]
parseInput = parseOnly $ do 
    vents  <- many' $ vent <* endOfLine
    endOfInput
    return vents

    where
        vent :: Parser Vent
        vent = do x1 <- decimal
                  char ','
                  y1 <- decimal
                  string " -> "
                  x2 <- decimal
                  char ','
                  y2 <- decimal
                  return Vent{..}

--------------------------------------------------------------------------------

isHorizontal :: Vent -> Bool
isHorizontal Vent{..} = y1 == y2

isVertical :: Vent -> Bool
isVertical Vent{..} = x1 == x2

expand :: Vent -> [Vent]
expand vent@Vent{..} | isHorizontal vent = [ Vent { x1 = x, x2 = x, y1 = y1, y2 = y2 }
                                           | x <- [min x1 x2 .. max x1 x2]
                                           ]
                     | isVertical   vent = [ Vent { x1 = x1, x2 = x2, y1 = y, y2 = y}
                                           | y <- [min y1 y2 .. max y1 y2]
                                           ]
                     | x1 <= x2          = mkDiagonal vent
                     | otherwise         = mkDiagonal (Vent { x1 = x2, x2 = x1, y1 = y2, y2 = y1 })
    where
        mkDiagonal :: Vent -> [Vent]
        mkDiagonal vent@Vent{..} = if y1 <= y2
                                    then [ Vent { x1 = x1 + ix, x2 = x1 + ix, y1 = y1 + ix, y2 = y1 + ix }
                                         | ix <- [0 .. x2-x1]
                                         ]
                                    else [ Vent { x1 = x1 + ix, x2 = x1 + ix, y1 = y1 - ix, y2 = y1 - ix }
                                         | ix <- [0 .. x2-x1]
                                         ]

main :: IO ()
main = do
    input <- parseInput <$> BS.readFile "input.txt"

    case input of
        Left err -> print err
        Right vents -> do
            let hv     = filter (\v -> isHorizontal v || isVertical v) vents
                ex     = concatMap expand hv
                counts = foldr (\vent m -> M.insertWith (+) vent 1 m) M.empty ex
            putStrLn $ "Part one: " ++ show (length . filter (>1) . M.elems $ counts)

            let ex     = concatMap expand vents
                counts = foldr (\vent m -> M.insertWith (+) vent 1 m) M.empty ex
            putStrLn $ "Part two: " ++ show (length . filter (>1) . M.elems $ counts)