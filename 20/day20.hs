#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Data.Map         as M
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

data Pixel = Dot
           | Hash

instance Show Pixel where
    show Dot  = "."
    show Hash = "#"

data Image = Image { width  :: Int
                   , height :: Int
                   , pixels :: !(Map Int Pixel)
                   }

instance Show Image where
    show Image{..} = unlines . chunks width . concatMap (show . snd) . M.toAscList $ pixels

type Algorithm = Int -> Pixel

type Parser = Parsec Void Text

parseInput :: Parser (Algorithm, Image)
parseInput = do
    algorithm <- mkAlgorithm <$> many pixel <* eol
    eol
    image <- mkImage <$> many (many pixel <* eol)
    eof
    return (algorithm, image)
    
    where
        pixel :: Parser Pixel
        pixel = choice [ char '.' >> return Dot
                       , char '#' >> return Hash
                       ]

mkAlgorithm :: [Pixel] -> Int -> Pixel
mkAlgorithm pixels n = M.fromList ([0..] `zip` pixels) M.! n

mkImage :: [[Pixel]] -> Image
mkImage pixelss = let width  = length . head $ pixelss
                      height = length          pixelss
                      pixels = M.fromList $ [0..] `zip` concat pixelss
                  in Image{..}

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l  = take n l : chunks n (drop n l)

--------------------------------------------------------------------------------

enhance :: Bool -> Algorithm -> Image -> Image
enhance odd algo Image{..} = let pixels' = [ algo (getIndex x y)
                                           | y <- [-1 .. height]
                                           , x <- [-1 .. width ]
                                           ]
                             in Image { width  = width  + 2
                                      , height = height + 2
                                      , pixels = M.fromList $ [0..] `zip` pixels'
                                      }
    where
        getIndex :: Int -> Int -> Int
        getIndex x y = toDec [ if x+i < 0 || x+i >= width || y+j < 0 || y+j >= height
                                   then if odd then Hash else Dot
                                   else pixels M.! ((y+j) * width + x+i)
                             | j <- [-1,0,1]
                             , i <- [-1,0,1]
                             ]
        
        toDec :: [Pixel] -> Int
        toDec pixels = sum . zipWith (*) [ 2^i | i <- [0..] ] . map (\case Dot -> 0; Hash -> 1) . reverse $ pixels

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right (algorithm, image) -> do
            let twoSteps  = enhance True algorithm . enhance False algorithm
                Image{..} = twoSteps image
            putStrLn $ "Part one: " ++ show (sum . map ((\case Dot -> 0; Hash -> 1) . snd) . M.toList $ pixels)

            let fiftySteps = foldr (.) id (replicate 25 twoSteps)
                Image{..}  = fiftySteps image
            putStrLn $ "Part two: " ++ show (sum . map ((\case Dot -> 0; Hash -> 1) . snd) . M.toList $ pixels)