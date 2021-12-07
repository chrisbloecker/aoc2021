#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString        ((<?>), Parser, parseOnly, choice)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString                   (ByteString)
import Data.List                         (sort)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
--------------------------------------------------------------------------------

type Location = Int

parseInput :: ByteString -> Either String [Location]
parseInput = parseOnly $ do 
    xs <- sepBy decimal (char ',') <* endOfLine
    endOfInput
    return xs

--------------------------------------------------------------------------------

increasingConsumption :: [Int]
increasingConsumption = scanl (+) 0 [1..]

main :: IO ()
main = do
    input <- parseInput <$> BS.readFile "input.txt"

    case input of
        Left err -> print err
        Right locations -> do
            let median    = sort locations !! (length locations `div` 2)
                fuelCosts = sum [ abs (location - median)  | location <- locations]
            putStrLn $ "Part one: " ++ show fuelCosts

            let distances = [ sum [ increasingConsumption !! abs (location - center)
                                  | location <- locations
                                  ]
                            | center <- [minimum locations .. maximum locations]
                            ]
            putStrLn $ "Part two: " ++ show (minimum distances)