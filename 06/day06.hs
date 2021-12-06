#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString        ((<?>), Parser, parseOnly, choice)
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor                    (first)
import Data.ByteString                   (ByteString)
import Data.Map.Strict                   (Map)
import Data.Maybe                        (fromMaybe)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
import qualified Data.Map.Strict as M
--------------------------------------------------------------------------------

type Countdown = Int
type Amount    = Int

type School = Map Countdown Amount

parseInput :: ByteString -> Either String School
parseInput = parseOnly $ do 
    xs <- sepBy decimal (char ',') <* endOfLine
    endOfInput
    let school = foldr (\x -> M.insertWith (+) x 1) M.empty $ xs
    return school

--------------------------------------------------------------------------------

tick :: School -> School
tick school = let spawn = M.singleton 8 (fromMaybe 0 (M.lookup 0 school))
                  new   = M.fromListWith (+)
                        . map (first updateCountdown)
                        . M.toList
                        $ school
              in M.filter (>0) (M.unionWith (+) spawn new)
    where
        updateCountdown :: Countdown -> Countdown
        updateCountdown c = if c == 0 then 6 else c - 1

main :: IO ()
main = do
    input <- parseInput <$> BS.readFile "input.txt"

    case input of
        Left err -> print err
        Right fishes -> do
            let eighty = foldr (.) id (replicate 80 tick)
            let _256   = foldr (.) id (replicate 256 tick)

            putStrLn $ "Part one: " ++ (show . sum . M.elems . eighty $ fishes)
            putStrLn $ "Part two: " ++ (show . sum . M.elems . _256 $ fishes)