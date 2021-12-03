#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString        ((<?>), Parser, parseOnly, choice)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString                   (ByteString)
import Data.Maybe                        (fromJust)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
--------------------------------------------------------------------------------

data Bit = Zero | One deriving (Eq, Show)

type Bits = [Bit]

parseInput :: ByteString -> Either String [Bits]
parseInput = parseOnly $ do 
    xs <- many' $ many' (choice [zero, one]) <* endOfLine
    endOfInput
    return xs

    where
        zero :: Parser Bit
        zero = char '0' >> return Zero
    
        one :: Parser Bit
        one = char '1' >> return One

--------------------------------------------------------------------------------

majorityAt :: [Bits] -> Int -> Maybe Bit
majorityAt bitss n | onesAtN > zerosAtN = Just One
                   | onesAtN < zerosAtN = Just Zero
                   | otherwise          = Nothing
    where
        onesAtN  = length . filter (== One) . concatMap (take 1 . drop n) $ bitss
        zerosAtN = length bitss - onesAtN

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One  = Zero

bitsToInt :: Bits -> Int
bitsToInt bits = sum . zipWith mul (reverse bits) $ [0..]
    where
        mul :: Bit -> Int -> Int
        mul Zero _ = 0
        mul One  n = 2^n

ratingSelector :: (Maybe Bit -> Bit) -> [Bits] -> Bits
ratingSelector bitSelector bitss = ratingSelector' bitss 0
    where
        ratingSelector' :: [Bits] -> Int -> Bits
        ratingSelector' [bits] _ = bits
        ratingSelector' bitss  n = 
            let selectedBit = bitSelector (majorityAt bitss n)
                bitss' = filter (\bits -> bits !! n == selectedBit) bitss
            in ratingSelector' bitss' (n+1)

oxygenGeneratorRating :: [Bits] -> Bits
oxygenGeneratorRating = ratingSelector f
    where
        f (Just Zero) = Zero
        f (Just One)  = One
        f Nothing     = One

co2ScrubberRating :: [Bits] -> Bits
co2ScrubberRating = ratingSelector f
    where
        f (Just Zero) = One
        f (Just One)  = Zero
        f Nothing     = Zero

main :: IO ()
main = do
    input <- parseInput <$> BS.readFile "input.txt"

    case input of
      Left err -> print err
      Right bitss -> do
        let gamma   = map (fromJust . majorityAt bitss) [0..11]
            epsilon = map flipBit gamma

        putStrLn $ "Part one: " ++ show (bitsToInt gamma * bitsToInt epsilon)

        let oxygen = oxygenGeneratorRating bitss
            co2    = co2ScrubberRating     bitss

        putStrLn $ "Part two: " ++ show (bitsToInt oxygen * bitsToInt co2)