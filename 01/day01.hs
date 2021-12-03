#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Main
  where
--------------------------------------------------------------------------------
import Data.Attoparsec.ByteString        ((<?>), Parser, parseOnly)
import Data.Attoparsec.ByteString.Char8  (decimal, endOfLine, endOfInput, many')
import Data.ByteString                   (ByteString)
import Data.List                         (elem)
--------------------------------------------------------------------------------
import qualified Data.ByteString as BS   (readFile)
--------------------------------------------------------------------------------

parseInput :: ByteString -> Either String [Int]
parseInput text = parseOnly parser text
  where
    parser :: Parser [Int]
    parser = do
      xs <- many' $ decimal <* endOfLine
      endOfInput
      return xs

--------------------------------------------------------------------------------

countIncreses :: [Int] -> Int
countIncreses xs = length . filter (\p -> fst p < snd p) $ xs `zip` tail xs

main :: IO ()
main = do
    input <- parseInput <$> BS.readFile "input.txt"

    case input of
      Left err -> print err
      Right xs -> do
        let slidingWindows = zip3 xs (drop 1 xs) (drop 2 xs)
            windowSums     = map (\(a,b,c) -> a+b+c) slidingWindows

        putStrLn $ "Part one: " ++ show (countIncreses xs)
        putStrLn $ "Part two: " ++ show (countIncreses windowSums)