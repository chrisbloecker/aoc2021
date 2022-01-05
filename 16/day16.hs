#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Control.Monad                     (replicateM)
import Data.Map.Strict                   (Map)
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

type BITS = [Int]

data Packet = Packet { packetVersion :: Int
                     , packetType    :: Int
                     , packetContent :: PacketContent
                     }
    deriving (Show)

data PacketContent = Literal  { value         :: BITS
                              }
                   | Operator { lengthType    :: Int
                              , content       :: [Packet]
                              }
    deriving (Show)

type Parser = Parsec Void Text

parseInput :: Parser [Packet]
parseInput = do 
    decode . concatMap hexToBin <$> many hexDigitChar <* eol <* eof

hexToBin :: Char -> [Int]
hexToBin '0' = [0,0,0,0]
hexToBin '1' = [0,0,0,1]
hexToBin '2' = [0,0,1,0]
hexToBin '3' = [0,0,1,1]
hexToBin '4' = [0,1,0,0]
hexToBin '5' = [0,1,0,1]
hexToBin '6' = [0,1,1,0]
hexToBin '7' = [0,1,1,1]
hexToBin '8' = [1,0,0,0]
hexToBin '9' = [1,0,0,1]
hexToBin 'A' = [1,0,1,0]
hexToBin 'B' = [1,0,1,1]
hexToBin 'C' = [1,1,0,0]
hexToBin 'D' = [1,1,0,1]
hexToBin 'E' = [1,1,1,0]
hexToBin 'F' = [1,1,1,1]

binToDec :: [Int] -> Int
binToDec bins = sum $ zipWith (*) (reverse bins) [ 2^i | i <- [0..] ]

-- we should really use a state monad here...
decode :: BITS -> [Packet]
decode []    = []
decode input = let (packet, rest) = go input
               in packet : decode rest
               
    where
        go :: BITS -> (Packet, BITS)
        go input = let (packetVersion, rest1) = (binToDec (take 3 input), drop 3 input)
                       (packetType,    rest2) = (binToDec (take 3 rest1), drop 3 rest1)
                       (packetContent, rest3) = case packetType of
                                                    -- literal
                                                    4 -> let (chunks, rest3) = literal rest2
                                                             --rest4 = drop (4 - (3 + 3 + length chunks * 5) `mod` 4) rest3
                                                         in (Literal (concat chunks), rest3)

                                                    -- operator
                                                    _ -> let (lengthType, rest3) = (binToDec (take 1 rest2), drop 1 rest2)
                                                             (content,    rest4) = case lengthType of
                                                                                       0 -> let (totalLength, rest4) = (binToDec (take 15 rest3), drop 15 rest3)
                                                                                            in (decode (take totalLength rest4), drop totalLength rest4)
                                                                                       1 -> let (subPackets,  rest4) = (binToDec (take 11 rest3), drop 11 rest3)
                                                                                            in parseSubPackets subPackets rest4
                                                         in (Operator{..}, rest4)
                       
                   in (Packet{..}, rest3)
        
        literal :: BITS -> ([BITS], BITS)
        literal (b:bs) = case b of
                             0 -> ([take 4 bs], drop 4 bs)
                             1 -> let (chunks, rest) = literal (drop 4 bs)
                                  in (take 4 bs : chunks, rest)
        
        dropZeroes :: BITS -> BITS
        dropZeroes bits = let bits' = drop (4 - length bits `mod` 4) bits
                          in case bits' of
                              (0:0:0:0:bs) -> dropZeroes bs
                              bs           -> bs
        
        parseSubPackets :: Int -> BITS -> ([Packet], BITS)
        parseSubPackets 0 bs = ([], bs)
        parseSubPackets n [] = ([], [])
        parseSubPackets n bs = let (p,  rest1) = go bs
                                   (ps, rest2) = parseSubPackets (n-1) rest1
                               in (p : ps, rest2)

--------------------------------------------------------------------------------

versionSum :: Packet -> Int
versionSum Packet{..} = packetVersion + case packetContent of
                                            Literal{..}  -> 0
                                            Operator{..} -> sum . map versionSum $ content

interpret :: Packet -> Int
interpret Packet{..} =
    case packetContent of
        Literal value -> binToDec value
        Operator{..}  -> case packetType of
            0 -> sum     (map interpret content)
            1 -> product (map interpret content)
            2 -> minimum (map interpret content)
            3 -> maximum (map interpret content)
            4 -> error "Literal."
            5 -> if interpret (head content) >  interpret (content !! 1) then 1 else 0
            6 -> if interpret (head content) <  interpret (content !! 1) then 1 else 0
            7 -> if interpret (head content) == interpret (content !! 1) then 1 else 0


main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left err -> print err
        Right packets -> do
            putStrLn $ "Part one: " ++ show (versionSum . head $ packets)
            putStrLn $ "Part two: " ++ show (interpret  . head $ packets)