#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Text                         (Text)
import Data.Void                         (Void)
import Text.Megaparsec            
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
--------------------------------------------------------------------------------
import qualified Data.List        as L
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

data Snailfish = Fish Integer
               | Snail Snailfish Snailfish
    deriving (Eq)

data Explosion = Unchanged Snailfish
               | Exploding Snailfish (Maybe Integer) (Maybe Integer)
    deriving (Show)

data Split = Unsplit Snailfish
           | Split   Snailfish
    deriving (Show)

(<+>) :: Snailfish -> Snailfish -> Snailfish
sf1 <+> sf2 = reduce $ Snail sf1 sf2

type Parser = Parsec Void Text

parseInput :: Parser [Snailfish]
parseInput = many (snailfish <* eol) <* eof
    where
        snailfish :: Parser Snailfish
        snailfish = choice [ Fish <$> decimal
                           , do char '['
                                sf1 <- snailfish
                                char ','
                                sf2 <- snailfish
                                char ']'
                                return $ Snail sf1 sf2
                           ]

instance Show Snailfish where
    show (Fish x) = show x
    show (Snail sf1 sf2) = "[" ++ show sf1 ++ "," ++ show sf2 ++ "]"

--------------------------------------------------------------------------------

explode :: Snailfish -> Snailfish
explode sf = case go 4 sf of
                 Unchanged _       -> sf
                 Exploding sf' _ _ -> sf'
    where
        go :: Int -> Snailfish -> Explosion
        go _ f@(Fish _)                = Unchanged f
        go 0 (Snail (Fish x) (Fish y)) = Exploding (Fish 0) (Just x) (Just y)
        go n (Snail l r)               = case go (n-1) l of
                                             e@(Unchanged _) ->
                                                 case go (n-1) r of
                                                     e@(Unchanged _)                    -> Unchanged (Snail l                  r )
                                                     e@(Exploding r' Nothing  Nothing)  -> Exploding (Snail l                  r') Nothing Nothing
                                                     e@(Exploding r' (Just x) Nothing)  -> Exploding (Snail (addRightmost x l) r') Nothing Nothing
                                                     e@(Exploding r' Nothing  (Just y)) -> Exploding (Snail l                  r') Nothing (Just y)
                                                     e@(Exploding r' (Just x) (Just y)) -> Exploding (Snail (addRightmost x l) r') Nothing (Just y)
                                             e@(Exploding l' Nothing  Nothing)  -> Exploding (Snail l' r                ) Nothing  Nothing
                                             e@(Exploding l' (Just x) Nothing)  -> Exploding (Snail l' r                ) (Just x) Nothing
                                             e@(Exploding l' Nothing  (Just y)) -> Exploding (Snail l' (addLeftmost y r)) Nothing  Nothing
                                             e@(Exploding l' (Just x) (Just y)) -> Exploding (Snail l' (addLeftmost y r)) (Just x) Nothing
        
        addLeftmost :: Integer -> Snailfish -> Snailfish
        addLeftmost z (Fish x)    = Fish (x+z)
        addLeftmost z (Snail l r) = Snail (addLeftmost z l) r

        addRightmost :: Integer -> Snailfish -> Snailfish
        addRightmost z (Fish x)    = Fish (x+z)
        addRightmost z (Snail l r) = Snail l (addRightmost z r)

split :: Snailfish -> Snailfish
split sf = case go sf of
               Unsplit sf' -> sf'
               Split   sf' -> sf'
    where
        go :: Snailfish -> Split
        go f@(Fish x) = if x >= 10 
                            then Split (Snail (Fish $ x `div` 2) (Fish $ x `div` 2 + x `mod` 2))
                            else Unsplit f
        go (Snail l r) = case go l of
                             Unsplit _  -> case go r of
                                 Unsplit _  -> Unsplit (Snail l r)
                                 Split   r' -> Split (Snail l r')
                             Split   l' -> Split (Snail l' r)

reduce :: Snailfish -> Snailfish
reduce sf = let sf'  = explode sf
                sf'' = split   sf
            in if sf' /= sf
                   then reduce sf'
                   else if sf'' /= sf
                            then reduce sf''
                            else sf

magnitude :: Snailfish -> Integer
magnitude (Fish x) = x
magnitude (Snail sf1 sf2) = 3 * magnitude sf1 + 2 * magnitude sf2

explodeN :: Int -> Snailfish -> Snailfish
explodeN n = foldr (.) id (replicate n explode)

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right snailfish -> do
            putStrLn $ "Part one: " ++ show (magnitude . foldl1 (<+>) $ snailfish)

            let magnitudes = [ magnitude . reduce $ l <+> r
                             | l <- snailfish
                             , r <- snailfish
                             , l /= r
                             ]
            putStrLn $ "Part two: " ++ show (maximum magnitudes)