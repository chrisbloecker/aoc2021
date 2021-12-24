#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  where
--------------------------------------------------------------------------------
import Data.Map                          (Map)
import Data.Text                         (Text)
import Data.Void                         (Void)
--------------------------------------------------------------------------------
import qualified Data.Map         as M
import qualified Data.Text.IO     as T   (readFile)
--------------------------------------------------------------------------------

data Player = Player { position :: Int
                     , score    :: Int
                     }
    deriving (Eq, Ord, Show)

data Game = Game { player1 :: Player
                 , player2 :: Player
                 }
    deriving (Eq, Ord, Show)

type Die = [(Int, Int)]

--------------------------------------------------------------------------------

deterministic :: Die
deterministic = [1..] `zip` concat (repeat [1..100])

play :: Player -> Player -> Die -> (Player, Player, Int)
play player1@Player{..} player2 die =
    let ((_,n1):(_,n2):(rolls,n3):die') = die
        newPos   = (position + n1 + n2 + n3) `mod` 10
        newScore = score + newPos + 1
        player1' = Player { position = newPos, score = newScore }
    in if newScore >= 1000
           then (player1', player2, rolls)
           else play player2 player1' die'

dirac :: [Int]
dirac = [ x+y+z | x <- [1..3], y <- [1..3], z <- [1..3] ]

turn :: Player -> ([Player], [Player])
turn player =
    let newPlayers = [ let Player{..} = player
                           newPos     = (position + roll) `mod` 10
                           newScore   = score + newPos + 1
                       in Player { position = newPos, score = newScore }
                     | roll <- dirac
                     ]
        won        = filter ((>= 21) . score) newPlayers
        continue   = filter ((<  21) . score) newPlayers
    in (won, continue)


-- returns how many times player1 and player2, respectively, win in the given game
playDirac :: Game -> (Int, Int)
playDirac game@Game{..} =
    let (won1, continue) = turn player1
        games            = foldr (M.unionWith (+)) M.empty
                         $ [ M.singleton (Game { player1 = player2, player2 = player1' }) 1
                           | player1' <- continue
                           ]
        (sub2s, sub1s)   = unzip [ let (sub2, sub1) = playDirac game
                                   in (count*sub2, count*sub1)
                                 | (game, count) <- M.toList games
                                 ]
    in (length won1 + sum sub1s, sum sub2s)

main :: IO ()
main = do
    let player1 = Player { position = 5, score = 0 }
        player2 = Player { position = 2, score = 0 }

        (player1', player2', rolls) = play player1 player2 deterministic

    putStrLn $ "Part one: " ++ show (score player2' * rolls)

    let game = Game player1 player2
        (wins1, wins2) = playDirac game
    
    putStrLn $ "Part two: " ++ show (min wins1 wins2)