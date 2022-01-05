#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

{-# LANGUAGE BangPatterns #-}

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

type Position = (Int, Int)
type Risk     = Int

type Topography = Map Position Risk

type Parser = Parsec Void Text

parseInput :: Parser Topography
parseInput = do 
    xss <- many (digits <* eol)
    eof
    return . M.fromList $ [ ((i,j), x)
                          | (i, xs) <- [0..] `zip` xss
                          , (j, x ) <- [0..] `zip` xs
                          ]

    where
        digits :: Parser [Int]
        digits = fmap (read . (:[])) <$> many digitChar

--------------------------------------------------------------------------------

neighbours :: Position -> [Position]
neighbours (x,y) = [ (x,   y-1)
                   , (x-1, y  )
                   , (x+1, y  )
                   , (x,   y+1)
                   ]

dijkstra :: Topography -> Position -> Position -> Maybe Risk
dijkstra !topo s t = go M.empty (M.singleton s (s, 0))
    where
        go :: Map Position (Position, Risk) -> Map Position (Position, Risk) -> Maybe Risk
        go !explored !discovered 
            | t `M.member` explored = Just $ snd (explored M.! t)
            | M.null discovered     = Nothing
            | otherwise             = let (position, (prev, risk)) = L.minimumBy (\(_,(_,r1)) (_,(_,r2)) -> compare r1 r2) . M.toList $ discovered
                                          explored'                = M.insert position (prev, risk) explored
                                          discovering              = [ (neighbour, (position, risk + risk'))
                                                                     | (neighbour, risk') <- M.toList $ topo `M.restrictKeys` (S.fromList . neighbours $ position)
                                                                     , neighbour `M.notMember` explored
                                                                     ]
                                          discovered'              = M.unionWith (\a@(_,r1) b@(_,r2) -> if r1 <= r2 then a else b) (M.fromList discovering) (M.delete position discovered)
                                      in go explored' discovered'

manhattan :: Position -> Position -> Int
manhattan (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

expandTopography :: Topography -> Topography
expandTopography topo = 
    let (w,h) = fst . M.findMax $ topo
    in foldr M.union M.empty [ M.mapKeys (\(x,y) -> (x+i*(w+1), y+j*(h+1))) 
                             . M.map (\r -> wrap (r + manhattan (0,0) (i,j))) 
                             $ topo
                             | i <- [0 .. 4]
                             , j <- [0 .. 4]
                             ]
    where
        wrap :: Risk -> Risk
        wrap r = if r > 9 then wrap (r-9) else r

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l  = take n l : chunks n (drop n l)

plot :: Topography -> String
plot topo = 
    let (w,h) = fst . M.findMax $ topo
        grid  = [ head (show (topo M.! (y,x)))
                | y <- [0 .. h]
                , x <- [0 .. w]
                ]
    in unlines . chunks (w+1) $ grid

main :: IO ()
main = do
    let filename = "input.txt"

    input <- parse parseInput filename <$> T.readFile filename

    case input of
        Left err -> print err
        Right topography -> do
            let largeTopography = expandTopography topography

            putStrLn $ "Part one: " ++ show (dijkstra topography (0,0) (fst . M.findMax $ topography))
            putStrLn $ "Part two: " ++ show (dijkstra largeTopography (0,0) (fst . M.findMax $ largeTopography))
