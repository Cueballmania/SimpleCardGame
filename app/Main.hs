module Main (main) where

import Deck
import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

data Game = Game
     { deck :: Deck
     , discard :: Deck
     , table :: Deck
     , gen :: StdGen}
     deriving (Show)

type GameT = StateT Game IO

shuffleDeck :: GameT ()
shuffleDeck = do
     game <- get
     let (shufDeck, gen') = shuffle (deck game) (gen game)
     let game' = game {deck = shufDeck, gen = gen'}
     put game'

shuffle :: Deck -> StdGen -> (Deck, StdGen)
shuffle [] gen = ([], gen)
shuffle deck gen =
     let (index, newGen) = randomR (0, length deck - 1) gen
         card = deck !! index
         rest = take index deck ++ drop (index+1) deck
     in (card : fst (shuffle rest newGen), newGen)

drawN :: Int -> GameT ()
drawN n = do
     g <- get
     let deck' = deck g
     let (drawnCards, remainingDeck) = splitAt n deck'
     put $ g { deck = remainingDeck, table = drawnCards}

clearTable :: GameT ()
clearTable = do
     g <- get
     let tabled = table g
     let discarded = discard g
     put $ g {table = [], discard = tabled ++ discarded}

gameLoop :: GameT ()
gameLoop = do
     g <- get
     lift $ putStrLn "How many cards would you like to draw?"
     drawStr <- lift getLine
     let draw = read drawStr :: Int
     let remaining = length $ deck g
     if draw > remaining
           then do
                lift $ putStrLn "Not enough cards left to draw that many cards."
                return ()
     else do
           drawN draw
           g' <- get
           lift $ putStrLn $ "Table: " ++ show (table g')
           gameLoop

main :: IO ()
main = do
     putStrLn "Enter a seed value: "
     seedStr <- getLine
     let seed = read seedStr :: Int
     let initGen = mkStdGen seed
     let initialGame = Game {deck = fullDeck, discard = [], table = [], gen = initGen}
     execStateT (shuffleDeck >> gameLoop) initialGame
     putStrLn "Thanks for playing"
