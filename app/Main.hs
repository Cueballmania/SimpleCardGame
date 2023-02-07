module Main (main) where

import Deck (Card, Deck, fullDeck, cardValue)
import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Text.Read (readMaybe)

data Game = Game
    { deck :: Deck
    , discard :: Deck
    , table :: Deck
    , gen :: StdGen}
    deriving (Show)

type GameT m = StateT Game m

printTableValue :: [Card] -> IO ()
printTableValue cs = putStrLn $ "The value of cards is " ++ show vals
    where vals = sum [cardValue c | c <- cs]

shuffleDeck :: Monad m => GameT m ()
shuffleDeck = do
    game <- get
    let (shufDeck, gen') = shuffle (deck game) (gen game)
    let game' = game {deck = shufDeck, gen = gen'}
    put game'

shuffle :: Deck -> StdGen -> (Deck, StdGen)
shuffle [] g = ([], g)
shuffle dk g =
    let (index, newGen) = randomR (0, length dk - 1) g
        card = dk !! index
        rest = take index dk ++ drop (index + 1) dk
    in (card : fst (shuffle rest newGen), newGen)

drawN :: Monad m => Int -> GameT m ()
drawN n = do
    g <- get
    let deck' = deck g
    let tabled = table g
    let discarded = discard g
    let (drawnCards, remainingDeck) = splitAt n deck'
    put $ g { deck = remainingDeck, table = drawnCards, discard = tabled ++ discarded}

clearTable :: Monad m => GameT m ()
clearTable = do
    g <- get
    let tabled = table g
    let discarded = discard g
    put $ g {table = [], discard = tabled ++ discarded}

gameLoop :: GameT IO ()
gameLoop = do
    g <- get
    lift $ putStrLn "How many cards would you like to draw?"
    drawStr <- lift getLine
    case readMaybe drawStr of
        Nothing -> 
            if drawStr == "quit" then lift $ return ()
            else do
                lift $ putStrLn "Invalid input. Please enter a number."
                gameLoop
        Just draw -> do
            let remaining = length $ deck g
            if draw > remaining
                then lift $ putStrLn $ "Not enough cards left to draw " ++ 
                            show draw ++ ". Only " ++ show remaining ++ " cards remaining."
                else do
                    drawN draw
                    printTable
                    gameLoop

printTable :: GameT IO ()
printTable = do
    g <- get
    let tabled = table g
    lift $ putStrLn $ "Table: " ++ show tabled
    lift $ printTableValue tabled

main :: IO ()
main = do
    putStrLn "Enter a seed value: "
    seedStr <- getLine
    let seed = read seedStr :: Int
    let initGen = mkStdGen seed
    let initialGame = Game {deck = fullDeck, discard = [], table = [], gen = initGen}
    _ <- execStateT (shuffleDeck >> gameLoop) initialGame
    putStrLn "Thanks for playing"
