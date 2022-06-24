{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad.State
import           Data.Char
import           Data.List
import           Data.Maybe
import           System.IO
import           System.Process

{----------------------------------------------------
The game of Hangman consists of:
    
    * a secret word entered by player 1

    * board of '_' of length (secret word) to be filled
    in by player 2

    * a list of (incorrect) guessed characters by player 2

Player 2 is allowed 7 incorrect guesses
(the length of "hangman")

These 3 pieces constitute a puzzle to be solved
The puzzle represents the state of the game
-----------------------------------------------------}
main :: IO ()
main = do
    -- set up buffering to skip the Enter key while entering letters
    mapM_ (`hSetBuffering` NoBuffering) [stdout, stdin]

    putStr "Player 1, enter a secret word: "
    secret <- secretGetLine
    putStrLn "Player 2, try to guess it!"
    runStateT game $ newPuzzle secret
    return ()


game :: StateT Puzzle IO ()
game = do
    -- clear the screen
    lift $ callCommand "clear"

    -- get the puzzle and print it
    p <- get
    lift $ print p

    -- did we win? lose? continue?
    let winner (Puzzle s b _) = s == catMaybes b
        loser (Puzzle _ _ g) = length g == length "hangman"
        lostMsg = "You lose! The secret word was " ++ secret p

    if winner p
        then lift $ putStrLn "Congrats, you got it!"
        else if loser p
            then lift $ putStrLn lostMsg
            else do
                c <- lift getChar
                play c
                game


play :: MonadState Puzzle m => Char -> m ()
play c = do
    p <- get
    if elem c (secret p) && notElem (Just c) (board p)
        then put $ p { board = updateBoard (board p) c (secret p) }
        else if notElem c (guessed p)
            then put $ p { guessed = c : guessed p }
            else return ()


updateBoard :: Board -> Char -> String -> Board
updateBoard []            _ _        = []
updateBoard (Just m : xs) c (k : ks) = Just m : updateBoard xs c ks
updateBoard (Nothing : xs) c (k : ks)
    | c == k    = Just c : updateBoard xs c ks
    | otherwise = Nothing : updateBoard xs c ks


{-----------------------------------------------------
define the basic data types
------------------------------------------------------}

type Board = [Maybe Char]

data Puzzle = Puzzle
    { secret  :: String
    , board   :: Board
    , guessed :: String
    }

{-----------------------------------------------------
define a "pretty printer" for type Puzzle
rather than deriving Show, we define what it means
to show Puzzle explicitly
------------------------------------------------------}
instance Show Puzzle where

    show (Puzzle s b g) = concat
        [ "\n"
        , "Lives:   "
        , space showTitle
        , "\n"
        , "\n"
        , "Board:   "
        , space showBoard
        , "\n"
        , "\n"
        , "Guessed: "
        , space showGuessed
        , "\n"
        ]

      where
        showBoard   = fromMaybe '_' <$> b
        showTitle   = take (length g) "Hangman"
        showGuessed = sort g
        space       = intersperse ' '


{-----------------------------------------------------
other helper functions
------------------------------------------------------}

newPuzzle :: String -> Puzzle
newPuzzle s = Puzzle secret board guessed
  where
    board   = const Nothing <$> s
    secret  = toLower <$> s
    guessed = []


secretGetLine :: IO String
secretGetLine = do
    x <- secretGetChar
    if x == '\n'
        then do
            putChar x
            return []
        else do
            putChar '-'
            xs <- secretGetLine
            return (x : xs)


secretGetChar :: IO Char
secretGetChar = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x