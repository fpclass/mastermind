--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Mastermind                                                   --
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Control.Monad                (replicateM)

import Data.Char                    (toLower)

import System.IO
import System.Random

import Text.Printf                  (printf)

import Game

--------------------------------------------------------------------------------

-- | A computation which generates random numbers.
rollDice :: IO Int
rollDice = getStdRandom $ randomR (0, length symbols - 1)

-- | A computation which generates a random peg.
randomPeg :: IO Symbol
randomPeg = rollDice >>= \i -> return (symbols !! i)

-- | A computation which generates a random code.
randomCode :: IO Code
randomCode = replicateM pegs randomPeg

-- | Prompts the user to enter a code until a valid one has been entered.
promptCode :: IO Code
promptCode = do
    putStr "Enter a code: "
    code <- map toLower <$> getLine

    if validateCode code
    then return code
    else do
        putStrLn "Invalid code!"
        promptCode

-- | Handles a human's turn as codemaker.
humanTurn :: IO ()
humanTurn = do
    putStrLn "You are the codemaker!"
    code <- promptCode
    compLoop 1 code firstGuess codes

compLoop :: Int -> Code -> Code -> [Code] -> IO ()
compLoop n code guess s
    | code == guess = do
        printf "%d. The computer guessed correctly (%s)!\n" n guess
    | otherwise     = do
        printf "%d. The computer guessed incorrectly (%s)!\n" n guess
        let s' = eliminate (score code guess) guess s
        compLoop (n+1) code (nextGuess s') s'

-- | Handles the AI's turn as codemaker.
compTurn :: IO ()
compTurn = do
    putStrLn "The computer is the codemaker!"
    code <- randomCode
    humanLoop code 1

humanLoop :: Code -> Int -> IO ()
humanLoop code n = do
    guess <- promptCode

    let s = score code guess

    if correctGuess s
    then printf "Correct after %d guesses!\n" n
    else do
        printf "Incorrect (%d coloured, %d white)!\n" (fst s) (snd s)
        humanLoop code (n+1)

-- | The human and AI take turns as codemaker.
takeTurns :: Player -> IO ()
takeTurns Human    = humanTurn >> takeTurns Computer
takeTurns Computer = compTurn  >> takeTurns Human

-- | The main entry point for this program.
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering
    takeTurns codemaker

--------------------------------------------------------------------------------
