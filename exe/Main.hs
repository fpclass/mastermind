--------------------------------------------------------------------------------
-- Functional Programming - Mastermind Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Control.Monad (replicateM)

import Data.Char (toLower)

import System.IO
import System.Random (getStdRandom, randomR)

import Text.Printf (printf)

import Game

--------------------------------------------------------------------------------

-- | `rollDice` is a computation which generates a random number bounded by
-- the length of `symbols`.
rollDice :: IO Int
rollDice = getStdRandom $ randomR (0, length symbols - 1)

-- | `randomPeg` is a computation which generates a random peg.
randomPeg :: IO Symbol
randomPeg = rollDice >>= \i -> pure (symbols !! i)

-- | `randomCode` is a computation which generates a random code.
randomCode :: IO Code
randomCode = replicateM pegs randomPeg

-- | `promptCode` is a computation which repeatedly prompts the player to enter a
-- code until a valid one has been entered.
promptCode :: IO Code
promptCode = do
    putStr "Enter a code: "
    code <- map toLower <$> getLine

    if validateCode code
    then return code
    else do
        putStrLn "Invalid code!"
        promptCode

-- | `humanTurn` is a computation which handles a human's turn as codemaker.
humanTurn :: IO ()
humanTurn = do
    putStrLn "You are the codemaker!"

    -- prompt the human codemaker to enter a code for the computer to guess
    code <- promptCode

    -- start the guessing loop with the hard-coded first guess and the
    -- full list of all available codes
    compLoop 1 code firstGuess codes

-- | `compLoop` @guessNumber code guess possibleCodes@ implements a step in
-- the Five-Guess algorithm, where @code@ is the code to guess, @guess@ is
-- the computer's current guess, @possibleCode@ is the list of codes that
-- could be the solution, and @guessNumber@ is a counter keeping track of
-- how many guesses the computer has taken so far.
compLoop :: Int -> Code -> Code -> [Code] -> IO ()
compLoop n code guess s
    -- is the computer's guess correct?
    | code == guess = do
        printf "%d. The computer guessed correctly (%s)!\n" n guess
    -- otherwise, eliminate all codes that can't be the right answer from
    -- the list of remaining options and determine the next code to guess
    | otherwise = do
        printf "%d. The computer guessed incorrectly (%s)!\n" n guess
        let s' = eliminate (score code guess) guess s
        compLoop (n+1) code (nextGuess s') s'

-- | `compTurn` is a computation which handles the AI's turn as codemaker.
compTurn :: IO ()
compTurn = do
    putStrLn "The computer is the codemaker!"

    -- generate a random code and let the human player guess
    code <- randomCode
    humanLoop code 1

-- | `humanLoop` @code attemtNumber@ implements a loop in which the human
-- codebreaker can enter guesses and see the resulting scores.
humanLoop :: Code -> Int -> IO ()
humanLoop code n = do
    -- prompt the player for a code
    guess <- promptCode

    -- score it
    let s = score code guess

    -- if it is correct, end the loop; otherwise, present the score to the
    -- player and loop
    if correctGuess s
    then printf "Correct after %d guesses!\n" n
    else do
        printf "Incorrect (%d coloured, %d white)!\n" (fst s) (snd s)
        humanLoop code (n+1)

-- | `takeTurns` @player@ implements a turn for the given @player@. At the end
-- of the turn, `takeTurns` is called with the opposite player.
takeTurns :: Player -> IO ()
takeTurns Human    = humanTurn >> takeTurns Computer
takeTurns Computer = compTurn  >> takeTurns Human

-- | `main` is the main entry point for the Mastermind program.
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin  NoBuffering
    takeTurns codemaker

--------------------------------------------------------------------------------
