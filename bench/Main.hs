--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Mastermind                                                   --
--------------------------------------------------------------------------------

import Criterion.Main

import Game

--------------------------------------------------------------------------------

-- | Counts the number of moves it takes the computer to guess the given code.
countMoves :: Code -> Int
countMoves code = go 1 firstGuess codes
    where
        go n g s | code == g = n
                 | otherwise = let s' = eliminate (score code g) g s
                               in go (n+1) (nextGuess s') s'

--------------------------------------------------------------------------------

testCodes :: [Code]
testCodes = [ "aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff",
    "aabb", "eeff", "abcd", "dcba" ]

main :: IO ()
main = defaultMain [
    bgroup "Mastermind" [
        bench "play test games" $ nf (map countMoves) testCodes
    ]
    ]

--------------------------------------------------------------------------------
