--------------------------------------------------------------------------------
-- Functional Programming - Mastermind Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

import Criterion.Main

import Game

--------------------------------------------------------------------------------

-- | `countMoves` @code@ counts the number of moves it takes the computer
-- to guess the given code.
countMoves :: Code -> Int
countMoves code = go 1 firstGuess codes
    where
        go n g s | code == g = n
                 | otherwise = let s' = eliminate (score code g) g s
                               in go (n+1) (nextGuess s') s'

--------------------------------------------------------------------------------

-- | `testCodes` is a list of `Code`s with which we benchmark the performance
-- of the implementation (via `countMoves`).
testCodes :: [Code]
testCodes =
    [ "aaaa"
    , "bbbb"
    , "cccc"
    , "dddd"
    , "eeee"
    , "ffff"
    , "aabb"
    , "eeff"
    , "abcd"
    , "dcba"
    ]

-- | `main` is the main entry point to the benchmark suite.
main :: IO ()
main =
    defaultMain [
        bgroup "Mastermind" $
            map (\code -> bench code $ nf countMoves code) testCodes

    ]

--------------------------------------------------------------------------------
