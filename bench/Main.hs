--------------------------------------------------------------------------------
-- Functional Programming (CS256)                                             --
-- Coursework 1: Mastermind                                                   --
--------------------------------------------------------------------------------

import Criterion.Main

import Game

main :: IO ()
main = defaultMain [
    bgroup "validateCode" [
        bench "validate all codes" $ whnf (map validateCode) codes
    ]
    ]

--------------------------------------------------------------------------------
