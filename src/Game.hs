--------------------------------------------------------------------------------
-- Functional Programming - Mastermind Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

-- | This module should contain your game code.
module Game where

--------------------------------------------------------------------------------

-- | The number of pegs in a code. You can change this value if you want to
-- play games with more than four pegs. You should make sure that you reference
-- this definition in your code instead of hard-coding the number 4 or writing
-- code that assumes this value is 4.
pegs :: Int
pegs = 4

-- | Symbols are represented by characters.
type Symbol = Char

-- | The available symbols in the game.
symbols :: [Symbol]
symbols = ['a'..'f']

-- | A code is represented by a list of symbols.
type Code = [Symbol]

-- | Guesses are scored using coloured and white markers. The first component
-- of the pair gives the number of coloured markers and the right component
-- gives the number of white markers.
type Score = (Int, Int)

-- | A player is either human or computer-controlled.
data Player = Human | Computer

-- | The first codemaker in a play session.
codemaker :: Player
codemaker = Human

-- | The first guess the AI will make.
firstGuess :: Code
firstGuess = "aabb"

--------------------------------------------------------------------------------

-- | `correctGuess` @score@ determines whether @score@ indicates that a guess
-- was correct or not. For example:
--
-- >>> correctGuess (pegs, 0)
-- True
--
-- >>> correctGuess (3, 1)
-- False
------------------------------------
-- [Your explanation]
correctGuess :: Score -> Bool
correctGuess score = undefined

-- | `validateCode` @code@ checks that the code entered by a human player is
-- valid. In other words, it should have the length given by `pegs` and it
-- should only contain valid symbols. For example (assuming default
-- definitions of `pegs` and `symbols`):
--
-- >>> validateCode "aabb"
-- True
--
-- >>> validateCode "zzyy"
-- False
--
-- >>> validateCode "ab"
-- False
------------------------------------
-- [Your explanation]
validateCode :: Code -> Bool
validateCode xs = undefined

-- | `codes` is a list of all possible codes. For example, assuming default
-- definitions of `pegs` and `symbols`:
--
-- >>> codes
-- ["aaaa", "aaab", "aaac", "aaad", .., "fffe", "ffff"]
------------------------------------
-- [Your explanation]
codes :: [Code]
codes = undefined

-- | `results` is a list of all possible scores. Assuming default definitions
-- of `pegs`:
--
-- >>> results
-- [ (0,0), (0,1), (0,2), (0,3), (0,4), (1,0), (1,1), (1,2), (1,3), (2,0)
-- , (2,1), (2,2), (3,0), (4,0)
-- ]
------------------------------------
-- [Your explanation]
results :: [Score]
results = undefined

-- | `score` @code guess@ scores @guess@ against @code@. Symbols which are
-- in the right place and of the right type score a coloured marker. Symbols
-- which are of the right type but in the wrong place score a white marker.
--
-- >>> score "abcd" "aabb"
-- (1,1)
------------------------------------
-- [Your explanation]
score :: Code -> Code -> Score
score code guess = undefined

-- | `nextGuess` @remainingOptions@ chooses the next guess from
-- @remainingOptions@. If there is only one option left, choose it.
-- Otherwise, calculate the hit score for each code and choose the code
-- with the largest hit score.
------------------------------------
-- [Your explanation]
nextGuess :: [Code] -> Code
nextGuess s = undefined

-- | `eliminate` @lastScore guess remainingOptions@ returns a list of `Code`s
-- in which all codes from @remainingOptions@ which would result in
-- a different score for @guess@ if they were the code are removed.
-- In other words, given the set of remaining possible codes, narrow it down
-- to those which would produce the same score we got from the codemaker.
------------------------------------
-- [Your explanation]
eliminate :: Score -> Code -> [Code] -> [Code]
eliminate lastScore guess codes = undefined

--------------------------------------------------------------------------------
