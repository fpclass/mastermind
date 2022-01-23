--------------------------------------------------------------------------------
-- Functional Programming - Mastermind Project
--------------------------------------------------------------------------------
-- Copyright (c) 2022 Michael B. Gale (michael@fpclass.online)
--
-- This source code is subject to the terms and conditions found in the LICENSE
-- file in the root directory of this source tree.
--------------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad

import Data.List ((\\), nub)

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit as Unit
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Runners.AntXML

import Hedgehog as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Game as G

--------------------------------------------------------------------------------

-- | `symbol` generates a random `Symbol`.
symbol :: MonadGen m => m Symbol
symbol = Gen.element symbols

-- `code` @range@ generates a random `Code` with a number of pegs in @range@.
genCode :: MonadGen m => Range Int -> m Code
genCode range = Gen.list range symbol

validCode :: MonadGen m => m Code
validCode = genCode $ Range.singleton pegs

-- `invalidSymbol` @validSymbols@ generates an invalid `Symbol`, where the
-- valid symbols are given by @validSymbols@.
invalidSymbol :: MonadGen m => [Symbol] -> m Symbol
invalidSymbol valid = Gen.element ([minBound..maxBound] \\ valid) -- Gen.filterT (`elem` valid) Gen.unicode

-- | `invalidCode` @size symbols@ generates an invalid code of length @size@
-- where valid characters are represented by @symbols@.
invalidCode :: MonadGen m => Int -> [Symbol] -> m Code
invalidCode 0    _  = pure []
invalidCode size xs = do
    p <- Gen.integral (Range.constant 0 (size-1))
    is <- Gen.list (Range.singleton (size-p)) (invalidSymbol xs)
    vs <- Gen.list (Range.singleton p) symbol
    Gen.shuffle (is++vs)

-- | `scoredCodes` is a list of triples of a code+a guess+an expected score.
scoredCodes :: [(Code, Code, Score)]
scoredCodes =
    [ ("aaaa", "bbbb", (0,0))
    , ("aaaa", "aaaa", (4,0))
    , ("feec", "feee", (3,0))
    , ("aabb", "bbaa", (0,4))
    , ("abbb", "caab", (1,1))
    ]

--------------------------------------------------------------------------------

-- | `countMoves` @code@ counts the number of moves it takes the computer to
-- guess @code@.
countMoves :: Code -> Int
countMoves code = go 1 firstGuess codes
    where
        go n g s | code == g = n
                 | otherwise = let s' = eliminate (score code g) g s
                               in go (n+1) (nextGuess s') s'

--------------------------------------------------------------------------------

prop_rejectWhiteMarkers :: Property
prop_rejectWhiteMarkers = property $ do
    wm <- forAll $ Gen.int (Range.constant 1 pegs)
    cm <- forAll $ Gen.int (Range.constant 0 (max 0 (pegs-wm-1)))
    H.assert $ not $ correctGuess (cm,wm)

prop_rejectTooManyColouredMarkers :: Property
prop_rejectTooManyColouredMarkers = property $ do
    cm <- forAll $ Gen.int (Range.constant (pegs+1) maxBound)
    H.assert $ not $ correctGuess (cm,0)

prop_rejectTooFewColouredMarkers :: Property
prop_rejectTooFewColouredMarkers = property $ do
    cm <- forAll $ Gen.int (Range.constant 0 (pegs-1))
    H.assert $ not $ correctGuess (cm,0)

correctGuessTests :: TestTree
correctGuessTests = testGroup "correctGuess"
    [ testProperty
        "rejects scores with white markers"
        "prop_rejectWhiteMarkers"
        prop_rejectWhiteMarkers
    , testCase
        "accepts scores with all black markers" $
        Unit.assertBool "score is not accepted" $ correctGuess (pegs,0)
    , testProperty
        "doesn't accept scores with too many coloured markers"
        "prop_rejectTooManyColouredMarkers"
        prop_rejectTooManyColouredMarkers
    , testProperty
        "doesn't accept scores with too few coloured markers"
        "prop_rejectTooFewColouredMarkers"
        prop_rejectTooFewColouredMarkers
    ]

--------------------------------------------------------------------------------

prop_rejectShortCodes :: Property
prop_rejectShortCodes = property $ do
    xs <- forAll $ genCode $ Range.constant 0 (pegs-1)
    H.assert $ not $ validateCode xs

prop_rejectLongCodes :: Property
prop_rejectLongCodes = property $ do
    xs <- forAll $ genCode $ Range.constant (pegs+1) (pegs*2)
    H.assert $ not $ validateCode xs

prop_rejectInvalidSymbols :: Property
prop_rejectInvalidSymbols = property $ do
    n <- forAll $ Gen.int $ Range.constant 0 (pegs*2)
    xs <- forAll $ invalidCode n symbols
    H.assert $ not $ validateCode xs

prop_acceptValidCodes :: Property
prop_acceptValidCodes = property $ do
    xs <- forAll validCode
    H.assert $ validateCode xs

validateCodeTests :: TestTree
validateCodeTests = testGroup "validateCode"
    [ testProperty
        "does not allow shorter codes"
        "prop_rejectShortCodes"
        prop_rejectShortCodes
    , testProperty
        "does not allow longer codes"
        "prop_rejectLongCodes"
        prop_rejectLongCodes
    , testProperty
        "does not allow invalid symbols"
        "prop_rejectInvalidSymbols"
        prop_rejectInvalidSymbols
    , testProperty
        "does allow valid codes"
        "prop_acceptValidCodes"
        prop_acceptValidCodes
    ]

--------------------------------------------------------------------------------

codesTests :: TestTree
codesTests = testGroup "codes"
    [ testCase "has the right number of codes" $
        length codes @?= (length symbols ^ pegs)
    , testCase "has no duplicate codes" $
        length codes @?= length (nub codes)
    , testCase "all codes are valid" $ forM_ codes $ \code ->
        assertBool (code <> " is not valid") (validateCode code)
    ]

--------------------------------------------------------------------------------

resultsTests :: TestTree
resultsTests = testGroup "results"
    [ testCase "all score pairs contains a valid number of markers" $
        forM_ results $ \r@(c,w) ->
            assertBool (show r <> " is not valid") $ c+w <= pegs && c+w >= 0
    , testCase "has no duplicate entries" $
        length results @?= length (nub results)
    , testCase "has the right number of results" $
        length (nub results) @?= ((pegs+1)*(pegs+2) `div` 2) - 1
    ]

--------------------------------------------------------------------------------

prop_scoreCommutes :: Property
prop_scoreCommutes = property $ do
    c <- forAll validCode
    g <- forAll validCode
    score c g === score g c

prop_scoresValid :: Property
prop_scoresValid = property $ do
    code <- forAll validCode
    guess <- forAll validCode

    let (c,w) = score code guess
    H.assert $ c+w <= pegs && c >= 0 && w >= 0

scoreTests :: TestTree
scoreTests = testGroup "score"
    [ testProperty
        "is commutative"
        "prop_scoreCommutes"
        prop_scoreCommutes
    , testProperty
        "produces valid scores"
        "prop_scoresValid"
        prop_scoresValid
    , testCase "scores samples correctly" $ forM_ scoredCodes $ \(c,g,s) ->
        s @?= score c g
    ]

--------------------------------------------------------------------------------

prop_lastCode :: Property
prop_lastCode = property $ do
    code <- forAll validCode
    nextGuess [code] === code

prop_validGuesses :: Property
prop_validGuesses = property $ do
    -- generate a list of random, valid codes
    cs <- forAll $ Gen.list (Range.constant 1 25) validCode

    -- call `nextGuess` on the list
    let guess = nextGuess cs
    annotateShow guess

    -- check that the resulting code is valid and
    -- one of the possible codes in the game
    H.assert $ validateCode guess && guess `elem` codes

nextGuessTests :: TestTree
nextGuessTests = testGroup "nextGuess"
    [ testProperty
        "if there is only one code left, choose it"
        "prop_lastCode"
        prop_lastCode
    , testProperty
        "returns valid guesses"
        "prop_validGuesses"
        prop_validGuesses
    ]

--------------------------------------------------------------------------------

prop_eliminateCodes :: Property
prop_eliminateCodes = property $ do
    code <- forAll validCode
    guess <- forAll validCode
    forM_ (eliminate (score code guess) guess codes) $ \x ->
        score guess x === score code guess

eliminateTests :: TestTree
eliminateTests = testGroup "eliminate"
    [ testProperty
        "eliminates all options which result in a different score"
        "prop_eliminateCodes"
        prop_eliminateCodes
    ]

--------------------------------------------------------------------------------

prop_fiveOrFewer :: Property
prop_fiveOrFewer = property $ do
    code <- forAll validCode
    diff (countMoves code) (<=) 5

gameTests :: TestTree
gameTests = localOption (HedgehogTestLimit $ Just 10)
    $ testGroup "nextGuess & eliminate"
    [ testCase "guesses test codes in <=5 attempts" $
        forM_ ["eaea", "eaca", "eaaa"] $ \code ->
            assertBool (code <> " took more than 5 moves") $
            countMoves code <= 5
    , testProperty
        "guesses random test codes in <=5 attempts"
        "prop_fiveOrFewer"
        prop_fiveOrFewer
    ]

--------------------------------------------------------------------------------

-- | `tests` is the main `TestTree` for the Mastermind test suite.
tests :: TestTree
tests = localOption (HedgehogShowReplay True)
     $ testGroup "Game"
     [ correctGuessTests
     , validateCodeTests
     , codesTests
     , resultsTests
     , scoreTests
     , after AllSucceed "Game.validateCode"
     $ after AllSucceed "Game.codes"
     $ after AllSucceed "Game.results"
     $ after AllSucceed "Game.score"
     $ nextGuessTests
     , after AllSucceed "Game.score"
     $ eliminateTests
     , after AllSucceed "$0 == \"Game.nextGuess\""
     $ after AllSucceed "$0 == \"Game.eliminate\""
     $ gameTests
     ]

-- | The list of tasty ingredients. Note: the order seems to matter,
-- `antXMLRunner` won't work at all if placed last in the list.
ingredients :: [Ingredient]
ingredients = [antXMLRunner, listingTests, consoleTestReporter]

-- | `main` is the main entry point to the test suite.
main :: IO ()
main = defaultMainWithIngredients ingredients tests

--------------------------------------------------------------------------------
