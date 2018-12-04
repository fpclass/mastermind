--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Mastermind                                                   --
--------------------------------------------------------------------------------

import Control.Applicative ((<$>))
import Control.Monad
import Data.List ((\\), nub)
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Game

--------------------------------------------------------------------------------

-- | `randomCode` @l @xs generates a code of length @l using
-- symbols from @xs.
randomCode :: Int -> [Symbol] -> IO Code
randomCode l xs = replicateM l randomPeg
    where
        randomPeg = do
            i <- getStdRandom $ randomR (0, length xs - 1)
            return (xs !! i)

-- | Custom code type so we can have arbitrary instances of them.
newtype CodeT   = Code   { unCode :: Code }

instance Arbitrary CodeT where
    arbitrary = Code <$> vectorOf pegs (arbitrary `suchThat`
        \x -> elem x symbols)

instance Show CodeT where
    show = show . unCode

-- | Some code/guess pairs with expected scores.
scoredCodes :: [(Code, Code, Score)]
scoredCodes =
    [ ("aaaa", "bbbb", (0,0))
    , ("aaaa", "aaaa", (4,0))
    , ("feec", "feee", (3,0))
    , ("aabb", "bbaa", (0,4))
    , ("abbb", "caab", (1,1))
    ]

--------------------------------------------------------------------------------

-- | Counts the number of moves it takes the computer to guess the given code.
countMoves :: Code -> Int
countMoves code = go 1 firstGuess codes
    where
        go n g s | code == g = n
                 | otherwise = let s' = eliminate (score code g) g s
                               in go (n+1) (nextGuess s') s'

--------------------------------------------------------------------------------

-- | The main entry point to the test suite.
main :: IO ()
main = hspec $ do
    describe "Game.correctGuess" $ do
        it "rejects scores with white markers" $
            (pegs-1,1) `shouldNotSatisfy` correctGuess
        it "accepts scores with all black markers" $
            (pegs,0) `shouldSatisfy` correctGuess
        it "doesn't accept scores with too many black markers" $
            (pegs+1,0) `shouldNotSatisfy` correctGuess
        it "doesn't accept scores with too few black markers" $
            (pegs-1,0) `shouldNotSatisfy` correctGuess
    describe "Game.validateCode" $ do
        it "does not allow shorter codes" $ do
            code <- randomCode (pegs-1) symbols
            code `shouldNotSatisfy` validateCode
        it "does not allow longer codes" $ do
            code <- randomCode (pegs+1) symbols
            code `shouldNotSatisfy` validateCode
        it "does not allow invalid symbols" $ do
            code <- randomCode pegs ([minBound..maxBound] \\ symbols)
            code `shouldNotSatisfy` validateCode
        prop "does allow valid codes" $
            \(Code code) -> validateCode code
    describe "Game.codes" $ do
        it "has the right number of codes" $
            length codes `shouldBe` (length symbols ^ pegs)
        it "has no duplicate codes" $
            length codes `shouldBe` length (nub codes)
        it "all codes are valid" $
            codes `shouldSatisfy` all validateCode
    describe "Game.results" $ do
        it "contain the right number of markers" $
            results `shouldSatisfy` all (
                \(c,w) -> c+w <= pegs && c+w >= 0)
        it "has no duplicate results" $
            length results `shouldBe` length (nub results)
        it "has the right number of results" $
            length (nub results) `shouldBe` ((pegs+1)*(pegs+2) `div` 2) - 1
    describe "Game.score" $ do
        prop "is commutative" $ \(Code code) -> \(Code guess) ->
            score code guess == score guess code
        prop "produces valid scores" $ \(Code code) -> \(Code guess) ->
            let (c,w) = score code guess
            in c+w <= pegs && c >= 0 && w >= 0
        it "scores correctly" $
            scoredCodes `shouldSatisfy` all (\(c,g,s) -> score c g == s)
    describe "Game.nextGuess" $ do
        prop "if there is only one code left, choose it" $ \(Code code) ->
            nextGuess [code] == code
        prop "returns a valid guess" $ forAll (listOf1 arbitrary) $ \cs ->
            let guess = nextGuess (map unCode cs)
            in validateCode guess && guess `elem` codes
    describe "Game.eliminate" $ do
        prop "eliminates all options which result in a different score" $
            \(Code code) -> \(Code guess) ->
                all ((==) (score code guess) . (score guess))
                (eliminate (score code guess) guess codes)
    describe "Game" $ modifyMaxSuccess (const 10) $ do
        it "correctly guesses codes in 5 or fewer attempts which are known to cause problems" $
            all (<= 5) $ map countMoves ["eaea", "eaca", "eaaa"]
        prop "computer correctly guesses codes in five or fewer attempts" $
            \(Code code) -> countMoves code <= 5

--------------------------------------------------------------------------------
