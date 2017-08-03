--------------------------------------------------------------------------------
-- Functional Programming (CS256)                                             --
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
            length (nub results) `shouldBe` ((pegs+1)*(pegs+2) `div` 2)
    describe "Game.score" $ do
        prop "produces valid scores" $ \(Code code) -> \(Code guess) ->
            let (c,w) = score code guess
            in c+w <= pegs && c+w >= 0
        prop "is commutative" $ \(Code code) -> \(Code guess) ->
            score code guess == score guess code
    describe "Game.nextGuess" $ do
        prop "if there is only one code left, choose it" $ \(Code code) ->
            nextGuess [code] == code
        prop "returns a valid guess" $ \cs ->
            let guess = nextGuess (map unCode cs)
            in validateCode guess && guess `elem` codes
    describe "Game.eliminate" $ do
        prop "eliminates all options which result in a different score" $
            \(Code code) -> \(Code guess) ->
                all ((==) (score code guess) . (score guess))
                (eliminate (score code guess) guess codes)

--------------------------------------------------------------------------------
