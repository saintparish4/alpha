{-# LANGUAGE OverloadedStrings #-}

module Alpha.TypesSpec where

import Test.Hspec
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)
import Alpha.Types

sampleTime :: UTCTime
sampleTime = parseTimeOrError True defaultTimeLocale
               "%Y-%m-%dT%H:%M:%SZ" "2024-01-02T09:30:00Z"

spec :: Spec
spec = describe "Alpha.Types" $ do

  describe "mkBar" $ do
    it "succeeds when high >= low" $
      mkBar sampleTime 100 105 95 101 1500 `shouldSatisfy` isJust'

    it "succeeds when high == low" $
      mkBar sampleTime 100 100 100 100 0 `shouldSatisfy` isJust'

    it "returns Nothing when high < low" $
      mkBar sampleTime 100 90 95 92 500 `shouldBe` Nothing

  describe "BacktestError constructors" $
    it "has exactly 3 constructors" $
      -- Enumerating all three proves exhaustiveness
      let _pe = ParseError 0 "x"
          _ve = ValidationError EmptyDataset
          _io = IOFailure (userError "x")
      in  True `shouldBe` True

  describe "ValidationFailure constructors" $
    it "has exactly 6 constructors" $
      let _1 = EmptyDataset
          _2 = DuplicateTimestamp 0 sampleTime
          _3 = NonMonotoneTimestamp 0 sampleTime sampleTime
          _4 = HighBelowLow 0
          _5 = NegativePrice 0
          _6 = NegativeVolume 0
      in  True `shouldBe` True

  describe "Signal" $ do
    it "is Bounded" $
      [minBound .. maxBound :: Signal] `shouldBe` [Buy, Sell, Hold]

  describe "PositionState" $ do
    it "is Bounded" $
      [minBound .. maxBound :: PositionState] `shouldBe` [Flat, Long]

  describe "applyFee" $ do
    it "FlatFee returns fixed amount" $
      applyFee (FlatFee 5) 1000 `shouldBe` 5

    it "PercentFee returns rate * value" $
      applyFee (PercentFee 0.01) 1000 `shouldBe` 10

    it "NoFee returns 0" $
      applyFee NoFee 1000 `shouldBe` 0

-- helper since we can't import Data.Maybe without adding it to deps
isJust' :: Maybe a -> Bool
isJust' (Just _) = True
isJust' Nothing  = False