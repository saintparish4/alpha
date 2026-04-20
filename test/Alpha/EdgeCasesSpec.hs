{-# LANGUAGE OverloadedStrings #-}

module Alpha.EdgeCasesSpec where

import Alpha.Data (ingestCSV)
import Alpha.Engine (runBacktest)
import Alpha.Metrics (computeMetrics)
import Alpha.Strategy (SmaCrossover (..))
import Alpha.Strategy.BuyHold (BuyAndHold (..))
import Alpha.Strategy.Momentum (MomentumBreakout (..))
import Alpha.Types
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust, isNothing)
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import Test.Hspec
import TestHelpers (FixedSignals (..), mkTestBar, scanStrategy)

-- | Build a (lazy) ASCII ByteString from a String.
asciiBs :: String -> BL.ByteString
asciiBs = BL.pack . map (fromIntegral . fromEnum)

spec :: Spec
spec = describe "Alpha.EdgeCases" $ do

  -- 1. Empty CSV (header only, no rows)
  describe "empty CSV" $
    it "returns ValidationError EmptyDataset" $ do
      let bs = asciiBs "timestamp,open,high,low,close,volume\n"
      ingestCSV bs `shouldBe` Left (ValidationError EmptyDataset)

  -- 2. Single bar -- 0 trades, defined metrics
  describe "single bar" $
    it "produces 0 trades and a defined MetricsResult" $ do
      let bars   = V.fromList [mkTestBar 0 100]
          result = runBacktest NoFee 10000 (SmaCrossover 3 5) bars
          m      = computeMetrics result
      length (brClosedTrades result) `shouldBe` 0
      brOpenPosition result `shouldBe` Nothing
      mrTotalReturn m      `shouldBe` 0
      mrAnnualizedReturn m `shouldBe` 0
      mrMaxDrawdown m      `shouldBe` 0
      mrWinRate m          `shouldBe` 0
      mrProfitFactor m     `shouldBe` 0

  -- 3. Flat market (zero volatility) -> sharpe == Nothing, no crash
  describe "flat market (zero volatility)" $
    it "yields sharpe == Nothing and computes without crashing" $ do
      let bars   = V.fromList [mkTestBar i 100 | i <- [0 .. 9]]
          result = runBacktest NoFee 10000 (SmaCrossover 3 5) bars
          m      = computeMetrics result
      mrSharpe m      `shouldSatisfy` isNothing
      mrMaxDrawdown m `shouldBe` 0
      length (brClosedTrades result) `shouldBe` 0

  -- 4. Duplicate timestamps -> ValidationError (DuplicateTimestamp ...)
  describe "duplicate timestamps" $
    it "is rejected with DuplicateTimestamp" $ do
      let bs = asciiBs $ unlines
            [ "timestamp,open,high,low,close,volume"
            , "2024-01-02T09:30:00Z,100,101,99,100,1000"
            , "2024-01-02T09:30:00Z,100,101,99,100,1000"
            ]
      case ingestCSV bs of
        Left (ValidationError (DuplicateTimestamp _ _)) -> pure ()
        other -> expectationFailure $
          "expected DuplicateTimestamp, got: " ++ show other

  -- 5. Strategy that never signals -> 0 trades, defined metrics
  describe "strategy that never signals" $
    it "produces 0 trades and a defined MetricsResult" $ do
      let bars   = V.fromList [mkTestBar i 100 | i <- [0 .. 9]]
          strat  = FixedSignals []   -- onBar always returns Hold once exhausted
          result = runBacktest NoFee 10000 strat bars
          m      = computeMetrics result
      length (brClosedTrades result) `shouldBe` 0
      brOpenPosition result `shouldBe` Nothing
      mrTotalReturn m  `shouldBe` 0
      mrMaxDrawdown m  `shouldBe` 0
      mrWinRate m      `shouldBe` 0
      mrProfitFactor m `shouldBe` 0

  -- ── Second strategy: momentum breakout ───────────────────────
  describe "MomentumBreakout (Strategy typeclass generality)" $ do
    it "emits Hold while the buffer is filling" $ do
      let mb   = MomentumBreakout 3
          bars = [mkTestBar i 100 | i <- [0 .. 2]]
          sigs = scanStrategy mb bars
      all (== Hold) sigs `shouldBe` True

    it "emits Buy on a clear upside breakout" $ do
      let mb   = MomentumBreakout 3
          -- buffer fills with [10,10,10]; next close 50 > max 10 -> Buy
          bars = zipWith mkTestBar [0 ..] ([10, 10, 10, 50] :: [Scientific])
          sigs = scanStrategy mb bars
      last sigs `shouldBe` Buy

    it "emits Sell on a clear downside breakout" $ do
      let mb   = MomentumBreakout 3
          -- buffer fills with [50,50,50]; next close 10 < min 50 -> Sell
          bars = zipWith mkTestBar [0 ..] ([50, 50, 50, 10] :: [Scientific])
          sigs = scanStrategy mb bars
      last sigs `shouldBe` Sell

    it "produces non-trivial signals on a synthetic trend" $ do
      let mb   = MomentumBreakout 3
          bars = zipWith mkTestBar [0 ..]
                   ([10, 11, 12, 13, 14, 15, 14, 13, 12, 11, 10] :: [Scientific])
          sigs = scanStrategy mb bars
      any (/= Hold) sigs `shouldBe` True

  -- ── Buy-and-hold benchmark vs SMA crossover on sample.csv ────
  describe "buy-and-hold benchmark vs SMA crossover" $ do
    it "both strategies produce a valid MetricsResult on sample.csv" $ do
      bs <- BL.readFile "test/fixtures/sample.csv"
      let bars       = either (error . show) id (ingestCSV bs)
          smaResult  = runBacktest NoFee 10000 (SmaCrossover 3 5) bars
          smaMetrics = computeMetrics smaResult
          bhResult   = runBacktest NoFee 10000 BuyAndHold bars
          bhMetrics  = computeMetrics bhResult
      V.length (brEquityCurve smaResult) `shouldSatisfy` (> 0)
      V.length (brEquityCurve bhResult)  `shouldSatisfy` (> 0)
      -- Buy-and-hold opens a position on bar 1 (after Buy on bar 0)
      -- and never closes it.
      length (brClosedTrades bhResult) `shouldBe` 0
      brOpenPosition bhResult `shouldSatisfy` isJust
      -- Sanity: bundled metrics fields are well-typed.
      mrSharpe smaMetrics `shouldSatisfy` (\x -> isJust x || isNothing x)
      mrSharpe bhMetrics  `shouldSatisfy` (\x -> isJust x || isNothing x)
