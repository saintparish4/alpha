module Alpha.EngineSpec where

import Alpha.Engine
import Alpha.Types
import Data.Maybe (isJust)
import qualified Data.Vector as V
import Test.Hspec
import TestHelpers

spec :: Spec
spec = describe "Alpha.Engine" $ do
  -- 1. Buy then Sell -> exactly 1 ClosedTrade
  it "Buy then Sell produces exactly 1 ClosedTrade" $ do
    let bars = map (uncurry mkTestBar) [(0, 100), (1, 100), (2, 110)]
        strat = FixedSignals [Buy, Sell, Hold]
        result = runBacktest NoFee 10000 strat bars
    length (brClosedTrades result) `shouldBe` 1
    brOpenPosition result `shouldBe` Nothing

  -- 2. Buy with no Sell -> 1 open Position, 0 ClosedTrade
  it "Buy with no Sell leaves open position" $ do
    let bars = map (uncurry mkTestBar) [(0, 100), (1, 100)]
        strat = FixedSignals [Buy, Hold]
        result = runBacktest NoFee 10000 strat bars
    length (brClosedTrades result) `shouldBe` 0
    brOpenPosition result `shouldSatisfy` isJust

  -- 3. Fee applied exactly twice (entry + exit) per ClosedTrade
  it "fee applied exactly twice per trade" $ do
    let bars = map (uncurry mkTestBar) [(0, 100), (1, 100), (2, 110)]
        strat = FixedSignals [Buy, Sell, Hold]
        result = runBacktest (PercentFee 0.01) 10000 strat bars
        trade = head (brClosedTrades result)
    -- Entry at bar1 open=100, shares=10000/100=100
    -- entryFee = 0.01 * (100 * 100) = 100
    ctEntryFee trade `shouldBe` 100
    -- Exit at bar2 open=110, exitFee = 0.01 * (100 * 110) = 110
    ctExitFee trade `shouldBe` 110

  -- 4. Two consecutive Buy signals -> no double-entry
  it "two consecutive Buys do not double-enter" $ do
    let bars = map (uncurry mkTestBar) [(0, 100), (1, 100), (2, 105), (3, 110)]
        strat = FixedSignals [Buy, Buy, Sell, Hold]
        result = runBacktest NoFee 10000 strat bars
    length (brClosedTrades result) `shouldBe` 1

  -- 5. Sell with no open position -> no-op
  it "Sell with no position is a no-op" $ do
    let bars = map (uncurry mkTestBar) [(0, 100), (1, 105)]
        strat = FixedSignals [Sell, Hold]
        result = runBacktest NoFee 10000 strat bars
    length (brClosedTrades result) `shouldBe` 0
    brOpenPosition result `shouldBe` Nothing

  -- 6. Starting cash of 10000 -> first equity curve point is 10000
  it "first equity curve point equals initial cash" $ do
    let bars = map (uncurry mkTestBar) [(0, 100), (1, 105)]
        strat = FixedSignals [Hold, Hold]
        result = runBacktest NoFee 10000 strat bars
    V.head (brEquityCurve result) `shouldBe` 10000