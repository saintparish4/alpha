module Alpha.MetricsSpec where

import Alpha.Metrics
import Alpha.Types
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
  ( choose,
    forAll,
    listOf,
    property,
    resize,
    (===),
  )
import TestHelpers

spec :: Spec
spec = describe "Alpha.Metrics" $ do 

 -- ── totalReturn ───────────────────────────────────────────────
  describe "totalReturn" $ do
    it "10% gain" $
      totalReturn (V.fromList [100, 110]) `shouldBe` 0.1

    it "no change" $
      totalReturn (V.fromList [100, 100]) `shouldBe` 0

    it "empty curve returns 0" $
      totalReturn V.empty `shouldBe` 0

    it "single element returns 0" $
      totalReturn (V.fromList [100]) `shouldBe` 0

  -- ── maxDrawdown ───────────────────────────────────────────────
  describe "maxDrawdown" $ do
    it "monotone rise has no drawdown" $
      maxDrawdown (V.fromList [100, 110, 120]) `shouldBe` 0

    it "computes 20% drawdown" $
      maxDrawdown (V.fromList [100, 90, 80]) `shouldBe` (-0.2)

    it "empty curve returns 0" $
      maxDrawdown V.empty `shouldBe` 0

    modifyMaxSuccess (const 50) $
      it "result is always in [-1, 0] (QC)" $
        property $
          forAll (genEquityCurve 10) $ \curve ->
            let dd = maxDrawdown curve
            in dd >= -1 && dd <= 0

  -- ── sharpeRatio ───────────────────────────────────────────────
  describe "sharpeRatio" $ do
    it "constant equity curve returns Nothing" $
      sharpeRatio (V.replicate 5 (100 :: Scientific)) `shouldBe` Nothing

    modifyMaxSuccess (const 50) $
      it "constant equity curve returns Nothing for any length >= 2 (QC)" $
        property $
          forAll (choose (2, 50)) $ \n ->
            forAll genPrice $ \p ->
              sharpeRatio (V.replicate n p) === Nothing

    it "single element returns Nothing" $
      sharpeRatio (V.fromList [100]) `shouldBe` Nothing

    it "empty curve returns Nothing" $
      sharpeRatio V.empty `shouldBe` Nothing

  -- ── profitFactor ──────────────────────────────────────────────
  describe "profitFactor" $ do
    it "empty list returns 0" $
      profitFactor [] `shouldBe` 0

    it "QC: profitFactor [] == 0 always" $
      profitFactor [] `shouldBe` (0 :: Scientific)

  -- ── winRate ───────────────────────────────────────────────────
  describe "winRate" $ do
    it "empty list returns 0" $
      winRate [] `shouldBe` 0

    modifyMaxSuccess (const 50) $
      it "result is in [0, 1] for any trade list (QC)" $
        property $
          forAll (resize 40 (listOf genClosedTrade)) $ \trades ->
            let wr = winRate trades
            in wr >= 0 && wr <= 1

  -- ── computeMetrics ────────────────────────────────────────────
  describe "computeMetrics" $ do
    it "returns defined MetricsResult for a result with no trades" $ do
      let br = BacktestResult
                 { brClosedTrades = []
                 , brOpenPosition = Nothing
                 , brEquityCurve  = V.fromList [10000, 10000, 10000]
                 , brBarCount     = 2
                 }
          mr = computeMetrics br
      mrTotalReturn mr `shouldBe` 0
      mrMaxDrawdown mr `shouldBe` 0
      mrWinRate mr `shouldBe` 0
      mrProfitFactor mr `shouldBe` 0

    it "constant equity -> sharpe is Nothing" $ do
      let br = BacktestResult
                 { brClosedTrades = []
                 , brOpenPosition = Nothing
                 , brEquityCurve  = V.replicate 10 10000
                 , brBarCount     = 9
                 }
      mrSharpe (computeMetrics br) `shouldBe` Nothing