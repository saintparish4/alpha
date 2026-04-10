module Alpha.StrategySpec where

import Alpha.Strategy
import Alpha.Strategy.Helpers
import Alpha.Types
import Data.Scientific (Scientific, fromFloatDigits)
import Test.Hspec
import Test.QuickCheck
import TestHelpers

spec :: Spec 
spec = do 
  -- ── resolveAction truth table (6 combinations) ─────────
  describe "resolveAction" $ do
    it "(Buy,  Flat) -> Enter" $
      resolveAction Buy Flat `shouldBe` Enter
    it "(Buy,  Long) -> DoNothing" $
      resolveAction Buy Long `shouldBe` DoNothing
    it "(Sell, Flat) -> DoNothing" $
      resolveAction Sell Flat `shouldBe` DoNothing
    it "(Sell, Long) -> Exit" $
      resolveAction Sell Long `shouldBe` Exit
    it "(Hold, Flat) -> DoNothing" $
      resolveAction Hold Flat `shouldBe` DoNothing
    it "(Hold, Long) -> DoNothing" $
      resolveAction Hold Long `shouldBe` DoNothing

  -- ── RunningSum helper ───────────────────────────────────
  describe "RunningSum" $ do
    it "sum after N pushes matches naive sum" $
      property $
        forAll (choose (1, 200)) $ \n ->
          forAll (vectorOf n (choose (0.1, 100.0) :: Gen Double)) $ \doubles ->
            let vals = map fromFloatDigits doubles :: [Scientific]
                rs = foldl (flip pushValue) (emptyRunningSum n) vals
             in currentSum rs == sum vals

    it "average of full buffer is correct" $ do
      let rs = foldl (flip pushValue) (emptyRunningSum 3) [10, 20, 30 :: Scientific]
      currentAverage rs `shouldBe` Just 20

    it "drops oldest value when buffer overflows" $ do
      let rs = foldl (flip pushValue) (emptyRunningSum 3) [10, 20, 30, 40 :: Scientific]
      currentSum rs `shouldBe` 90
      currentAverage rs `shouldBe` Just 30

    it "rsFull is False before capacity reached" $ do
      let rs = pushValue 1 (emptyRunningSum 3)
      rsFull rs `shouldBe` False

    it "rsFull is True at capacity" $ do
      let rs = foldl (flip pushValue) (emptyRunningSum 2) [1, 2 :: Scientific]
      rsFull rs `shouldBe` True

  -- ── SMA Crossover ──────────────────────────────────────
  describe "SmaCrossover" $ do
    it "emits Hold when buffers are not full" $ do
      let sma = SmaCrossover 3 5
          st0 = initStrategy sma
          (_, sig) = onBar sma st0 (mkTestBar 0 100)
      sig `shouldBe` Hold

    it "emits Buy when short SMA > long SMA (rising prices)" $ do
      let sma = SmaCrossover 2 3
          bars = zipWith mkTestBar [0 ..] [10, 10, 10, 20, 30 :: Scientific]
          sigs = scanStrategy sma bars
      last sigs `shouldBe` Buy

    it "emits Sell when short SMA < long SMA (falling prices)" $ do
      let sma = SmaCrossover 2 3
          bars = zipWith mkTestBar [0 ..] [30, 30, 30, 20, 10 :: Scientific]
          sigs = scanStrategy sma bars
      last sigs `shouldBe` Sell

    it "emits valid signals on random bar series" $
      property $
        forAll (genBarSeries 20) $ \bars ->
          let sma = SmaCrossover 3 5
              sigs = scanStrategy sma bars
           in length sigs == length bars
                && all (`elem` [Buy, Sell, Hold]) sigs