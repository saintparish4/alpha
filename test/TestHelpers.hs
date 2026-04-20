{-# LANGUAGE TypeFamilies #-}

module TestHelpers where

import Alpha.Strategy
import Alpha.Types
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector as V
import Test.QuickCheck

-- | Arbitrary valid Bar with high >= low and non-negative fields.
genValidBar :: UTCTime -> Gen Bar
genValidBar ts = do
  o <- genPrice
  l <- genPrice
  h <- (l +) <$> genPrice -- guarantees h >= l
  frac <- fromFloatDigits <$> (choose (0.0, 1.0) :: Gen Double)
  let c = l + frac * (h - l)
  v <- genVolume
  pure
    Bar
      { barTimestamp = ts,
        barOpen = o,
        barHigh = h,
        barLow = l,
        barClose = c,
        barVolume = v
      }

-- | Generate a chronological series of valid bars.
genBarSeries :: Int -> Gen [Bar]
genBarSeries n = do
  let epoch = posixSecondsToUTCTime 0
      times =
        [ addUTCTime (secondsToNominalDiffTime (fromIntegral i * 3600)) epoch
          | i <- [0 .. n - 1]
        ]
  mapM genValidBar times

genPrice :: Gen Scientific
genPrice = fromFloatDigits <$> (choose (1.0, 500.0) :: Gen Double)

genVolume :: Gen Scientific
genVolume = fromFloatDigits <$> (choose (0.0, 10000.0) :: Gen Double)

-- | Build a bar at a given hour offset with all OHLC fields equal to the
--   supplied price. Useful for tests that need precise control over
--   open/close prices.
mkTestBar :: Int -> Scientific -> Bar
mkTestBar hourIdx price =
  Bar
    { barTimestamp =
        addUTCTime
          (secondsToNominalDiffTime (fromIntegral hourIdx * 3600))
          epoch,
      barOpen = price,
      barHigh = price,
      barLow = price,
      barClose = price,
      barVolume = 1000
    }
  where
    epoch = posixSecondsToUTCTime 0

-- ── Generators for metrics / equity ───────────────────────────────

-- | Generate a single ClosedTrade with randomised prices and fees.
genClosedTrade :: Gen ClosedTrade
genClosedTrade = do
  hourOffset <- choose (0, 1000 :: Int)
  entryPrice <- genPrice
  exitPrice <- genPrice
  shares <- genPrice
  entryFee <- fromFloatDigits <$> (choose (0.0, 5.0) :: Gen Double)
  exitFee <- fromFloatDigits <$> (choose (0.0, 5.0) :: Gen Double)
  let entryTime =
        addUTCTime (secondsToNominalDiffTime (fromIntegral hourOffset * 3600)) epoch
      exitTime =
        addUTCTime (secondsToNominalDiffTime (fromIntegral (hourOffset + 1) * 3600)) epoch
  pure
    ClosedTrade
      { ctEntryTime = entryTime,
        ctEntryPrice = entryPrice,
        ctExitTime = exitTime,
        ctExitPrice = exitPrice,
        ctShares = shares,
        ctEntryFee = entryFee,
        ctExitFee = exitFee
      }
  where
    epoch = posixSecondsToUTCTime 0

-- | Generate an equity curve of length n with positive prices.
genEquityCurve :: Int -> Gen (V.Vector Scientific)
genEquityCurve n = V.fromList <$> vectorOf n genPrice

-- | A test-only strategy that replays a fixed list of signals in order,
--   returning Hold once the list is exhausted.
newtype FixedSignals = FixedSignals [Signal]

instance Strategy FixedSignals where
  type State FixedSignals = [Signal]
  initStrategy (FixedSignals sigs) = sigs
  onBar _ [] _ = ([], Hold)
  onBar _ (s : ss) _ = (ss, s)

-- | Run a strategy over a list of bars and collect signals.
scanStrategy :: (Strategy s) => s -> [Bar] -> [Signal]
scanStrategy strat = go (initStrategy strat)
  where
    go _ [] = []
    go st (b : bs) = let (st', sig) = onBar strat st b in sig : go st' bs