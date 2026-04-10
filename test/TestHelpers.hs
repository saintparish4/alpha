module TestHelpers where

import Alpha.Strategy
import Alpha.Types
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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

-- | Run a strategy over a list of bars and collect signals.
scanStrategy :: (Strategy s) => s -> [Bar] -> [Signal]
scanStrategy strat = go (initStrategy strat)
  where
    go _ [] = []
    go st (b : bs) = let (st', sig) = onBar strat st b in sig : go st' bs