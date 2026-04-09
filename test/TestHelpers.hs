module TestHelpers where

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