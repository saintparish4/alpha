{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Alpha.Data
  ( parseCSV
  , validateBars
  , ingestCSV
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv             as Csv
import qualified Data.Vector          as V
import Data.Text        (pack)
import Data.Time        (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

import Alpha.Types

-- ── Cassava instances ───────────────────────────────────────────

instance Csv.FromNamedRecord RawBar where
  parseNamedRecord m = RawBar
    <$> m Csv..: "timestamp"
    <*> m Csv..: "open"
    <*> m Csv..: "high"
    <*> m Csv..: "low"
    <*> m Csv..: "close"
    <*> m Csv..: "volume"

instance Csv.FromField UTCTime where
  parseField s = case Csv.runParser (Csv.parseField s :: Csv.Parser String) of
    Left err  -> fail err
    Right str ->
      case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" str of
        Just t  -> pure t
        Nothing -> case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" str of
          Just t  -> pure t
          Nothing -> fail $ "Cannot parse timestamp: " ++ str

-- ── Public API ──────────────────────────────────────────────────

parseCSV :: BL.ByteString -> Either BacktestError [RawBar]
parseCSV bs = case Csv.decodeByName bs of
  Left err   -> Left (ParseError 0 (pack err))
  Right (_, rows) -> Right (V.toList rows)

validateBars :: [RawBar] -> Either BacktestError (V.Vector Bar)
validateBars [] = Left (ValidationError EmptyDataset)
validateBars raws = do
  checkMonotone raws
  checkDuplicates raws
  bars <- traverse checkSingleBar (zip [1..] raws)
  Right (V.fromList bars)

-- | Convenience: parseCSV then validateBars.
ingestCSV :: BL.ByteString -> Either BacktestError (V.Vector Bar)
ingestCSV bs = parseCSV bs >>= validateBars

-- ── Validation helpers (fail-fast) ──────────────────────────────

checkSingleBar :: (Int, RawBar) -> Either BacktestError Bar
checkSingleBar (row, rb) = do
  checkHighLow row rb
  checkNonNegPrices row rb
  checkNonNegVolume row rb
  Right Bar
    { barTimestamp = rbTimestamp rb
    , barOpen      = rbOpen rb
    , barHigh      = rbHigh rb
    , barLow       = rbLow rb
    , barClose     = rbClose rb
    , barVolume    = rbVolume rb
    }

checkHighLow :: Int -> RawBar -> Either BacktestError ()
checkHighLow row rb
  | rbHigh rb >= rbLow rb = Right ()
  | otherwise             = Left (ValidationError (HighBelowLow row))

checkNonNegPrices :: Int -> RawBar -> Either BacktestError ()
checkNonNegPrices row rb
  | rbOpen rb  >= 0
  , rbHigh rb  >= 0
  , rbLow rb   >= 0
  , rbClose rb >= 0 = Right ()
  | otherwise       = Left (ValidationError (NegativePrice row))

checkNonNegVolume :: Int -> RawBar -> Either BacktestError ()
checkNonNegVolume row rb
  | rbVolume rb >= 0 = Right ()
  | otherwise        = Left (ValidationError (NegativeVolume row))

checkMonotone :: [RawBar] -> Either BacktestError ()
checkMonotone = go 2
  where
    go _ []  = Right ()
    go _ [_] = Right ()
    go n (a:b:rest)
      | rbTimestamp a > rbTimestamp b =
          Left (ValidationError (NonMonotoneTimestamp n (rbTimestamp a) (rbTimestamp b)))
      | otherwise = go (n+1) (b:rest)

checkDuplicates :: [RawBar] -> Either BacktestError ()
checkDuplicates = go 2
  where
    go _ []  = Right ()
    go _ [_] = Right ()
    go n (a:b:rest)
      | rbTimestamp a == rbTimestamp b =
          Left (ValidationError (DuplicateTimestamp n (rbTimestamp b)))
      | otherwise = go (n+1) (b:rest)