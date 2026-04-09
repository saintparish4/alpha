{-# LANGUAGE OverloadedStrings #-}

module Alpha.DataSpec where

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import Data.Time (UTCTime, parseTimeOrError, defaultTimeLocale)

import Alpha.Types
import Alpha.Data

mkTime :: String -> UTCTime
mkTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

mkRaw :: String -> Double -> Double -> Double -> Double -> Double -> RawBar
mkRaw ts o h l c v = RawBar
  (mkTime ts)
  (fromRational $ toRational o)
  (fromRational $ toRational h)
  (fromRational $ toRational l)
  (fromRational $ toRational c)
  (fromRational $ toRational v)

validRow1, validRow2 :: RawBar
validRow1 = mkRaw "2024-01-02T09:30:00Z" 100 105 95 101 1500
validRow2 = mkRaw "2024-01-02T10:30:00Z" 101 106 100 103 1800

spec :: Spec
spec = describe "Alpha.Data" $ do

  -- ── parseCSV ───────────────────────────────────────────
  describe "parseCSV" $ do
    it "parses sample.csv successfully" $ do
      bs <- BL.readFile "test/fixtures/sample.csv"
      case parseCSV bs of
        Right bars -> length bars `shouldBe` 16
        Left  err  -> expectationFailure (show err)

    it "returns ParseError on malformed CSV" $ do
      let garbage = "not,a,valid\ncsv,\"unterminated"
      case parseCSV garbage of
        Left (ParseError _ _) -> pure ()
        other -> expectationFailure $
          "Expected ParseError, got: " ++ show other

  -- ── validateBars ───────────────────────────────────────
  describe "validateBars" $ do

    it "validates sample.csv bars end-to-end" $ do
      bs <- BL.readFile "test/fixtures/sample.csv"
      case ingestCSV bs of
        Right v  -> length v `shouldBe` 16
        Left err -> expectationFailure (show err)

    it "rejects empty dataset" $
      validateBars [] `shouldBe`
        Left (ValidationError EmptyDataset)

    it "rejects non-monotone timestamps" $ do
      let backward = mkRaw "2024-01-02T10:30:00Z" 100 105 95 101 1500
          earlier  = mkRaw "2024-01-02T09:30:00Z" 101 106 100 103 1800
      case validateBars [backward, earlier] of
        Left (ValidationError (NonMonotoneTimestamp _ _ _)) -> pure ()
        other -> expectationFailure $
          "Expected NonMonotoneTimestamp, got: " ++ show other

    it "rejects duplicate timestamps" $ do
      let dup = mkRaw "2024-01-02T09:30:00Z" 101 106 100 103 1800
      case validateBars [validRow1, dup] of
        Left (ValidationError (DuplicateTimestamp _ _)) -> pure ()
        other -> expectationFailure $
          "Expected DuplicateTimestamp, got: " ++ show other

    it "rejects high < low" $ do
      let bad = mkRaw "2024-01-02T09:30:00Z" 100 90 95 92 500
      case validateBars [bad] of
        Left (ValidationError (HighBelowLow _)) -> pure ()
        other -> expectationFailure $
          "Expected HighBelowLow, got: " ++ show other

    it "rejects negative prices" $ do
      let bad = mkRaw "2024-01-02T09:30:00Z" (-1) 10 5 8 500
      case validateBars [bad] of
        Left (ValidationError (NegativePrice _)) -> pure ()
        other -> expectationFailure $
          "Expected NegativePrice, got: " ++ show other

    it "rejects negative volume" $ do
      let bad = mkRaw "2024-01-02T09:30:00Z" 100 105 95 101 (-10)
      case validateBars [bad] of
        Left (ValidationError (NegativeVolume _)) -> pure ()
        other -> expectationFailure $
          "Expected NegativeVolume, got: " ++ show other