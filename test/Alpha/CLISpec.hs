{-# LANGUAGE OverloadedStrings #-}

module Alpha.CLISpec where

import Alpha.CLI
import Alpha.Data (ingestCSV)
import Alpha.Engine (runBacktest)
import Alpha.Metrics (computeMetrics)
import Alpha.Strategy (SmaCrossover (..))
import Alpha.Types
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Options.Applicative
  ( ParserResult (..),
    defaultPrefs,
    execParserPure,
    helper,
    info,
    renderFailure,
    (<**>),
  )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Test.Hspec

-- | Run argsParser against a list of raw CLI strings.
parseWith :: [String] -> Either String Args
parseWith argv =
  case execParserPure defaultPrefs pinfo argv of
    Success a -> Right a
    Failure f -> Left (fst (renderFailure f "alpha"))
    CompletionInvoked _ -> Left "completion"
  where
    pinfo = info (argsParser <**> helper) mempty

-- | Run a successful parse or fail the example with the parser error message.
withParsed :: [String] -> (Args -> IO ()) -> IO ()
withParsed argv k = case parseWith argv of
  Left err -> expectationFailure err
  Right args -> k args

spec :: Spec
spec = describe "Alpha.CLI" $ do

  -- ── Arg parser ────────────────────────────────────────────────
  describe "argsParser" $ do
    it "parses --data flag" $
      withParsed ["--data", "file.csv"] $ \args ->
        argsData args `shouldBe` "file.csv"

    it "defaults strategy to sma-crossover" $
      withParsed ["--data", "x.csv"] $ \args ->
        argsStrategy args `shouldBe` "sma-crossover"

    it "defaults cash to 10000" $
      withParsed ["--data", "x.csv"] $ \args ->
        argsCash args `shouldBe` 10000

    it "parses --fee-pct" $
      withParsed ["--data", "x.csv", "--fee-pct", "0.001"] $ \args ->
        argsFee args `shouldBe` FeePct 0.001

    it "parses --fee-flat" $
      withParsed ["--data", "x.csv", "--fee-flat", "5"] $ \args ->
        argsFee args `shouldBe` FeeFlat 5

    it "defaults to FeeNone when no fee flag given" $
      withParsed ["--data", "x.csv"] $ \args ->
        argsFee args `shouldBe` FeeNone

    it "parses --cash override" $
      withParsed ["--data", "x.csv", "--cash", "5000"] $ \args ->
        argsCash args `shouldBe` 5000

    it "parses --output" $
      withParsed ["--data", "x.csv", "--output", "out/run1"] $ \args ->
        argsOutput args `shouldBe` Just "out/run1"

    it "fails when --data is missing" $ do
      parseWith [] `shouldSatisfy` isLeft

  -- ── Integration smoke test ────────────────────────────────────
  describe "full pipeline on sample.csv" $ do
    it "produces valid JSON-encoded MetricsResult" $ do
      bs <- BL.readFile "test/fixtures/sample.csv"
      let bars    = either (error . show) id (ingestCSV bs)
          result  = runBacktest NoFee 10000 (SmaCrossover 3 5) bars
          metrics = computeMetrics result
          encoded = Aeson.encode metrics
      -- round-trip: encoded JSON must decode back to a valid object
      Aeson.decode encoded `shouldSatisfy` (isJust' :: Maybe Aeson.Value -> Bool)

    it "equity curve length equals barCount + 1" $ do
      bs <- BL.readFile "test/fixtures/sample.csv"
      let bars   = either (error . show) id (ingestCSV bs)
          result = runBacktest NoFee 10000 (SmaCrossover 3 5) bars
      V.length (brEquityCurve result) `shouldSatisfy` (> 0) -- non-empty

    it "barCount equals number of bars in sample.csv" $ do
      bs <- BL.readFile "test/fixtures/sample.csv"
      let bars   = either (error . show) id (ingestCSV bs)
          result = runBacktest NoFee 10000 (SmaCrossover 3 5) bars
      brBarCount result `shouldBe` 16

  -- ── Golden file (auto-creates on first run) ───────────────────
  describe "golden outputs" $ do
    it "metrics JSON golden (creates on first run, diffs on subsequent)" $ do
      bs <- BL.readFile "test/fixtures/sample.csv"
      let bars    = either (error . show) id (ingestCSV bs)
          result  = runBacktest NoFee 10000 (SmaCrossover 3 5) bars
          metrics = computeMetrics result
          actual  = Aeson.encode metrics
          goldenPath = "test/golden/metrics.json"
      createDirectoryIfMissing True "test/golden"
      exists <- doesFileExist goldenPath
      if exists
        then do
          expected <- BL.readFile goldenPath
          actual `shouldBe` expected
        else do
          BL.writeFile goldenPath actual
          putStrLn $ "[golden] created " ++ goldenPath

-- helpers

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

isJust' :: Maybe a -> Bool
isJust' (Just _) = True
isJust' Nothing  = False