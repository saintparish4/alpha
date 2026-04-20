{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Alpha.CLI
  ( Args (..)
  , FeeArgs (..)
  , argsParser
  , parseArgs
  , runCLI
  ) where

import Alpha.Data (ingestCSV)
import Alpha.Engine (runBacktest)
import Alpha.Metrics (computeMetrics)
import Alpha.Strategy (SmaCrossover (..))
import Alpha.Types
import Control.Exception (IOException, try)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- ── Orphan: ToJSON MetricsResult ────────────────────────────────

instance Aeson.ToJSON MetricsResult where
  toJSON mr =
    Aeson.object
      [ "totalReturn"      .= mrTotalReturn mr
      , "annualizedReturn" .= mrAnnualizedReturn mr
      , "maxDrawdown"      .= mrMaxDrawdown mr
      , "winRate"          .= mrWinRate mr
      , "profitFactor"     .= mrProfitFactor mr
      , "sharpe"           .= mrSharpe mr
      ]

-- ── Args types ──────────────────────────────────────────────────

data FeeArgs
  = FeePct Scientific
  | FeeFlat Scientific
  | FeeNone
  deriving (Show, Eq)

data Args = Args
  { argsData     :: FilePath
  , argsStrategy :: String
  , argsCash     :: Scientific
  , argsFee      :: FeeArgs
  , argsOutput   :: Maybe FilePath
  } deriving (Show)

-- ── optparse-applicative parser ──────────────────────────────────

argsParser :: Parser Args
argsParser =
  Args
    <$> strOption
          ( long "data"
          <> metavar "PATH"
          <> help "Path to OHLCV CSV file (required)"
          )
    <*> strOption
          ( long "strategy"
          <> metavar "NAME"
          <> value "sma-crossover"
          <> showDefault
          <> help "Strategy to run (sma-crossover)"
          )
    <*> option auto
          ( long "cash"
          <> metavar "AMOUNT"
          <> value 10000
          <> showDefault
          <> help "Initial cash amount"
          )
    <*> feeParser
    <*> optional
          ( strOption
            ( long "output"
            <> metavar "PATH"
            <> help "Output path prefix (omit to write to stdout)"
            )
          )

feeParser :: Parser FeeArgs
feeParser =
      (FeePct <$> option auto
        ( long "fee-pct"
        <> metavar "RATE"
        <> help "Percentage fee rate applied per trade side (e.g. 0.001)"
        ))
  <|> (FeeFlat <$> option auto
        ( long "fee-flat"
        <> metavar "AMOUNT"
        <> help "Flat fee per trade side"
        ))
  <|> pure FeeNone

parseArgs :: IO Args
parseArgs =
  execParser $
    info
      (argsParser <**> helper)
      ( fullDesc
      <> progDesc "Run a backtest on OHLCV CSV data"
      <> header "alpha - Haskell backtesting engine"
      )

-- ── Pipeline helpers ─────────────────────────────────────────────

resolveFee :: FeeArgs -> FeeModel
resolveFee (FeePct r)  = PercentFee r
resolveFee (FeeFlat f) = FlatFee f
resolveFee FeeNone     = NoFee

-- | Resolve strategy name to a concrete strategy.
-- Currently only sma-crossover is supported; periods 3/5 work within
-- the 16-bar sample fixture.
resolveStrategy :: String -> SmaCrossover
resolveStrategy _ = SmaCrossover { smaShortPeriod = 3, smaLongPeriod = 5 }

-- ── Output formatters ────────────────────────────────────────────

-- | Encode equity curve as "bar,equity\n" CSV rows.
encodeEquityCsv :: V.Vector Scientific -> BL.ByteString
encodeEquityCsv vec =
  BL8.pack $
    "bar,equity\n"
    ++ concatMap
         (\(i, v) -> show (i :: Int) ++ "," ++ show v ++ "\n")
         (zip [0 ..] (V.toList vec))

-- ── Output dispatch ──────────────────────────────────────────────

outputResults :: Args -> BacktestResult -> MetricsResult -> IO ()
outputResults args result metrics = do
  let metricsJson = Aeson.encode metrics
      equityCsv   = encodeEquityCsv (brEquityCurve result)
  case argsOutput args of
    Nothing -> do
      BL8.putStrLn metricsJson
      BL8.putStr equityCsv
    Just prefix -> do
      let jsonPath = prefix ++ "_metrics.json"
          csvPath  = prefix ++ "_equity.csv"
      BL.writeFile jsonPath metricsJson
      BL.writeFile csvPath equityCsv
      putStrLn $ "Wrote " ++ jsonPath
      putStrLn $ "Wrote " ++ csvPath

-- ── Error exit ───────────────────────────────────────────────────

die :: BacktestError -> IO a
die err = do
  hPutStrLn stderr $ "alpha: error: " ++ show err
  exitFailure

-- ── Entry point ──────────────────────────────────────────────────

runCLI :: IO ()
runCLI = do
  args <- parseArgs
  readResult <- try (BL.readFile (argsData args)) :: IO (Either IOException BL.ByteString)
  case readResult of
    Left ioErr -> die (IOFailure ioErr)
    Right bs ->
      case ingestCSV bs of
        Left err -> die err
        Right bars -> do
          let feeModel = resolveFee (argsFee args)
              cash     = argsCash args
              strat    = resolveStrategy (argsStrategy args)
              result   = runBacktest feeModel cash strat bars
              metrics  = computeMetrics result
          outputResults args result metrics