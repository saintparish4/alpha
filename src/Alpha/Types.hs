module Alpha.Types
  ( -- * Market Data
    RawBar (..),
    Bar (..),
    mkBar,

    -- * Signals / Actions
    Signal (..),
    PositionState (..),
    Action (..),

    -- * Positions / Trades
    Position (..),
    ClosedTrade (..),

    -- * Fee Model
    FeeModel (..),
    applyFee,

    -- * Errors
    BacktestError (..),
    ValidationFailure (..),

    -- * Results
    BacktestResult (..),
    MetricsResult (..),
  )
where

import Control.Exception (IOException)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)

-- ── Core Market Data ────────────────────────────────────────────

data RawBar = RawBar
  { rbTimestamp :: UTCTime,
    rbOpen :: Scientific,
    rbHigh :: Scientific,
    rbLow :: Scientific,
    rbClose :: Scientific,
    rbVolume :: Scientific
  }
  deriving (Show, Eq)

data Bar = Bar
  { barTimestamp :: UTCTime,
    barOpen :: Scientific,
    barHigh :: Scientific,
    barLow :: Scientific,
    barClose :: Scientific,
    barVolume :: Scientific
  }
  deriving (Show, Eq)

-- | Smart constructor: rejects bars where high < low.
mkBar ::
  UTCTime ->
  Scientific ->
  Scientific ->
  Scientific ->
  Scientific ->
  Scientific ->
  Maybe Bar
mkBar ts o h l c v
  | h >= l = Just (Bar ts o h l c v)
  | otherwise = Nothing

-- ── Signals and Actions ─────────────────────────────────────────

data Signal = Buy | Sell | Hold deriving (Show, Eq, Enum, Bounded)

data PositionState = Flat | Long deriving (Show, Eq, Enum, Bounded)

data Action = Enter | Exit | DoNothing deriving (Show, Eq)

-- ── Positions and Trades ────────────────────────────────────────

data Position = Position
  { posEntryTime :: UTCTime,
    posEntryPrice :: Scientific,
    posShares :: Scientific
  }
  deriving (Show, Eq)

data ClosedTrade = ClosedTrade
  { ctEntryTime :: UTCTime,
    ctEntryPrice :: Scientific,
    ctExitTime :: UTCTime,
    ctExitPrice :: Scientific,
    ctShares :: Scientific,
    ctEntryFee :: Scientific,
    ctExitFee :: Scientific
  }
  deriving (Show, Eq)

-- ── Fee Model ───────────────────────────────────────────────────

data FeeModel = FlatFee Scientific | PercentFee Scientific | NoFee
  deriving (Show, Eq)

applyFee :: FeeModel -> Scientific -> Scientific
applyFee (FlatFee f) _ = f
applyFee (PercentFee r) value = r * value
applyFee NoFee _ = 0

-- ── Error Hierarchy ─────────────────────────────────────────────

data BacktestError
  = ParseError Int Text
  | ValidationError ValidationFailure
  | IOFailure IOException
  deriving (Show)

instance Eq BacktestError where
  ParseError r1 t1    == ParseError r2 t2    = r1 == r2 && t1 == t2
  ValidationError v1  == ValidationError v2  = v1 == v2
  IOFailure e1        == IOFailure e2        = show e1 == show e2
  _                   == _                   = False

data ValidationFailure
  = EmptyDataset
  | DuplicateTimestamp Int UTCTime
  | NonMonotoneTimestamp Int UTCTime UTCTime
  | HighBelowLow Int
  | NegativePrice Int
  | NegativeVolume Int
  deriving (Show, Eq)

-- ── Results ─────────────────────────────────────────────────────

data BacktestResult = BacktestResult
  { brClosedTrades :: [ClosedTrade],
    brOpenPosition :: Maybe Position,
    brEquityCurve :: Vector Scientific,
    brBarCount :: Int
  }
  deriving (Show)

data MetricsResult = MetricsResult
  { mrTotalReturn :: Scientific,
    mrAnnualizedReturn :: Scientific,
    mrMaxDrawdown :: Scientific,
    mrWinRate :: Scientific,
    mrProfitFactor :: Scientific,
    mrSharpe :: Maybe Scientific
  }
  deriving (Show)
