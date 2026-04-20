module Alpha.Metrics
  ( -- * Individual metric functions
    totalReturn
  , annualizedReturn
  , maxDrawdown
  , winRate
  , profitFactor
  , sharpeRatio
    -- * Bundle
  , computeMetrics
  ) where

import Alpha.Types 
import Data.Scientific (Scientific, fromFloatDigits, toRealFloat) 
import qualified Data.Vector as V 
import Data.Vector (Vector)

-- ── Total Return ────────────────────────────────────────────────

-- | (last equity - first equity) / first equity
-- Returns 0 for empty or single-element curves.
-- Division performed in Double to avoid Scientific's non-terminating loop.
totalReturn :: Vector Scientific -> Scientific 
totalReturn curve 
    | V.length curve < 2 = 0 
    | V.head curve == 0 = 0 
    | otherwise =
        let h = toRealFloat (V.head curve) :: Double
            l = toRealFloat (V.last curve) :: Double
        in fromFloatDigits ((l - h) / h)

-- ── Annualized Return ────────────────────────────────────────── 

-- | Annualized return assuming 252 bars per year 
-- Uses Double internally for exponentiation; converts back to Scientific  
annualizedReturn :: Vector Scientific -> Int -> Scientific 
annualizedReturn curve barCount 
    | barCount <= 0 = 0
    | otherwise = 
        let tr = totalReturn curve 
            base = toRealFloat (1 + tr) :: Double 
            expo = 252 / fromIntegral barCount :: Double 
        in fromFloatDigits (base ** expo -1) 

-- ── Max Drawdown ────────────────────────────────────────────────

-- | Maximum peak-to-trough decline as a fraction in [-1, 0].
-- Returns 0 for empty or single-element curves.
-- Division performed in Double to avoid Scientific's non-terminating loop.
maxDrawdown :: Vector Scientific -> Scientific
maxDrawdown curve
    | V.length curve < 2 = 0
    | otherwise =
        let h = toRealFloat (V.head curve) :: Double
            (_, minDd) =
              V.foldl' step (h, 0) (V.map (\x -> toRealFloat x :: Double) (V.tail curve))
        in fromFloatDigits minDd
    where
        step (peak, minDd) v =
            let newPeak = max peak v
                dd = if newPeak > 0 then (v - newPeak) / newPeak else 0
            in (newPeak, min minDd dd)

-- ── winRate ─────────────────────────────────────────────────────

-- | Fraction of trades with positive net profit (gross PnL minus fees).
-- Returns 0 for an empty trade list.
-- Division performed in Double to avoid Scientific's non-terminating loop
-- (e.g. 1 win / 3 trades would otherwise hang).
winRate :: [ClosedTrade] -> Scientific
winRate [] = 0
winRate trades =
  fromFloatDigits
    (fromIntegral (length (filter isWin trades)) /
     fromIntegral (length trades) :: Double)
  where
    isWin t =
      let gross = ctShares t * (ctExitPrice t - ctEntryPrice t)
      in gross - ctEntryFee t - ctExitFee t > 0

-- ── profitFactor ────────────────────────────────────────────────

-- | Gross profit divided by gross loss (both computed after fees).
-- Returns 0 for an empty trade list.
-- Returns grossProfit when there are no losing trades (no division by zero).
-- Division performed in Double to avoid Scientific's non-terminating loop.
profitFactor :: [ClosedTrade] -> Scientific
profitFactor [] = 0
profitFactor trades =
  let pnls        = map tradePnl trades
      grossProfit = sum $ filter (> 0) pnls
      grossLoss   = abs . sum $ filter (< 0) pnls
  in if grossLoss == 0
       then grossProfit
       else fromFloatDigits
              (toRealFloat grossProfit / toRealFloat grossLoss :: Double)
  where
    tradePnl t =
      ctShares t * (ctExitPrice t - ctEntryPrice t)
        - ctEntryFee t
        - ctExitFee t

-- ── sharpeRatio ─────────────────────────────────────────────────

-- | Annualized Sharpe ratio (mean / stddev × √252).
-- Returns Nothing when stddev of period returns is zero (constant curve).
-- Requires at least 2 equity points to compute returns.
sharpeRatio :: Vector Scientific -> Maybe Scientific
sharpeRatio curve
  | V.length curve < 2 = Nothing
  | otherwise =
      let dCurve = V.map (\x -> toRealFloat x :: Double) curve
          dRets  = V.zipWith periodReturn (V.init dCurve) (V.tail dCurve)
          n      = fromIntegral (V.length dRets) :: Double
          mean   = V.sum dRets / n
          var    = V.foldl' (\acc r -> acc + (r - mean) ^ (2 :: Int)) 0 dRets / n
          sd     = sqrt var
      in if sd == 0
           then Nothing
           else Just $ fromFloatDigits (mean / sd * sqrt 252)
  where
    periodReturn :: Double -> Double -> Double
    periodReturn prev cur
      | prev == 0 = 0
      | otherwise = (cur - prev) / prev

-- ── computeMetrics ──────────────────────────────────────────────

-- | Bundle all six metrics from a BacktestResult.
computeMetrics :: BacktestResult -> MetricsResult
computeMetrics br =
  MetricsResult
    { mrTotalReturn      = totalReturn curve
    , mrAnnualizedReturn = annualizedReturn curve (brBarCount br)
    , mrMaxDrawdown      = maxDrawdown curve
    , mrWinRate          = winRate (brClosedTrades br)
    , mrProfitFactor     = profitFactor (brClosedTrades br)
    , mrSharpe           = sharpeRatio curve
    }
  where
    curve = brEquityCurve br