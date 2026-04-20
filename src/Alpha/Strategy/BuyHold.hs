{-# LANGUAGE TypeFamilies #-}

module Alpha.Strategy.BuyHold
  ( BuyAndHold (..)
  , BuyHoldState (..)
  ) where

import Alpha.Strategy (Strategy (..))
import Alpha.Types (Signal (..))

-- | Benchmark strategy: emit a single Buy on the very first bar and
-- Hold forever after. Useful as a baseline for any other strategy.
data BuyAndHold = BuyAndHold
  deriving (Show, Eq)

data BuyHoldState
  = BuyHoldStart    -- ^ no Buy emitted yet; will emit Buy on next bar
  | BuyHoldHolding  -- ^ Buy already emitted; hold forever
  deriving (Show, Eq)

instance Strategy BuyAndHold where
  type State BuyAndHold = BuyHoldState

  initStrategy _ = BuyHoldStart

  onBar _ BuyHoldStart   _ = (BuyHoldHolding, Buy)
  onBar _ BuyHoldHolding _ = (BuyHoldHolding, Hold)
