{-# LANGUAGE TypeFamilies #-}

module Alpha.Strategy.Momentum
  ( MomentumBreakout (..)
  , MomentumState (..)
  ) where

import Alpha.Strategy (Strategy (..))
import Alpha.Types (Bar (..), Signal (..))
import Data.Scientific (Scientific)
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

-- | Donchian-style breakout: signal Buy when the current bar's close
-- exceeds the highest close of the prior @mbLookback@ bars; signal
-- Sell when it falls below the lowest close of that window. Hold
-- otherwise (and while the buffer is filling).
newtype MomentumBreakout = MomentumBreakout
  { mbLookback :: Int
  }
  deriving (Show, Eq)

-- | Bounded ring buffer of the most recent closes (excluding current).
data MomentumState = MomentumState
  { msBuffer :: !(Seq Scientific)
  , msCap    :: !Int
  }
  deriving (Show, Eq)

instance Strategy MomentumBreakout where
  type State MomentumBreakout = MomentumState

  initStrategy mb =
    MomentumState
      { msBuffer = Seq.empty
      , msCap    = mbLookback mb
      }

  onBar _ st bar =
    let close   = barClose bar
        full    = Seq.length (msBuffer st) >= msCap st
        signal
          | not full || Seq.null (msBuffer st) = Hold
          | otherwise =
              let closes = toList (msBuffer st)
                  hi     = maximum closes
                  lo     = minimum closes
              in if      close > hi then Buy
                 else if close < lo then Sell
                 else                    Hold
        pushed  = msBuffer st |> close
        newBuf
          | Seq.length pushed > msCap st =
              case Seq.viewl pushed of
                EmptyL    -> pushed
                _ :< rest -> rest
          | otherwise = pushed
        st' = st { msBuffer = newBuf }
    in (st', signal)
