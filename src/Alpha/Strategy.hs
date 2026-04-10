{-# LANGUAGE TypeFamilies #-} 

module Alpha.Strategy 
    (
        Strategy(..),
        resolveAction, 
        SmaCrossover(..)
    )
where 

import Alpha.Types 
import Alpha.Strategy.Helpers 

-- ── Strategy typeclass ──────────────────────────────────────────

class Strategy s where
  type State s
  initStrategy :: s -> State s
  onBar :: s -> State s -> Bar -> (State s, Signal)

-- ── resolveAction truth table ───────────────────────────────────

resolveAction :: Signal -> PositionState -> Action
resolveAction Buy Flat = Enter
resolveAction Buy Long = DoNothing
resolveAction Sell Flat = DoNothing
resolveAction Sell Long = Exit
resolveAction Hold _ = DoNothing

-- ── SMA crossover strategy ───────────────────────────────────────

data SmaCrossover = SmaCrossover 
    {
        smaShortPeriod :: !Int, 
        smaLongPeriod :: !Int
    }
    deriving (Show, Eq) 

data SmaState = SmaState 
    {
        ssShort :: !RunningSum,
        ssLong :: !RunningSum 
    }
    deriving (Show, Eq)  

instance Strategy SmaCrossover where 
    type State SmaCrossover = SmaState 

    initStrategy sma = 
        SmaState 
        { ssShort = emptyRunningSum (smaShortPeriod sma), 
          ssLong = emptyRunningSum (smaLongPeriod sma)
        }

    onBar _sma state bar =
        let close = barClose bar
            newShort = pushValue close (ssShort state)
            newLong = pushValue close (ssLong state)
            newState = SmaState newShort newLong
            signal
                | not (rsFull newShort) || not (rsFull newLong) = Hold
                | otherwise =
                    case (currentAverage newShort, currentAverage newLong) of
                        (Just s, Just l)
                            | s > l -> Buy
                            | s < l -> Sell
                            | otherwise -> Hold
                        _ -> Hold
        in (newState, signal)
