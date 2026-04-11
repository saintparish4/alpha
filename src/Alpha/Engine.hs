{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Alpha.Engine 
    (
        EngineState (..), 
        runBacktest 
    )
where 

import Alpha.Strategy 
import Alpha.Types 
import Data.Foldable (foldl') 
import Data.Scientific (Scientific) 
import qualified Data.Vector as V  

-- ── Strict accumulator ──────────────────────────────────────────

data EngineState s = EngineState 
    { 
        esStratState :: !(State s), 
        esPosition :: !(Maybe Position), 
        esTrades :: ![ClosedTrade], 
        esEquity :: ![Scientific],
        esPending :: !(Maybe (Signal, Bar)),
        esCash :: !Scientific 
    }

-- ── Public API ──────────────────────────────────────────────────

runBacktest :: 
    (Foldable f, Strategy s) => 
    FeeModel -> 
    Scientific -> 
    s -> 
    f Bar -> 
    BacktestResult  
runBacktest feeModel initialCash strat bars = 
    let initSt = 
            EngineState 
            {
                esStratState = initStrategy strat, 
                esPosition = Nothing, 
                esTrades = [], 
                esEquity = [initialCash], 
                esPending = Nothing, 
                esCash = initialCash 
            }
        final = foldl' (stepEngine feeModel strat) initSt bars 
    in BacktestResult 
        {
            brClosedTrades = reverse (esTrades final), 
            brOpenPosition = esPosition final, 
            brEquityCurve = V.fromList (reverse (esEquity final)), 
            brBarCount = length (esEquity final) - 1 
        }

-- ── Fold step ───────────────────────────────────────────────────

stepEngine ::
    (Strategy s) => 
    FeeModel -> 
    s -> 
    EngineState s -> 
    Bar -> 
    EngineState s 
stepEngine feeModel strat !es bar = 
    let -- 1. Execute any pending signal at this bar's open 
        !es1 = case esPending es of 
            Nothing -> es 
            Just (sig, _) -> 
                let posState = maybe Flat (const Long) (esPosition es) 
                in execAction feeModel (resolveAction sig posState) bar es 
        -- 2. Run strategy on current bar 
        (!newStratSt, !signal) = onBar strat (esStratState es1) bar
        -- 3. Mark-to-market equity 
        !equity = case esPosition es1 of 
            Nothing -> esCash es1 
            Just pos -> posShares pos * barClose bar 
    in EngineState 
        {
            esStratState = newStratSt, 
            esPosition = esPosition es1, 
            esTrades = esTrades es1, 
            esEquity = equity : esEquity es1, 
            esPending = Just (signal, bar), 
            esCash = esCash es1 
        }

-- ── Action execution ────────────────────────────────────────────
-- Position sizing: shares = cash / entryPrice (all-in, single position)
-- Fees recorded on ClosedTrade; settled at exit 

execAction :: FeeModel -> Action -> Bar -> EngineState s -> EngineState s 
execAction _ DoNothing _ es = es 
execAction _ Enter bar es = 
    let !entryPrice = barOpen bar 
        !shares = esCash es / entryPrice 
        !pos = 
            Position 
            { posEntryTime = barTimestamp bar, 
              posEntryPrice = entryPrice, 
              posShares = shares 
            }
    in es {esPosition = Just pos, esCash = 0} 
execAction feeModel Exit bar es =
    case esPosition es of
        Nothing -> es
        Just pos ->
            let !exitPrice = barOpen bar
                !entryFee = applyFee feeModel (posShares pos * posEntryPrice pos)
                !exitFee = applyFee feeModel (posShares pos * exitPrice)
                !proceeds = posShares pos * exitPrice
                !newCash = proceeds - entryFee - exitFee
                !trade =
                    ClosedTrade
                        { ctEntryTime = posEntryTime pos,
                          ctEntryPrice = posEntryPrice pos,
                          ctExitTime = barTimestamp bar,
                          ctExitPrice = exitPrice,
                          ctShares = posShares pos,
                          ctEntryFee = entryFee,
                          ctExitFee = exitFee
                        }
            in es
                { esPosition = Nothing,
                  esTrades = trade : esTrades es,
                  esCash = newCash
                }
