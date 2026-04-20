# Alpha

A backtesting engine for trading strategies, written in Haskell.

Feed it OHLCV CSV data, plug in a strategy, and get back a JSON report of the run plus a per-bar equity curve. Six standard metrics out of the box: total return, annualized return, max drawdown, win rate, profit factor, and Sharpe ratio.

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.6.7-purple.svg)](https://www.haskell.org/ghc/)

---

## Why

Most backtesters are Python notebooks: convenient, but easy to corrupt with hidden mutable state, lookahead bias, or off-by-one bar accounting. **Alpha** is a small, typed, tested core where:

- Strategies are **pure functions of bars** — `onBar :: state -> Bar -> (state, Signal)`. No I/O inside the simulation loop. No way to peek at the future.
- The engine is a **strict left fold** over the bar series. Backtests are deterministic and reproducible to the bit.
- CSV ingest is a **two-phase pipeline** (parse, then validate) that rejects malformed, non-monotone, duplicate, or nonsensical OHLCV rows before any strategy ever sees them.
- Metrics are computed once, at the end, from a single typed `BacktestResult`. No floating-point sneak attacks: prices stay in `Data.Scientific`; only metric ratios drop to `Double` (with documented reasons — see [Design notes](#design-notes)).
- The engine, parser, metrics, and CLI are each independently tested with hspec + QuickCheck, plus a golden test on the full CSV-to-JSON pipeline.

---

## Quickstart

Requires GHC 9.6.x and Cabal 3.10+ (recommend [GHCup](https://www.haskell.org/ghcup/)).

```bash
git clone https://github.com/saintparish4/alpha.git
cd alpha
cabal build
cabal test
cabal run alpha -- --data test/fixtures/sample.csv
```

Sample output (the bundled `sample.csv` is a 16-bar fixture):

```json
{"annualizedReturn":3.768777779771688,"maxDrawdown":0,"profitFactor":0,"sharpe":17.73393721073622,"totalReturn":0.1042654028436018,"winRate":0}
```

Followed by a `bar,equity` CSV on stdout. To write to files instead:

```bash
cabal run alpha -- --data test/fixtures/sample.csv --output out/run1
# → out/run1_metrics.json
# → out/run1_equity.csv
```

All flags:

```bash
cabal run alpha -- --help
```

| Flag | Description | Default |
|------|-------------|---------|
| `--data PATH` | OHLCV CSV file (required) | — |
| `--strategy NAME` | Strategy to run | `sma-crossover` |
| `--cash AMOUNT` | Initial cash | `10000` |
| `--fee-pct RATE` | Percentage fee per trade side (e.g. `0.001`) | none |
| `--fee-flat AMOUNT` | Flat fee per trade side | none |
| `--output PATH` | Output prefix; omit to write to stdout | stdout |

---

## Architecture

```
CSV bytes
    │
    ▼
Alpha.Data.parseCSV      ──► [RawBar]      ── decode (cassava)
    │
    ▼
Alpha.Data.validateBars  ──► [Bar]         ── monotone, non-negative, h >= l
    │
    ▼
Alpha.Engine.runBacktest ──► BacktestResult ── strict left fold over bars
    │                                          + applies fee model
    ▼                                          + tracks single position
Alpha.Metrics.computeMetrics ──► MetricsResult
    │
    ▼
Alpha.CLI.outputResults  ──► JSON (metrics) + CSV (equity curve)
```

Strategies plug in via the `Strategy` typeclass (`src/Alpha/Strategy.hs`):

```haskell
class Strategy s where
  type State s
  initStrategy :: s -> State s
  onBar        :: s -> State s -> Bar -> (State s, Signal)
```

A signal (`Buy`, `Sell`, `Hold`) is resolved against the current `PositionState` (`Flat`, `Long`) into an `Action` (`Enter`, `Exit`, `DoNothing`) by `resolveAction`. Orders execute at the **next bar's open** to avoid lookahead bias.

Module map:

| Module | Role |
|--------|------|
| `Alpha.Types` | Domain types only; no business logic |
| `Alpha.Data` | CSV parsing and OHLCV validation |
| `Alpha.Strategy` | `Strategy` class, `resolveAction`, SMA crossover impl |
| `Alpha.Strategy.Helpers` | O(1) running sum / moving average buffer |
| `Alpha.Engine` | `runBacktest`, fold step, position lifecycle |
| `Alpha.Metrics` | Six metric functions and `MetricsResult` bundle |
| `Alpha.CLI` | `optparse-applicative` flags + JSON/CSV output |

---

## What's tested

- **Property tests** (QuickCheck) for metric invariants:
  - `maxDrawdown` always lies in `[-1, 0]`
  - `winRate` always lies in `[0, 1]`
  - `sharpeRatio` returns `Nothing` on any constant equity curve, regardless of length
  - `RunningSum` matches a naive sum after N pushes
  - SMA crossover emits valid signals on random bar series
- **Unit tests** for every public function in `Types`, `Data`, `Strategy`, `Engine`, `Metrics`
- **CLI tests** for every `optparse-applicative` flag and default
- **Engine lifecycle tests**: Buy→Sell produces exactly one `ClosedTrade`; orphaned Buys leave open positions; fees are charged exactly twice per trade; consecutive Buys don't double-enter
- **Golden test**: full `sample.csv → metrics JSON` pipeline pinned to `test/golden/metrics.json`
- **CSV validation tests**: rejects empty data, non-monotone or duplicate timestamps, `high < low`, negative prices, negative volume

Run:

```bash
cabal test --test-show-details=direct
```

---

## Design notes

A few decisions worth flagging for readers:

- **`Scientific` for prices, `Double` for ratios.** Prices, fees, and equity values stay in arbitrary-precision `Data.Scientific` so CSV round-trips are exact. But `Scientific`'s `Fractional` instance loops forever on non-terminating decimals (e.g. `1/3`), so every metric that performs division (`totalReturn`, `maxDrawdown`, `winRate`, `profitFactor`, `sharpeRatio`) and the engine's share-sizing all convert to `Double` for the divide and back via `fromFloatDigits`. This was caught by the QuickCheck property on `maxDrawdown` hanging the test suite — a good advertisement for property tests.
- **Single position, all-in sizing.** The engine currently holds at most one long position at a time, sized as `cash / entryPrice`. This is intentional for v0.1 — it keeps the fold step trivially correct. Multi-asset and partial sizing are on the roadmap.
- **Orders fill at the *next* bar's open.** The strategy sees bar `t`, emits a signal, and that signal is executed at `open(t+1)`. This avoids the most common backtesting bug (lookahead via close-of-bar fills).
- **Fees are charged on both sides of a round trip.** Entry and exit fees are recorded on the `ClosedTrade` and netted out at exit, not silently absorbed into the equity curve mid-trade.

---

## Roadmap

- [ ] Walk-forward / out-of-sample analysis
- [ ] Multi-asset portfolios with rebalancing
- [ ] Bracket orders, stop-loss, take-profit
- [ ] Additional strategies (mean reversion, momentum, breakout)
- [ ] HTML report with embedded equity curve plot
- [ ] Hackage release

---

## Repository layout

```
app/        thin CLI entrypoint (calls Alpha.CLI.runCLI)
src/Alpha/  library modules
test/       hspec specs, fixtures/, golden/
data/       user data (gitignored)
output/     runtime outputs (gitignored)
DEVGUIDE.md architecture and implementation phases
```

---

## Platform note

On Windows, you may need to disable Smart App Control (or switch it to evaluation mode) for `cabal build` to succeed.

---

## License

MIT — see [LICENSE](LICENSE).
