# Revision history for alpha

## 0.1.0.0 -- 2026-04-19

Initial release.

* OHLCV CSV ingest with two-phase parse + validation pipeline (`Alpha.Data`).
* Pluggable strategies via the `Strategy` typeclass; ships with an SMA crossover (`Alpha.Strategy`).
* Strict-fold backtest engine with single-position, all-in sizing and configurable fee model (`Alpha.Engine`).
* Six metrics: total return, annualized return, max drawdown, win rate, profit factor, Sharpe ratio (`Alpha.Metrics`).
* `optparse-applicative` CLI with JSON metrics + CSV equity-curve output (`Alpha.CLI`).
* Hspec + QuickCheck test suite, including a golden test on the full pipeline.
