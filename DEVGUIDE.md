# Alpha 


## Goal

A Haskell backtesting engine that ingests OHLCV CSV data, runs a pluggable strategy, simulates execution with configurable fees, and reports metrics. CLI-driven, tested with golden files and properties, deterministic output.

## Repository layout

- `app/Main.hs` — thin CLI entry (parse args, call the library).
- `src/Alpha/` — library: types, data pipeline, strategy, engine, metrics, CLI parsing.
- `test/` — `hspec-discover` entrypoint, `TestHelpers`, `Alpha/*Spec.hs`, `fixtures/`, `golden/`.
- `data/` — user data (not committed; see `.gitignore`).
- `output/` — runtime outputs (not committed).

## Module map

| Module | Role |
|--------|------|
| `Alpha.Types` | Domain types only; no business logic. |
| `Alpha.Data` | CSV parsing and validation (two-phase pipeline). |
| `Alpha.Strategy` | `Strategy` class, `resolveAction`, SMA crossover. |
| `Alpha.Strategy.Helpers` | Incremental sums and bounded buffer for moving averages. |
| `Alpha.Engine` | `runBacktest`, `EngineState`, `BacktestResult`. |
| `Alpha.Metrics` | Six metric functions and `MetricsResult`. |
| `Alpha.CLI` | `optparse-applicative` flags and wiring. |

## Implementation phases

Work proceeds in phases (0–8): project setup, types, data ingestion, strategy, engine, metrics, CLI and golden tests, edge cases and hardening, then README/CHANGELOG/release. Dependency additions (e.g. `cassava`, `aeson`, `optparse-applicative`) are introduced in the phase that needs them.

