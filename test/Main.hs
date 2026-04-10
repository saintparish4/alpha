module Main (main) where

import Test.Hspec

import qualified Alpha.CLISpec as CLISpec
import qualified Alpha.DataSpec as DataSpec
import qualified Alpha.EdgeCasesSpec as EdgeCasesSpec
import qualified Alpha.EngineSpec as EngineSpec
import qualified Alpha.MetricsSpec as MetricsSpec
import qualified Alpha.StrategySpec as StrategySpec
import qualified Alpha.TypesSpec as TypesSpec

main :: IO ()
main = hspec $ do
  TypesSpec.spec
  DataSpec.spec
  StrategySpec.spec
  EngineSpec.spec
  MetricsSpec.spec
  CLISpec.spec
  EdgeCasesSpec.spec
