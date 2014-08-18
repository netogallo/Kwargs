{-# Language FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables #-}
module System.Console.CmdArgs.Generic(
  processKwargs,
  kwargs,
  getBuilders
  ) where

import System.Console.CmdArgs.Generic.Builder hiding (kwargs)
import System.Console.CmdArgs.Generic.Parsing (StartFlag, GKwargsDataParser)
import GHC.Generics

kwargs :: forall a x. (GKwargsDataParser (Rep a x) StartFlag) => BaseBuilder a -> [String] -> Either [String] a
kwargs k = processKwargs defaultConfig (Kwargs [kwargsBuilder k])
