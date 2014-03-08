{-# Language ScopedTypeVariables, DefaultSignatures, FlexibleContexts, RecordWildCards #-}
module System.Console.CmdArgs.Generic.Builder where

import System.Console.CmdArgs.Generic.Parsing
import GHC.Generics
import qualified Data.Map as M
import Control.Monad
import qualified System.Console.CmdArgs.Explicit as C
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getProgName)
import Data.Either (rights, lefts)
import Data.Maybe (fromMaybe)

data BaseBuilder a = BaseBuilder{
  assignments :: M.Map String String,
  builderFormat :: [KwargFormat a],
  baseBuilder :: [String] -> Either String a
  }
  | Alt (BaseBuilder a) (BaseBuilder a)

data Kwarg a = Kwarg{

  builder :: BaseBuilder a,
  flags :: [C.Flag (M.Map String String)]
  }

data KwargsConfig = KwargsConfig {

  programName :: String,
  helpText :: Maybe C.Help,
  helpTriggers :: [C.Name]
  
  }

data Kwargs a = Kwargs{

  kwargs :: [Kwarg a]
  
  }

defaultConfig = KwargsConfig{
  programName = unsafePerformIO getProgName,
  helpText = Nothing,
  helpTriggers = ["h", "help"]
  }
                
-- |
getBuilders :: forall a x . (Generic a, GKwargsDataParser (Rep a x) StartFlag) => BaseBuilder a
getBuilders = foldl (\s f -> Alt s (build f)) (build reader) readers

    where
      rep :: Rep a x
      rep = from (undefined :: a)
      reader:readers = readData rep (undefined :: StartFlag)
      dataBuilder strs = liftM to $  strs
      build (f,r) = BaseBuilder{
        assignments = M.empty,
        builderFormat = map kwargFormatConv f,
        baseBuilder = \strs -> liftM to $ r strs 
        }

createFlag f = case kwargType f of
  Mandatory -> argArg name
  Optional  -> argArg name
  Flag -> flagArg name

  where
    name = constrName f
    ins k v m = Right $ M.insert k v m
    argArg name = C.flagReq [name] (ins name) name ""
    flagArg name = C.flagOpt "True" [name] (ins name) name ""

kwargsBuilder bb = Kwarg{
  builder = bb,
  flags = flags
  }

  where
    flags = map createFlag (builderFormat bb)

defaultMode KwargsConfig{..} = C.mode "explicit" M.empty programName baseFlag
  where
    baseFlag = C.flagArg (const Right) $ fromMaybe "" helpText

formatProcess vals f = case M.lookup (getName f) vals of
  Just arg -> Right arg
  Nothing | isRequired f -> Left $ "Required argument " ++ (getName f) ++ " not found."
  Nothing -> Right ""

-- | Function that given a value builder and argumetns, attempts to build the value
kwargProcessor cfg Kwarg{..} vals = do
  cmdArgs <- C.process mode vals
  buildArgs <- mapM (formatProcess cmdArgs) $ builderFormat builder
  baseBuilder builder buildArgs

  where
    mode = defaultMode cfg flags
      
-- | Function that given a processor for argumetns will
-- and a list of arguments will atempt to build the value
-- for the given processor out of the provided arguments 
processKwargs cfg Kwargs{..} args = case rights results of
  v:_ -> return v
  _ -> Left $ errs
  where
    process kwarg = kwargProcessor cfg kwarg args
    results = map process kwargs
    errs = lefts results

