{-# Language ScopedTypeVariables, DefaultSignatures, FlexibleContexts, RecordWildCards, AllowAmbiguousTypes, GADTs #-}
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
import Unsafe.Coerce (unsafeCoerce)

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


createFlag (Comp a b) = a1 ++ b1
  where
    a1 = unsafeCoerce createFlag a
    b1 = unsafeCoerce createFlag b

createFlag f@(KwargFormat _ _) =
  case kwargType f of
    Mandatory -> [argArg name]
    Optional  -> [argArg name]
    Flag -> [flagArg name]
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
    flags = concatMap createFlag (builderFormat bb)

defaultMode KwargsConfig{..} = C.mode "explicit" M.empty programName baseFlag
  where
    baseFlag = C.flagArg (const Right) $ fromMaybe "" helpText

formatProcess vals f@(KwargFormat _ _) = case M.lookup (constrName f) vals of
  Just arg -> Right arg
  Nothing | isRequired f -> Left $ "Required argument " ++ (getName f) ++ " not found."
  Nothing -> Right ""
formatProcess vals (Comp a b) =
  case rights [unsafeCoerce formatProcess vals a, unsafeCoerce formatProcess vals b] of
    [] -> Left "Todo: error message"
    v:_ -> Right v

-- | Function that given a value builder and argumetns, attempts to build the value

kwargProcessor :: KwargsConfig -> Kwarg t -> [String] -> Either String t
kwargProcessor cfg Kwarg{builder=Alt ba bb,flags=fs} vals = 
  let
    res1 = baseBuilderA vals
    res2 = baseBuilderB vals
  in case (res1, res2) of
    (Right r,_) -> Right r -- Maybe throw an error if both are right
                          -- although it will be slower
    (Left _,Right r) -> Right r
    (Left e1, Left e2) -> Left $ e1 ++ e2

  where
    baseBuilderGen builder = kwargProcessor cfg Kwarg{
      builder=builder,
      flags=fs
      }
    baseBuilderA = baseBuilderGen ba
    baseBuilderB = baseBuilderGen bb
    
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

