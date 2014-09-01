{-# Language ScopedTypeVariables,
             DefaultSignatures,
             TypeOperators,
             FlexibleContexts,
             FlexibleInstances,
             OverlappingInstances,
             MultiParamTypeClasses,
             GADTs #-}
module System.Console.CmdArgs.Generic.Parsing where

import GHC.Generics
import Control.Applicative
import Safe (readMay)
import Data.Maybe (isJust)
import Unsafe.Coerce (unsafeCoerce)
import Data.List (elemIndex)
import Debug.Trace (trace)

data KwargParser a

-- | Categorizes the command line arguments as required,
-- optional or flag
data KwargType = Mandatory | Optional | Flag deriving Show

-- | Parameters used to determine how a type will be specified
-- in the command line
data KwargFormat a where
  KwargFormat :: {
    -- | The name of that will be used in the command line to specify
    -- a value
    constrName :: String,
    -- | Indicates how the flag will be treated
    kwargType :: KwargType
    } -> KwargFormat a
  Comp :: KwargFormat a ->  KwargFormat b -> KwargFormat (a,b)


-- | Used to transform format parameters for values into
-- equivalent parameters for the generic representation of
-- those values
kwargFormatConv :: KwargFormat a -> KwargFormat b
kwargFormatConv (Comp a b) = unsafeCoerce $ Comp (kwargFormatConv a) (kwargFormatConv b)
kwargFormatConv x@(KwargFormat _ _) = KwargFormat{
  constrName = constrName x,
  kwargType = kwargType x
  }

-- | Convert all argumetns of the formatting into optional arguments
-- this is used when grouping dependent values
mkOpt :: KwargFormat x -> KwargFormat x
mkOpt (Comp a b) = Comp (mkOpt a) (mkOpt b)
mkOpt k = k{kwargType=Optional}

isRequired f =
  case kwargType f of
    Mandatory -> True
    _ -> False

-- | Class that represents the vaules that can be
-- parsed from strings within the command line
class KwargsRead a where

  -- | Indicates how a string should be converted
  -- into a value of this type
  readKwarg :: String -> Either String a
  default readKwarg :: Read a => String -> Either String a
  readKwarg x =
    case filter isJust opts of
      Just v:_ -> Right v
      _ -> Left $ "Could not parse: " ++ x
    where
      opts = [readMay x]

  -- | Specifies the formatting options for the value
  kwargFormat :: KwargFormat a
  default kwargFormat :: KwargFormat a
  kwargFormat = KwargFormat{
    kwargType = Mandatory,
    constrName = ""
    }
  
instance KwargsRead Int

instance KwargsRead String where
  readKwarg "\"\"" = Right ""
  readKwarg ""= Left "No input."
  readKwarg x =
    case filter isJust [readMay $ "\"" ++ x ++ "\"", readMay x] of
      [] -> Left $ "Could not parse: " ++ x
      Just v:_ -> Right v

instance KwargsRead x => KwargsRead (Maybe x) where

  readKwarg str =
    case str of
      "" -> Right Nothing
      _ -> readKwarg str >>= Right . Just
  kwargFormat = KwargFormat{
    constrName = "",
    kwargType = Optional
    }

data OptionalFlag
data RequiredFlag

-- | Parsers for the internal types of a generic type
class GKwargsParser a where
  
  readVal :: [String] -> Either String a
  format :: [KwargFormat a]

-- | Value used to distinguish the primary constructor
-- of a value from the inner constructors for generics
data ConstrFlag

-- | Type used to indicate where the generic analysis of
-- a value begins
data StartFlag

-- | Type that represents a parser that parses a list of command
-- line arguments
class GKwargsDataParser a flag where

  readData :: a -> flag -> [([KwargFormat a],[String] -> Either String a)]

instance (KwargsRead a) => GKwargsParser (K1 i a m) where

  readVal (x:_) =
    case readKwarg x of
      Left e -> Left e
      Right v -> Right (K1 v)
  format =  [KwargFormat{constrName="", kwargType=Mandatory}]

instance (KwargsRead a, KwargsRead b) => GKwargsParser (K1 i (a,b) m) where
  readVal (v:w:_) = 
    case (readKwarg v, readKwarg w) of
      (Right x, Right y) -> Right $ K1 (x,y)
      (Left e1, Left e2) -> Left (e1 ++ e2)
      (Left e, _) -> Left e
      (_, Left e) -> Left e
  format = [unsafeCoerce $ Comp (kwargFormat :: KwargFormat a) (kwargFormat :: KwargFormat b)]

instance (KwargsRead a) => GKwargsParser (K1 i (Maybe a) m) where
  readVal (v:_) =
    case (readKwarg v) of
      Right (x :: a) -> Right $ K1 $ Just x
      _ -> Right $ K1 Nothing
  format = [mkOpt $ kwargFormatConv (kwargFormat :: KwargFormat a)]


-- | Instance to handle optional tuples. This means that whenever
-- one of the tuple triggers is present, the rest must be there
-- or the parsing fails. Nevertheless, if none is present, the
-- parsing succeds and the field is set to Nothing
instance (KwargsRead a, KwargsRead b) => GKwargsParser (K1 i (Maybe (a,b)) m) where
  readVal (v:w:_) =
    case (readKwarg v, readKwarg $ trace ("Opt: " ++ (show $ v:w:[])) w) of
      (Right (x :: a), Right (y :: b)) -> Right $ K1 $ Just (x,y)
      _ -> Right $ K1 Nothing
  format = [mkOpt $ unsafeCoerce $ kwargFormatConv $ Comp (kwargFormat :: KwargFormat a) (kwargFormat :: KwargFormat b)]

instance (GKwargsParser (f x), Selector s, GKwargsParser (g x))
         => GKwargsParser (((M1 S s f) :*: g) x) where

  readVal (x:xs) = (:*:) <$> readVal [x] <*> readVal xs
  format = map (kwargFormatConv) (format :: [KwargFormat (M1 S s f x)])
           ++ map (kwargFormatConv) (format :: [KwargFormat (g x)])

-- | Instance for the elements of a record type that are asociated
-- with a selector. It recursively generates a parser for the type
-- of the value for the field and indicates cmdargs to trigger the
-- parser with the name of the selector
instance (GKwargsParser (f p), Selector c) => GKwargsParser (M1 S c f p) where

  readVal xs = readVal xs >>= Right . M1

  format = concatMap (addNames (selName constr)) (format :: [KwargFormat (f p)])

    where
      -- | Function that adds the constructor name to the parsing
      -- configuration of the given type. If the name contains
      -- underscores '_' it will create several triggers for
      -- that field
      addNames s x@(KwargFormat _ _) = KwargFormat{
        constrName = s,
        kwargType = kwargType x
        } : []
      addNames s (Comp a b) =
        case elemIndex '_' s >>= Just . flip splitAt s of
          Nothing ->
            unsafeCoerce $ Comp (unsafeCoerce addNames s a) (unsafeCoerce addNames s b)
          Just (s1,s2) ->
            (unsafeCoerce addNames (trace (show (s1,tail s2)) s1) a)
            ++ (unsafeCoerce addNames (tail s2) b)
      
      constr = undefined :: M1 S c f p 

instance (GKwargsDataParser (f p) ConstrFlag) => (GKwargsDataParser (M1 i c f p)) StartFlag where

  readData _ _ = map (\(f,conv) -> (map kwargFormatConv f, \xs -> conv xs >>= Right . M1))
                 (readData undefined (undefined :: ConstrFlag))

instance GKwargsParser (f p) => (GKwargsDataParser (M1 i c f p)) ConstrFlag where

  readData _ _ = [(map kwargFormatConv (format :: [KwargFormat (f p)]),
                      \xs -> readVal xs >>= Right . M1)]
