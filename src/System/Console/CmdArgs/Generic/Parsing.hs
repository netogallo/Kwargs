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
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException, try)
import Safe (readMay)
import Data.Maybe (isJust)
import Unsafe.Coerce (unsafeCoerce)
import Data.List (elemIndex)
import Debug.Trace (trace)

data KwargParser a

data KwargType = Mandatory | Optional | Flag deriving Show

data KwargFormat a where
  KwargFormat :: {
    constrName :: String,
    kwargType :: KwargType
    } -> KwargFormat a
  Comp :: KwargFormat a ->  KwargFormat b -> KwargFormat (a,b)

kwargFormatConv (Comp a b) = unsafeCoerce $ Comp (unsafeCoerce kwargFormatConv a) (unsafeCoerce kwargFormatConv b)

kwargFormatConv x@(KwargFormat _ _) = KwargFormat{
  constrName = constrName x,
  kwargType = kwargType x
  }

mkOpt :: KwargFormat x -> KwargFormat x
mkOpt (Comp a b) = Comp (mkOpt a) (mkOpt b)
mkOpt k = k{kwargType=Optional}

isRequired f =
  case kwargType f of
    Mandatory -> True
    _ -> False

getName = constrName

class KwargsRead a where

  readKwarg :: String -> Either String a
  default readKwarg :: Read a => String -> Either String a
  readKwarg x =
    case filter isJust opts of
      Just v:_ -> Right v
      _ -> Left $ "Could not parse: " ++ x
    where
      opts = [readMay x]


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

class GKwargsParser a where
  
  readVal :: [String] -> Either String a
  format :: [KwargFormat a]

data ConstrFlag
data StartFlag

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


instance (GKwargsParser (f p), Selector c) => GKwargsParser (M1 S c f p) where

  readVal xs = readVal xs >>= Right . M1

  format = concatMap (addNames (selName constr)) (format :: [KwargFormat (f p)])

    where
      addNames s x@(KwargFormat _ _) = KwargFormat{
        constrName = s,
        kwargType = kwargType x
        } : []
      -- addNames _ _ = error "Joder, esta pizada"
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

kwargParser :: forall a. Generic a => KwargParser a
kwargParser = undefined
