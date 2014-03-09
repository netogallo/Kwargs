{-# Language ScopedTypeVariables,
             DefaultSignatures,
             TypeOperators,
             FlexibleContexts,
             FlexibleInstances,
             OverlappingInstances,
             MultiParamTypeClasses #-}
module System.Console.CmdArgs.Generic.Parsing where

import GHC.Generics
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException, try)

data KwargParser a

data KwargType = Mandatory | Optional | Flag deriving Show

data KwargFormat a = KwargFormat{

  constrName :: String,
  kwargType :: KwargType
}

kwargFormatConv x = KwargFormat{
  constrName = constrName x,
  kwargType = kwargType x
  }

isRequired f =
  case kwargType f of
    Mandatory -> True
    _ -> False

getName = constrName

class KwargsRead a where

  readKwarg :: String -> Either String a
  default readKwarg :: Read a => String -> Either String a
  readKwarg x = Right $ read x

  kwargFormat :: KwargFormat a
  default kwargFormat :: KwargFormat a
  kwargFormat = KwargFormat{
    kwargType = Mandatory,
    constrName = ""
    }
  
instance KwargsRead Int

instance KwargsRead String

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
  format =  [kwargFormatConv (kwargFormat :: KwargFormat a)]

instance (GKwargsParser (f x), Selector s, GKwargsParser (g x))
         => GKwargsParser (((M1 S s f) :*: g) x) where

  readVal (x:xs) = (:*:) <$> readVal [x] <*> readVal xs
  format = map kwargFormatConv (format :: [KwargFormat (M1 S s f x)])
           ++ map kwargFormatConv (format :: [KwargFormat (g x)])

instance (GKwargsParser (f p), Selector c) => GKwargsParser (M1 S c f p) where

  readVal xs = readVal xs >>= Right . M1

  format = map (addNames (selName constr)) (format :: [KwargFormat (f p)])

    where
      addNames s x = KwargFormat{
        constrName = s,
        kwargType = kwargType x
        }
      
      constr = undefined :: M1 S c f p 

instance (GKwargsDataParser (f p) ConstrFlag) => (GKwargsDataParser (M1 i c f p)) StartFlag where

  readData _ _ = map (\(f,conv) -> (map kwargFormatConv f, \xs -> conv xs >>= Right . M1))
                 (readData undefined (undefined :: ConstrFlag))

instance GKwargsParser (f p) => (GKwargsDataParser (M1 i c f p)) ConstrFlag where

  readData _ _ = [(map kwargFormatConv (format :: [KwargFormat (f p)]),
                      \xs -> readVal xs >>= Right . M1)]

kwargParser :: forall a. Generic a => KwargParser a
kwargParser = undefined
