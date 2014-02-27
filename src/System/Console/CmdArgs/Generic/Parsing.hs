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

data KwargParser a

data KwargFormat a = Mandatory String | Optional String | Flag String deriving Show

kwargFormatConv x = case x of
  Mandatory str -> Mandatory str
  Optional str -> Optional str
  Flag str -> Flag str

isRequired f =
  case f of
    Mandatory _ -> True
    _ -> False

getName a = case a of
  Mandatory s -> s
  Optional s -> s
  Flag s -> s

class KwargsRead a where

  readKwarg :: String -> Either String a
  default readKwarg :: Read a => String -> Either String a
  readKwarg x = Right $ read x

  kwargFormat :: KwargFormat a
  default kwargFormat :: KwargFormat a
  kwargFormat = Mandatory ""
  
instance KwargsRead Int 
instance KwargsRead String

instance KwargsRead x => KwargsRead (Maybe x) where

  readKwarg str =
    case str of
      "" -> Right Nothing
      _ -> readKwarg str >>= Right . Just
  kwargFormat = Optional ""

data OptionalFlag
data RequiredFlag

class GKwargsParser a where
  
  readVal :: [String] -> Either String a
  format :: [KwargFormat a]

data ConstrFlag
data StartFlag

class GKwargsDataParser a flag where

  readData :: a -> flag -> [String] -> Either String a
  dataFormat :: a -> flag -> [[KwargFormat a]]

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
      addNames s x = case x of
        Mandatory _ -> Mandatory s
        Optional _ -> Optional s
        Flag _ -> Flag s
      constr = undefined :: M1 S c f p 

instance (GKwargsDataParser (f p) ConstrFlag) => (GKwargsDataParser (M1 i c f p)) StartFlag where

  readData _ _ xs = readData undefined (undefined :: ConstrFlag) xs >>= Right . M1
  dataFormat _ _ = map (map kwargFormatConv)
                 (dataFormat undefined (undefined :: ConstrFlag)  :: [[KwargFormat (f p)]])

instance GKwargsParser (f p) => (GKwargsDataParser (M1 i c f p)) ConstrFlag where

  readData _ _ xs = readVal xs >>= Right . M1
  dataFormat _ _ = [map kwargFormatConv (format :: [KwargFormat (f p)])]
  
kwargParser :: forall a. Generic a => KwargParser a
kwargParser = undefined
