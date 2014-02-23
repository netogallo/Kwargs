{-# Language ScopedTypeVariables, DefaultSignatures, FlexibleContexts #-}
module System.Environment.Kwargs.Builder where

import System.Environment.Kwargs.Parsing
import GHC.Generics
import qualified Data.Map as M
import Control.Monad

data BaseBuilder a = BaseBuilder{
  assignments :: M.Map String String,
  builderFormat :: [KwargFormat a],
  baseBuilder :: [String] -> Maybe a
  } | Alt (BaseBuilder a) (BaseBuilder a)


data ArgDelimiters = ArgDelimiter{
  kwArg :: String,
  arg :: String
  } deriving Show

data ArgComposition =
  Letters [String]
  | Word {argName :: String, argVal :: String}
  deriving Show

data KwargsBuilder a = KwargsBuilder{

  builder :: BaseBuilder a,
  aliases :: M.Map String String
  }

-- |
getBuilders :: forall a x . (Generic a, GKwargsDataParser (Rep a x) StartFlag) => BaseBuilder a
getBuilders = foldl (\s f -> Alt s (build f)) (build fmt) fmts

    where
      rep :: Rep a x
      rep = from (undefined :: a)
      fmt:fmts = dataFormat rep (undefined :: StartFlag)
      dataBuilder strs = liftM to $ readData rep (undefined :: StartFlag) strs
      build f = BaseBuilder{
        assignments = M.empty,
        builderFormat = map kwargFormatConv f,
        baseBuilder = dataBuilder
        }


checkFlag :: String -> String -> Maybe String
checkFlag patt str
  | patt == prefix = suffix
  | otherwise = Nothing

  where
    pattLen = length patt
    (prefix,suffix)
      | length str > pattLen = (take pattLen str, Just $ drop pattLen str) 
      | otherwise = ("",Nothing)

readFlags :: ArgDelimiters -> String -> Maybe ArgComposition
readFlags d str = build (chk $ kwArg d) (chk $ arg d) 
  where
    build (Just kw) _ = undefined
    build _ (Just arg) = undefined
    build _ _ = Nothing
    chk p = checkFlag p str 
                                   
extractor :: ArgDelimiters -> [String] -> (([String],Maybe String),[String])
extractor _ [] = (([],Nothing),[])
