module Dictionary
( readDictionary
, Dictionary(..)
, queryDict
) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Array (filter, index, mapWithIndex, partition)
import Data.Either (Either(..))
import Data.Foreign
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), charAt, contains, indexOf, joinWith, split, toLower)
import Prelude (class Show, Unit, bind, pure, show, ($), (<>), (=<<), (==), (>>=), (<$>))

import Data.Argonaut
import Control.Monad.Except


foreign import readDictionaryImpl
  :: forall eff
  .  Fn2 String
         (Json -> Eff eff Unit)
         (Eff eff Unit)

readDictionary
  :: forall eff
  .  String
  -> Aff eff (Array Dictionary)
readDictionary filename = do
  raw <- makeAff (\err succ -> runFn2 readDictionaryImpl filename succ)
  case decodeJson raw of
    Left e -> throwError $ error $ show e
    Right result -> pure result

data Dictionary = Dictionary
  { spell :: String
  , root :: Maybe String
  , meanings :: Array {meaning :: String, example :: Maybe String}
  , level :: Maybe String
  , chara :: String
  }

--derive instance genericDictionary :: Generic Dictionary

instance dictionaryShow :: Show Dictionary where
  show (Dictionary d) = d.spell

instance dictionaryDecode :: DecodeJson Dictionary where
  decodeJson raw' = do
    raw <- decodeJson raw'
    s <- raw .? "Simplingua"
    m <- raw .? "词义"
    c <- raw .? "类型"
    let
      mes = mapWithIndex (\i m1 -> {meaning: m1, example: es >>= (\x -> index x i)}) ms
      ms = split (Pattern "\n") m
      es = split (Pattern "\n") <$> e
      e = case raw .? "例句" of
        Left _ -> Nothing
        Right ee -> ee
    pure $ Dictionary
      { spell: s
      , root:
          case raw .? "词根" of
            Left _ -> Nothing
            Right "" -> Nothing
            Right rr -> Just rr
      , meanings: mes
      , level:
          case raw .? "等级" of
            Left _ -> Nothing
            Right ll -> ll
      , chara: c
      }



{-
instance dictionaryForeign :: IsForeign Dictionary where
  read raw = do
    s <- readProp "Simplingo" raw
    -- r <- readUndefined (readProp "词根") raw
    r <- readNullOrUndefined read =<< readProp "词根" raw
    m <- readProp "词义" raw
    e <- readNullOrUndefined read =<< readProp "例句" raw
    l <- readNullOrUndefined read =<< readProp "等级" raw
    c' <- readNullOrUndefined read =<< readProp "词性" raw
    let
      ms = split (Pattern "\n") m
      es = split (Pattern "\n") <$> unNullOrUndefined e
      mes = mapWithIndex (\i m1 -> {meaning: m1, example: es >>= (\x -> index x i)}) ms
      c = case unNullOrUndefined c' of
        Nothing -> "词缀"
        Just cc -> cc
    pure $ Dictionary
      { spell: s
      , root:  unNullOrUndefined r >>= (\x-> if x == "" then Nothing else pure x)
      , meanings: mes
      , level: unNullOrUndefined l
      , chara: c
      }
-}

queryDict :: String -> Array Dictionary -> Array Dictionary
queryDict q' dict = p1.yes <> p2.yes <> p3.yes <> p4 where
  q = toLower q'
  inMeanning (Dictionary d) = contains (Pattern q) $ joinWith "\n" $ (\x -> x.meaning) <$> d.meanings
  inSpell (Dictionary d) = contains (Pattern q) d.spell
  inFirstSpell (Dictionary d) = case indexOf (Pattern q) d.spell of
    Nothing -> false
    Just 0 -> true
    _ -> case charAt 0 d.spell of
      Just '/' -> true
      _ -> false
  inRoot (Dictionary d)  = case d.root of
    Nothing -> false
    Just dd -> contains (Pattern q) dd
  p1 = partition inFirstSpell dict
  p2 = partition inRoot p1.no
  p3 = partition inMeanning p2.no
  p4 = filter inSpell p3.no
