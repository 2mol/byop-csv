{-# LANGUAGE OverloadedStrings #-}

module CsvHelper (toCsv) where

import           Data.List       (intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (maybe)
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Data.Text       (Text)
import qualified Data.Text       as T

(|>) = flip ($)

toCsv :: (Ord k1, Ord k2, Show k1, Show k2, Show a) => Map k1 (Map k2 a) -> Text
toCsv dict =
    toRows dict
        |> map (T.intercalate ",")
        |> T.unlines

toRows :: (Ord k1, Ord k2, Show k1, Show k2, Show a) => Map k1 (Map k2 a) -> [[Text]]
toRows nestedDict =
    let
        columnNames =
            map M.keysSet (M.elems nestedDict)
                |> S.unions
                |> S.toList

        rows =
            M.map (rowToCsv columnNames) nestedDict
                |> M.toList
                |> map prependKey

        titleRow = "" : map (T.pack . show) columnNames

    in
        titleRow : rows

prependKey :: (Show a) => (a, [Text]) -> [Text]
prependKey (a, as) = (T.pack $ show a) : as

rowToCsv :: (Ord k, Show v) => [k] -> Map k v -> [Text]
rowToCsv columnNames rowDict =
    map (lookupShow rowDict) columnNames

lookupShow :: (Ord k, Show v) => Map k v -> k -> Text
lookupShow dict key =
    case M.lookup key dict of
      Nothing  -> ""
      Just val -> T.pack $ show val
