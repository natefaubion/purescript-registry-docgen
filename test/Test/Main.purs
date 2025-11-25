module Test.Main where

import Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Filterable (partitionMap)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import JSON as JSON
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Glob.Basic (expandGlobsCwd)
import Registry.Docgen.Legacy.JSON (decodeDocPackage)

main :: Effect Unit
main = launchAff_ do
  files <- expandGlobsCwd [ "pursuit-backups/*/*.json" ]
  results <- map Map.fromFoldable $ flip parTraverse (Array.fromFoldable files) \filePath -> do
    fileContents <- readTextFile UTF8 filePath
    pure $ Tuple filePath $ JSON.parse fileContents >>= decodeDocPackage >>> lmap DecodeError.print
  let { left } = partitionMap identity results
  forWithIndex_ left \filePath error -> do
    log filePath
    log error
    log ""

  log $ "Read " <> show (Map.size results) <> " files"
  log $ "  - " <> show (Map.size left) <> " errors"

-- getPackageNameVersion filePath =
--   case Array.takeEnd 2 $ String.split (Pattern "/") filePath of
--     [ package, version ]
--       | Just v <- String.stripSuffix (Pattern ".json") version ->
--           Just (Tuple package v)
--     _ ->
--       Nothing
