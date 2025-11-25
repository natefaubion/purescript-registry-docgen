module GeneratePackageDocsJSON where

import Prelude

import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (lift, runExceptT, throwError)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Codec.JSON (encode)
import Data.Either (Either(..))
import Data.Filterable (separate)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup.Foldable (intercalateMap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import JSON as JSON
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Errors (printParseError)
import PureScript.CST.Parser.Monad (PositionedError)
import PureScript.CST.Types as CST
import Registry.Docgen.Codec as Codecs
import Registry.Docgen.Convert (fromLegacyModule)
import Registry.Docgen.Docs (DocModule(..))
import Registry.Docgen.Legacy.JSON (decodeDocModule)
import Registry.Docgen.Reexports (modulesWithReexports)
import Safe.Coerce (coerce)

buildDocModules
  :: { packageDirectory :: FilePath
     , pursDocs :: Array FilePath
     , sources :: Array FilePath
     }
  -> Aff (Either (NonEmptyArray (Tuple String String)) (Array DocModule))
buildDocModules options = runExceptT do
  srcDocs <- getAllSourceDocs
  srcModules <- getAllSourceModules
  results <- liftEither
    $ lmap (map missingError)
    $ modulesWithReexports srcDocs
    $ map _.sourceModule srcModules

  let
    srcDir = Path.concat [ options.packageDirectory, "src" ]
    srcModuleNames = Set.fromFoldable $ Array.mapMaybe
      ( \{ sourceModule: CST.Module { header: CST.ModuleHeader { name: CST.Name { name } } }, sourcePath } ->
          const (coerce name) <$> String.stripPrefix (Pattern srcDir) sourcePath
      )
      srcModules

  pure $ Array.filter
    ( \(DocModule { name }) ->
        Set.member name srcModuleNames
    )
    results
  where
  missingError moduleName =
    Tuple (unwrap moduleName) $ "Module not found in sources"

  getAllSourceDocs = do
    allDocs <- lift $ flip parTraverse options.pursDocs \docsJsonPath -> do
      contents <- FS.readTextFile UTF8 docsJsonPath
      pure $ case JSON.parse contents of
        Left err ->
          Left $ Tuple docsJsonPath $ "Failed to parse JSON: " <> err
        Right json ->
          case decodeDocModule json of
            Left err ->
              Left $ Tuple docsJsonPath $ "Failed to parse legacy doc: " <> DecodeError.print err
            Right legacyModule -> do
              Right $ fromLegacyModule legacyModule
    let { left: docErrors, right: srcDocs } = separate allDocs
    case NonEmptyArray.fromArray docErrors of
      Just errs ->
        throwError errs
      Nothing ->
        pure srcDocs

  getAllSourceModules = do
    allSourceModules <- lift $ flip parTraverse options.sources \sourcePath -> do
      contents <- FS.readTextFile UTF8 sourcePath
      pure $ case parseModule contents of
        ParseSucceeded sourceModule ->
          Right { sourcePath, sourceModule }
        ParseSucceededWithErrors _ errs ->
          Left $ Tuple sourcePath $ intercalateMap "\n" printPositionedError errs
        ParseFailed err ->
          Left $ Tuple sourcePath $ printPositionedError err
    let { left: srcErrors, right: srcModules } = separate allSourceModules
    case NonEmptyArray.fromArray srcErrors of
      Just errs ->
        throwError errs
      Nothing ->
        pure srcModules

printPositionedError :: PositionedError -> String
printPositionedError { error, position: { line, column } } =
  "[" <> show line <> ":" <> show column <> "] " <> printParseError error

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect $ Array.drop 2 <$> Process.argv
  case Array.uncons args of
    Just { head: packageDirectory, tail: spagoSources } -> do
      let outputDirectory = Path.concat [ packageDirectory, "docs" ]
      sources <- expandGlobs packageDirectory spagoSources
      pursDocs <- expandGlobs packageDirectory [ Path.concat [ "output", "*", "docs.json" ] ]
      packageModules <- buildDocModules
        { packageDirectory
        , pursDocs: Set.toUnfoldable pursDocs
        , sources: Set.toUnfoldable sources
        }
      case packageModules of
        Left errs -> do
          for_ errs \(Tuple location error) -> do
            Console.error $ location <> ":"
            for_ (String.split (Pattern "\n") error) \line ->
              Console.error $ "  " <> line
          liftEffect $ Process.setExitCode 1
        Right result -> do
          FS.mkdir' outputDirectory { mode: Perms.permsAll, recursive: true }
          for_ result \doc@(DocModule { name }) -> do
            let filePath = Path.concat [ outputDirectory, unwrap name <> ".json" ]
            FS.writeTextFile UTF8 filePath $ JSON.print $ encode Codecs.docModule doc
    Nothing -> do
      liftEffect $ Process.setExitCode 1
      Console.error "Expected package directory"
