module MigrateLegacyDocs where

import Prelude

import Codec.JSON.DecodeError (DecodeError)
import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.JSON (decode, encode)
import Data.Either (Either(..), either)
import Data.Filterable (partitionMap)
import Data.Foldable (fold, for_)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Github (githubReadme)
import JSON as JSON
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.Glob.Basic (expandGlobsCwd)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Registry.Docgen.Codec as Codec
import Registry.Docgen.Convert (fromLegacyPackage)
import Registry.Docgen.Docs (DocPackage(..), Readme(..))
import Registry.Docgen.Legacy.Docs as Legacy
import Registry.Docgen.Legacy.JSON (decodeDocPackage)
import Registry.PackageName as PackageName
import Registry.Version as Version

cachePath :: FilePath
cachePath = "scripts/docs-cache"

outputPath :: FilePath
outputPath = "scripts/docs-output"

historyFile :: FilePath
historyFile = Path.concat [ cachePath, "history" ]

data MigrationError
  = FailedToParseJSON String
  | FailedToDecodePackage DecodeError
  | GithubError Int

printMigrationError :: MigrationError -> String
printMigrationError = case _ of
  FailedToParseJSON err -> err
  FailedToDecodePackage err -> DecodeError.print err
  GithubError status -> "Github error: " <> show status

main :: Effect Unit
main = launchAff_ do
  mbToken <- liftEffect $ Process.lookupEnv "GITHUB_TOKEN"
  globs <- liftEffect $ Array.drop 2 <$> Process.argv
  paths <- Set.filter (eq ".json" <<< Path.extname) <$> expandGlobsCwd globs
  history <- Set.fromFoldable <<< either (const []) (String.split (Pattern "\n")) <$> try (FS.readTextFile UTF8 historyFile)
  results <- for (Array.fromFoldable (Set.difference paths history)) \path -> runExceptT do
    contents <- lift $ FS.readTextFile UTF8 path
    json <- liftEither $ lmap FailedToParseJSON $ JSON.parse contents
    legacyPackage <- liftEither $ lmap FailedToDecodePackage $ decodeDocPackage json
    let (Legacy.DocPackage { github, versionTag }) = legacyPackage
    let (DocPackage package) = fromLegacyPackage legacyPackage
    readme <- ExceptT $ fetchReadme { token: fold mbToken, repo: github, ref: versionTag }
    let outputFileBase = Path.concat [ outputPath, PackageName.print package.name ]
    let outputFile = Path.concat [ outputFileBase, Version.print package.version <> ".json" ]
    lift $ FS.mkdir' outputFileBase { mode: Perms.permsAll, recursive: true }
    lift $ FS.writeTextFile UTF8 outputFile $ JSON.print $ encode Codec.docPackage $ DocPackage $ package { readme = readme }
    lift $ FS.appendTextFile UTF8 historyFile $ path <> "\n"

  let { left } = partitionMap identity results
  for_ left $ liftEffect <<< Console.error <<< printMigrationError

fetchReadme
  :: { token :: String
     , repo :: Legacy.GithubData
     , ref :: String
     }
  -> Aff (Either MigrationError (Maybe Readme))
fetchReadme { token, repo: Legacy.GithubData { repo, user }, ref } = do
  let
    cacheFile = Path.concat
      [ cachePath
      , repo <> "-" <> user <> "-" <> ref <> "-readme.json"
      ]
  try (FS.readTextFile UTF8 cacheFile) >>= case _ of
    Left _ -> do
      Console.log $ "Downloading README: " <> user <> "/" <> repo <> "#" <> ref
      { name, content, ok, status } <- githubReadme { token, user, repo, ref }
      if ok then do
        let
          readme = Readme
            { content
            , extension: case Path.extname name of
                "" -> Nothing
                other -> String.stripPrefix (Pattern ".") other
            }
        FS.mkdir' cachePath { mode: Perms.permsAll, recursive: true }
        FS.writeTextFile UTF8 cacheFile $ JSON.print $ encode Codec.readme readme
        pure $ Right $ Just readme
      else if status == 404 then do
        FS.mkdir' cachePath { mode: Perms.permsAll, recursive: true }
        FS.writeTextFile UTF8 cacheFile "{}"
        pure $ Right Nothing
      else do
        Console.error $ show status
        pure $ Left $ GithubError status
    Right prev ->
      case (lmap DecodeError.print <<< decode Codec.readme) =<< JSON.parse prev of
        Left _ -> do
          pure $ Right Nothing
        Right readme ->
          pure $ Right $ Just readme

