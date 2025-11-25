module GenerateLegacyDocsHTML where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, for_)
import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import JSON as JSON
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process as Process
import Registry.Docgen.Convert (fromLegacyPackage)
import Registry.Docgen.Docs (DocModule(..), DocPackage(..), ModuleName(..))
import Registry.Docgen.HTML (HTML(..))
import Registry.Docgen.HTML as H
import Registry.Docgen.Legacy.JSON (decodeDocPackage)
import Registry.Docgen.Package.Render (defaultPackageLinker, renderContainer, renderDocument, renderModule, renderPackageIndex)
import Registry.PackageName as PackageName

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect $ Array.drop 2 <$> Process.argv
  for_ args \fileName -> do
    contents <- readTextFile UTF8 fileName
    case JSON.parse contents of
      Left err ->
        Console.log err
      Right json ->
        case decodeDocPackage json of
          Left err ->
            Console.logShow err
          Right legacyPackage -> do
            let package@(DocPackage { modules, name }) = fromLegacyPackage legacyPackage
            let packageTitle = PackageName.print name
            let linker = defaultPackageLinker package
            Console.log $ un HTML $ renderDocument
              { title: packageTitle
              , body: fold
                  [ renderContainer
                      { anchorId: packageTitle
                      , content: renderPackageIndex linker package
                      }
                  , H.forEach modules \mod@(DocModule { name: ModuleName name }) ->
                      renderContainer
                        { anchorId: name
                        , content: renderModule linker package mod
                        }
                  ]
              }
