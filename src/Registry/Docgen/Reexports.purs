module Registry.Docgen.Reexports
  ( modulesWithReexports
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either(..))
import Data.Filterable (partitionMap)
import Data.Foldable (fold, foldMap)
import Data.List (List)
import Data.List as List
import Data.Map (Map, SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import PureScript.CST.Types as CST
import Registry.Docgen.Docs (DataConstructorName(..), DocChildDeclaration(..), DocChildDeclarationInfo(..), DocDeclaration(..), DocDeclarationInfo(..), DocModule(..), DocReexport(..), InfixAlias(..), ModuleName, OperatorName(..), Qualified(..), TypeName(..), ValueName(..), isPrim)
import Safe.Coerce (coerce)

data RefSet a = RefAll | RefSet a

instance Semigroup a => Semigroup (RefSet a) where
  append = case _, _ of
    RefSet a, RefSet b -> RefSet (a <> b)
    RefAll, _ -> RefAll
    _, RefAll -> RefAll

newtype ModuleMemberSet = ModuleMemberSet
  { operators :: Set OperatorName
  , typeClasses :: Set TypeName
  , typeOperators :: Set OperatorName
  , types :: Map TypeName (RefSet (Set DataConstructorName))
  , values :: Set ValueName
  }

instance Semigroup ModuleMemberSet where
  append (ModuleMemberSet a) (ModuleMemberSet b) = ModuleMemberSet
    { operators: a.operators <> b.operators
    , typeClasses: a.typeClasses <> b.typeClasses
    , typeOperators: a.typeOperators <> b.typeOperators
    , types: Map.unionWith (<>) a.types b.types
    , values: a.values <> b.values
    }

instance Monoid ModuleMemberSet where
  mempty = ModuleMemberSet
    { operators: mempty
    , typeClasses: mempty
    , typeOperators: mempty
    , types: Map.empty
    , values: mempty
    }

type ImportModuleSet = SemigroupMap ModuleName (RefSet ModuleMemberSet)

data ReexportResult = Pending | Missing | Blocked | Result DocModule

modulesWithReexports :: Array DocModule -> Array (CST.Module Void) -> Either (NonEmptyArray ModuleName) (Array DocModule)
modulesWithReexports allDocs allSourceModules = go Map.empty $ keysOf modules
  where
  sourceModulesByName = Map.fromFoldable $ map
    ( \(sourceModule@(CST.Module { header: CST.ModuleHeader { name: CST.Name { name } } })) ->
        Tuple (coerce name) sourceModule
    )
    allSourceModules

  modules = Map.fromFoldable $ Array.mapMaybe
    ( \doc@(DocModule { name }) ->
        if isPrim name then
          -- Prim modules have no sources, but don't reexport anything.
          pure $ Tuple name (Tuple doc Map.empty)
        else do
          sourceModule <- Map.lookup name sourceModulesByName
          pure $ Tuple name $ Tuple doc
            -- Modules can reexport themselves, but we don't care about that for
            -- the purposes of filling in docs reexports.
            $ Map.delete name
            $ reexportsOf sourceModule
    )
    allDocs

  go results stk = case stk of
    List.Nil -> do
      let
        { left: docErrors, right: docModules } = partitionMap
          ( \(Tuple moduleName result) ->
              case result of
                Missing ->
                  Left $ moduleName
                Result docModule ->
                  Right $ Just docModule
                _ ->
                  Right Nothing
          )
          (Map.toUnfoldable results)
      maybe (Right (Array.catMaybes docModules)) Left
        $ NonEmptyArray.fromArray docErrors
    List.Cons moduleName next ->
      case Map.lookup moduleName modules of
        Nothing ->
          go (Map.insert moduleName Missing results) next
        Just (Tuple (DocModule docs) imports) ->
          case Map.lookup moduleName results of
            Nothing ->
              go (Map.insert moduleName Pending results) $ keysOf imports <> stk
            Just Pending -> do
              let
                reexportedDocs = Map.mapMaybeWithKey
                  ( \depName refs ->
                      Map.lookup depName results >>= case _ of
                        Result depModule ->
                          Just (matchingExportsOf refs depModule)
                        _ ->
                          Nothing
                  )
                  imports
                reexports = map
                  ( \(Tuple depModuleName decls) ->
                      DocReexport
                        { declarations: foldMap (NonEmptyArray.toArray <<< snd) decls
                        , moduleName: depModuleName
                        }
                  )
                  (Map.toUnfoldable reexportedDocs)
              if Map.size reexportedDocs /= Map.size imports then
                go (Map.insert moduleName Blocked results) next
              else
                go (Map.insert moduleName (Result (DocModule docs { reexports = reexports })) results) next
            Just _ ->
              go results next

matchingExportsOf :: RefSet ModuleMemberSet -> DocModule -> Array (Tuple ModuleName (NonEmptyArray DocDeclaration))
matchingExportsOf refSet (DocModule { declarations, name, reexports }) =
  groupByModuleName case refSet of
    RefAll -> allDecls
    RefSet ms -> traverse (declInMemberSet ms) =<< allDecls
  where
  allDecls =
    map (Tuple name) declarations <>
      bindFlipped (\(DocReexport re) -> map (Tuple re.moduleName) re.declarations) reexports

  groupByModuleName =
    Array.groupAllBy (comparing fst) >>> map \decls -> do
      let (Tuple moduleName _) = NonEmptyArray.head decls
      Tuple moduleName (snd <$> decls)

declInMemberSet :: ModuleMemberSet -> DocDeclaration -> Array DocDeclaration
declInMemberSet (ModuleMemberSet members) decl@(DocDeclaration { children, info, comments, sourceSpan }) = case info of
  DeclValue { name }
    | Set.member (unqualify name) members.values ->
        pure decl
  DeclData { name }
    | Just ctorsRef <- Map.lookup (unqualify name) members.types ->
        pure $ DocDeclaration
          { children: case ctorsRef of
              RefAll -> children
              RefSet ctors ->
                Array.filter
                  case _ of
                    DocChildDeclaration { info: ChildDeclConstructor { name: ctorName } } ->
                      Set.member (unqualify ctorName) ctors
                    _ ->
                      true
                  children
          , info
          , comments
          , sourceSpan
          }
  DeclForeignData { name }
    | Map.member (unqualify name) members.types ->
        pure decl
  DeclType { name }
    | Map.member (unqualify name) members.types ->
        pure decl
  DeclTypeClass { name }
    | Set.member (unqualify name) members.typeClasses ->
        pure $ DocDeclaration
          { children: Array.filter
              case _ of
                DocChildDeclaration { info: ChildDeclTypeClassMember { name: childName } } ->
                  Set.member (unqualify childName) members.values
                _ ->
                  true
              children
          , info
          , comments
          , sourceSpan
          }
    | otherwise ->
        Array.mapMaybe
          case _ of
            DocChildDeclaration
              { comments: childComments
              , info: ChildDeclTypeClassMember { name: childName, signature }
              , sourceSpan: childSourceSpan
              }
              | Set.member (unqualify childName) members.values ->
                  Just $ DocDeclaration
                    { children: []
                    , info: DeclValue
                        { name: childName
                        , signature
                        }
                    , comments: childComments
                    , sourceSpan: childSourceSpan
                    }
            _ ->
              Nothing
          children
  DeclInfix { name, alias: AliasConstructor _ }
    | Set.member (unqualify name) members.operators ->
        pure decl
  DeclInfix { name, alias: AliasValue _ }
    | Set.member (unqualify name) members.operators ->
        pure decl
  DeclInfix { name, alias: AliasType _ }
    | Set.member (unqualify name) members.typeOperators ->
        pure decl
  _ ->
    []
  where
  unqualify :: forall a. Qualified a -> a
  unqualify (Qualified { name }) = name

reexportsOf :: CST.Module Void -> Map ModuleName (RefSet ModuleMemberSet)
reexportsOf sourceModule@(CST.Module { header: CST.ModuleHeader { exports } }) =
  un SemigroupMap $ fold $ Map.filterWithKey (\k _ -> Set.member k moduleKeys) importSet
  where
  SemigroupMap importSet = importSetOf sourceModule
  moduleKeys = foldMap (foldMap moduleKeyOf <<< delimitedNonEmptyToArray) exports
  moduleKeyOf = case _ of
    CST.ExportModule _ (CST.Name { name }) ->
      Set.singleton (coerce name)
    _ ->
      mempty

importSetOf :: CST.Module Void -> SemigroupMap ModuleName ImportModuleSet
importSetOf (CST.Module { header: CST.ModuleHeader { imports } }) =
  foldMap fromImportDecl imports
  where
  ModuleMemberSet initial = mempty

  fromImportDecl (CST.ImportDecl { module: CST.Name { name: importModule }, names, qualified }) =
    SemigroupMap
      $ Map.singleton qualifiedModule
      $ SemigroupMap
      $ Map.singleton (coerce importModule) memberSet
    where
    qualifiedModule = case qualified of
      Just (Tuple _ (CST.Name { name })) ->
        coerce name
      Nothing ->
        coerce importModule

    memberSet = case names of
      Just (Tuple _ importList) ->
        RefSet $ foldMap fromImport $ delimitedNonEmptyToArray importList
      Nothing ->
        RefAll

  fromImport = ModuleMemberSet <<< case _ of
    CST.ImportValue (CST.Name { name }) ->
      initial { values = Set.singleton (coerce name) }
    CST.ImportOp (CST.Name { name }) ->
      initial { operators = Set.singleton (coerce name) }
    CST.ImportType (CST.Name { name }) members ->
      initial { types = Map.singleton (coerce name) $ fromDataMembers members }
    CST.ImportTypeOp _ (CST.Name { name }) ->
      initial { typeOperators = Set.singleton (coerce name) }
    CST.ImportClass _ (CST.Name { name }) ->
      initial { typeClasses = Set.singleton (coerce name) }
    CST.ImportError _ ->
      initial

  fromDataMembers = case _ of
    Just (CST.DataAll _) ->
      RefAll
    Just (CST.DataEnumerated names) ->
      RefSet
        $ Set.fromFoldable
        $ map (\(CST.Name { name }) -> coerce name)
        $ delimitedToArray names
    Nothing ->
      RefSet mempty

delimitedNonEmptyToArray :: forall a. CST.DelimitedNonEmpty a -> Array a
delimitedNonEmptyToArray (CST.Wrapped { value }) = separatedToArray value

delimitedToArray :: forall a. CST.Delimited a -> Array a
delimitedToArray (CST.Wrapped { value }) = foldMap separatedToArray value

separatedToArray :: forall a. CST.Separated a -> Array a
separatedToArray (CST.Separated { head, tail }) = Array.cons head $ snd <$> tail

keysOf :: forall k v. Map k v -> List k
keysOf = List.fromFoldable <<< Map.keys
