module Registry.Docgen.Convert
  ( fromLegacyPackage
  , fromLegacyModule
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Registry.Docgen.Docs (DataConstructorName(..), DocChildDeclaration(..), DocChildDeclarationInfo(..), DocConstraint(..), DocDeclaration(..), DocDeclarationInfo(..), DocModule(..), DocPackage(..), DocReexport(..), DocType(..), ForallBinding(..), FunDep(..), Ident(..), InfixAlias(..), IntLiteral(..), ModuleName(..), OperatorName(..), Qualified(..), RowLabel(..), RowRep, SourceSpan(..), StringLiteral, TypeName(..), TypeVar(..), ValueName(..))
import Registry.Docgen.Legacy.Docs (InPackage(..))
import Registry.Docgen.Legacy.Docs as L
import Registry.Location (Location(..))
import Safe.Coerce (coerce)

fromLegacyPackage :: L.DocPackage -> DocPackage
fromLegacyPackage (L.DocPackage pkg@{ github: L.GithubData github, packageMeta: L.DocPackageMeta meta }) =
  DocPackage
    { dependencies: meta.dependencies
    , description: meta.description
    , license: meta.license
    , location
    , locationRef: pkg.versionTag
    , name: coerce meta.name
    , modules: fromLegacyModule <$> pkg.modules
    , readme: Nothing
    , resolvedDependencies: pkg.resolvedDependencies
    , resolvedModulePackages: pkg.moduleMap
    , version: pkg.version
    }
  where
  location :: Location
  location = GitHub
    { owner: github.user
    , repo: github.repo
    , subdir: Nothing
    }

fromLegacyModule :: L.DocModule -> DocModule
fromLegacyModule (L.DocModule mod@{ name: currentModule }) =
  DocModule
    { comments: mod.comments
    , declarations: fromLegacyDeclaration <$> mod.declarations
    , name: mod.name
    , reexports: fromLegacyReExport <$> mod.reExports
    }
  where
  fromLegacyReExport :: L.ReExport -> DocReexport
  fromLegacyReExport (L.ReExport { declarations, moduleName: InPackage { item: moduleName } }) =
    DocReexport
      { declarations: fromLegacyDeclaration <$> declarations
      , moduleName
      }

  fromLegacyDeclaration :: L.Declaration -> DocDeclaration
  fromLegacyDeclaration (L.Declaration decl) =
    DocDeclaration
      { children: fromLegacyChildDeclaration <$> decl.children
      , info: fromLegacyDeclarationInfo decl.title (fromLegacyType <<< _.kind <<< unwrap <$> decl.kindInfo) decl.info
      , comments: decl.comments
      , sourceSpan: packageRelativeSourceSpan <$> decl.sourceSpan
      }

  fromLegacyDeclarationInfo :: String -> Maybe DocType -> L.DeclarationInfo -> DocDeclarationInfo
  fromLegacyDeclarationInfo title signature = case _ of
    L.ValueDeclaration ty ->
      DeclValue
        { name: Qualified { moduleName: currentModule, name: ValueName title }
        , signature: fromLegacyType ty
        }
    L.DataDeclaration declType tyVars roles ->
      DeclData
        { isNewtype: case declType of
            L.Newtype -> true
            L.Data -> false
        , name: Qualified { moduleName: currentModule, name: TypeName title }
        , roles
        , signature
        , vars: fromLegacyTypeVar <$> tyVars
        }
    L.TypeSynonymDeclaration tyVars body ->
      DeclType
        { body: fromLegacyType body
        , name: Qualified { moduleName: currentModule, name: TypeName title }
        , signature
        , vars: fromLegacyTypeVar <$> tyVars
        }
    L.TypeClassDeclaration tyVars cons funDeps ->
      DeclTypeClass
        { funDeps: fromLegacyFunDep <$> funDeps
        , name: Qualified { moduleName: currentModule, name: TypeName title }
        , signature
        , superClasses: fromLegacyConstraint <$> cons
        , vars: fromLegacyTypeVar <$> tyVars
        }
    L.AliasDeclaration (L.Fixity { associativity, precedence }) qual@(L.Qualified _ alias) ->
      DeclInfix
        { alias: case alias of
            Left ty ->
              AliasType $ fromLegacyQualified (qual $> ty)
            Right (Left ident) ->
              AliasValue $ fromLegacyQualified (qual $> coerce ident)
            Right (Right ctor) ->
              AliasConstructor $ fromLegacyQualified (qual $> ctor)
        , associativity
        , name: Qualified { moduleName: currentModule, name: unsafeOperatorNameFromTitle title }
        , precedence
        }
    L.ExternDataDeclaration ty roles ->
      DeclForeignData
        { name: Qualified { moduleName: currentModule, name: TypeName title }
        , roles
        , signature: fromLegacyType ty
        }

  fromLegacyChildDeclaration :: L.ChildDeclaration -> DocChildDeclaration
  fromLegacyChildDeclaration (L.ChildDeclaration decl) =
    DocChildDeclaration
      { comments: decl.comments
      , info: fromLegacyChildDeclarationInfo decl.title decl.info
      , sourceSpan: packageRelativeSourceSpan <$> decl.sourceSpan
      }

  fromLegacyChildDeclarationInfo :: String -> L.ChildDeclarationInfo -> DocChildDeclarationInfo
  fromLegacyChildDeclarationInfo title = case _ of
    L.ChildInstance cons ty ->
      ChildDeclInstance
        { constraints: fromLegacyConstraint <$> cons
        , head: fromLegacyType ty
        , name: Qualified { moduleName: currentModule, name: ValueName title }
        }
    L.ChildDataConstructor tys ->
      ChildDeclConstructor
        { args: fromLegacyTypePrec PrecAtom <$> tys
        , name: Qualified { moduleName: currentModule, name: DataConstructorName title }
        }
    L.ChildTypeClassMember ty ->
      ChildDeclTypeClassMember
        { signature: fromLegacyType ty
        , name: Qualified { moduleName: currentModule, name: ValueName title }
        }

  fromLegacyConstraint :: L.DocConstraint -> DocConstraint
  fromLegacyConstraint (L.DocConstraint name _ args) =
    DocConstraint
      { args: fromLegacyTypePrec PrecAtom <$> args
      , name: fromLegacyQualified name
      }

  fromLegacyQualified :: forall a. L.Qualified a -> Qualified a
  fromLegacyQualified (L.Qualified qb name) = case qb of
    L.ByModuleName moduleName ->
      Qualified { moduleName, name }
    L.BySourcePos _ ->
      Qualified { moduleName: currentModule, name }

  fromLegacyTypeVar :: L.DocTypeVar -> TypeVar
  fromLegacyTypeVar (L.DocTypeVar ident signature) =
    TypeVar
      { ident
      , signature: fromLegacyType <$> signature
      }

  fromLegacyFunDep :: L.Fundep -> FunDep
  fromLegacyFunDep (L.Fundep determiners determinees) =
    FunDep
      { determiners
      , determinees
      }

  fromLegacyTypePrec :: TypePrec -> L.DocType -> DocType
  fromLegacyTypePrec prec ty = do
    let ty' = fromLegacyType ty
    if precOfType ty' <= prec then
      ty'
    else
      TypeParens ty'

  fromLegacyType :: L.DocType -> DocType
  fromLegacyType = case _ of
    L.TypeVar ident ->
      TypeIdent $ Ident ident
    L.TypeLevelString str ->
      TypeString str
    L.TypeLevelInt int ->
      TypeInt $ IntSmall int
    L.TypeWildcard _ ->
      TypeWildcard
    L.TypeConstructor name ->
      TypeConstructor $ fromLegacyQualified name
    L.TypeOp name ->
      TypeOperator $ fromLegacyQualified name
    L.TypeApp ty1 ty2
      | Just arg <- isFunction ty1 ->
          TypeFunction
            { arg: fromLegacyTypePrec PrecApp arg
            , result: fromLegacyTypePrec PrecArrow ty2
            }
      | isRecord ty1
      , L.RCons label ty3 ty4 <- ty2 ->
          TypeRecord $ toRowRep [ toRowLabel label ty3 ] ty4
      | otherwise ->
          TypeApp
            { arg: fromLegacyTypePrec PrecApp ty2
            , function: fromLegacyTypePrec PrecApp ty1
            }
    L.KindApp _ ty2 ->
      fromLegacyType ty2
    L.ForAll vis ident sig ty ->
      TypeForall $ go (NonEmptyArray.singleton (toForallBinding vis ident sig)) ty
      where
      toForallBinding vis' ident' sig' =
        ForallBinding
          { isVisible: case vis' of
              L.TypeVarVisible -> true
              L.TypeVarInvisible -> false
          , name: Ident ident'
          , signature: fromLegacyType <$> sig'
          }
      go bindings = case _ of
        L.ForAll vis' ident' sig' ty' ->
          go (NonEmptyArray.snoc bindings (toForallBinding vis' ident' sig')) ty'
        body ->
          { bindings, body: fromLegacyType body }
    L.ConstrainedType con ty ->
      TypeConstrained
        { constraint: fromLegacyConstraint con
        , result: fromLegacyTypePrec PrecArrow ty
        }
    L.KindedType ty1 ty2 ->
      TypeKindSignature
        { signature: fromLegacyTypePrec PrecArrow ty2
        , term: fromLegacyType ty1
        }
    L.BinaryNoParensType ty1 ty2 ty3 ->
      TypeInfixApp
        { argLhs: fromLegacyTypePrec PrecApp ty2
        , argRhs: fromLegacyTypePrec PrecApp ty3
        , operator: fromLegacyType ty1
        }
    L.ParensInType ty ->
      TypeParens $ fromLegacyType ty
    L.RCons label ty1 ty2 ->
      TypeRow $ toRowRep [ toRowLabel label ty1 ] ty2
    L.REmpty ->
      TypeRow { labels: [], tail: Nothing }

  toRowLabel :: StringLiteral -> L.DocType -> RowLabel
  toRowLabel label sig =
    RowLabel
      { label
      , signature: fromLegacyType sig
      }

  toRowRep :: Array RowLabel -> L.DocType -> RowRep
  toRowRep labels = case _ of
    L.RCons label ty1 ty2 ->
      toRowRep (Array.snoc labels (toRowLabel label ty1)) ty2
    L.REmpty ->
      { labels, tail: Nothing }
    tail ->
      { labels, tail: Just $ fromLegacyType tail }

unsafeOperatorNameFromTitle :: String -> OperatorName
unsafeOperatorNameFromTitle title =
  OperatorName $ fromMaybe title $ valueOperator <|> typeOperator
  where
  valueOperator = do
    String.stripPrefix (String.Pattern "(") title
      >>= String.stripSuffix (String.Pattern ")")

  typeOperator = do
    String.stripPrefix (String.Pattern "type (") title
      >>= String.stripSuffix (String.Pattern ")")

packageRelativeSourceSpan :: SourceSpan -> SourceSpan
packageRelativeSourceSpan (SourceSpan span) =
  SourceSpan $ span { path = newPath }
  where
  newPath =
    span.path
      # Regex.split pathSeparator
      # Array.dropWhile (_ /= "src")
      # intercalate "/"

pathSeparator :: Regex
pathSeparator = unsafeRegex """\\|/""" noFlags

data TypePrec
  = PrecBottom
  | PrecAtom
  | PrecApp
  | PrecInfix
  | PrecArrow
  | PrecKinded
  | PrecTop

derive instance Eq TypePrec
derive instance Ord TypePrec

precOfType :: DocType -> TypePrec
precOfType = case _ of
  TypeApp _ -> PrecApp
  TypeInfixApp _ -> PrecInfix
  TypeForall _ -> PrecArrow
  TypeConstrained _ -> PrecArrow
  TypeFunction _ -> PrecArrow
  TypeKindSignature _ -> PrecKinded
  _ -> PrecAtom

isFunction :: L.DocType -> Maybe L.DocType
isFunction = case _ of
  L.TypeApp (L.TypeConstructor (L.Qualified (L.ByModuleName (ModuleName "Prim")) (TypeName "Function"))) arg ->
    Just arg
  _ ->
    Nothing

isRecord :: L.DocType -> Boolean
isRecord = case _ of
  L.TypeConstructor (L.Qualified (L.ByModuleName (ModuleName "Prim")) (TypeName "Record")) ->
    true
  _ ->
    false
