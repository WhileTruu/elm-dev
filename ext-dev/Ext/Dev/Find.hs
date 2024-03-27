{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find
  ( definition
  , references
  , usedModules
  , encodeResult
  , PointRegion (..)
  ) 
where

import AST.Canonical (Type (..))
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Canonicalize.Environment
import qualified Canonicalize.Type
import qualified Compile
import Control.Applicative ((<|>))

import qualified Data.Set as Set
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Name (Name)
import qualified Data.Name as Name

import qualified Data.Text as T
import qualified Elm.Details
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String
import qualified Ext.CompileProxy
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import Reporting.Error.Docs (SyntaxProblem (Name))
import qualified Reporting.Exit as Exit
import qualified Reporting.Result
import StandaloneInstances
import qualified Stuff as PerUserCache
import qualified System.Directory as Dir
import qualified Text.Show.Unicode
import qualified Util
import qualified Watchtower.Editor
import qualified Ext.Dev.Find.Canonical
import qualified Ext.Dev.Project
import qualified Stuff
import qualified System.FilePath as Path
import qualified Ext.Dev.Package
import qualified Ext.Dev.Usage
import qualified Control.Monad
import qualified Ext.Dev.Help
import qualified Ext.Dev.Explain

{- Find Definition -}


data PointRegion =
  PointRegion
    { path :: FilePath
    , region :: A.Region
    }


encodeResult :: Either String PointRegion -> Json.Encode.Value
encodeResult result =
  case result of
    Left _ ->
      Json.Encode.null
    Right (PointRegion path region) ->
      Json.Encode.object
        [ ( "definition",
            Json.Encode.object
              [ ("region", Watchtower.Editor.encodeRegion region),
                ("path", Json.Encode.string (Json.String.fromChars path))
              ]
          )
        ]


definition :: FilePath -> Watchtower.Editor.PointLocation -> IO (Either String PointRegion)
definition root (Watchtower.Editor.PointLocation path point) = do
  (Ext.CompileProxy.Single source warnings interfaces canonical compiled) <- Ext.CompileProxy.loadSingle root path
  case source of
    Left err ->
       pure (Left (show err))
    
    Right srcMod -> do
      let foundType = findTypeAtPoint point srcMod

      appendFile "/tmp/lsp.log" ("point: " ++ show point ++ "\n  type: " ++ show foundType ++ "\n\n")
      found <-
        case foundType of
          FoundNothing -> do
            case canonical of
              Just canMod ->
                  pure $ findDeclAtPoint point (Can._decls canMod)
              
              Nothing ->
                  pure foundType

          existing ->
            pure existing

      case found of
        FoundNothing -> do
          pure (Left $ "Found nothing for: " ++ show (Src._name srcMod))

        FoundExpr expr patterns -> do
          case getLocatedDetails expr of
            Nothing ->
                pure (Left "Found no location details for expr.")

            Just (Local localName) ->
              findFirstPatternIntroducing localName patterns
                & fmap (\(A.At region _) -> Right (PointRegion path region))
                & Maybe.fromMaybe (Left "Not found")
                & pure

            Just (External extCanMod name) ->
              findExternalWith findFirstValueNamed name Src._values extCanMod

            Just (Ctor extCanMod name) ->
              findExternalWith findFirstCtorNamed name Src._unions extCanMod

        FoundPattern (A.At _ (Can.PCtor extCanMod _ _ ctorName _ _)) -> do
          findExternalWith findFirstCtorNamed ctorName Src._unions extCanMod

        FoundPattern pattern ->
          pure (Left ("Found pattern: " ++ show pattern))

        FoundType tipe -> do
          canonicalizationEnvResult <- Ext.CompileProxy.loadCanonicalizeEnv root path srcMod
          case canonicalizationEnvResult of
            Nothing ->
              pure (Left "did not canonicalize env")

            Just env -> do
              let (_, eitherCanType) = Reporting.Result.run $ Canonicalize.Type.canonicalize env tipe

              case eitherCanType of
                Left err ->
                  pure (Left "FoundType canonicalization error.")

                Right (Can.TType extCanMod name _) ->
                  findExternalWith findFirstTypeNamed name id extCanMod

                Right (Can.TAlias extCanMod name _ _) -> do
                  findExternalWith findFirstTypeNamed name id extCanMod

                Right (Can.TVar name) ->
                  let canMod = Canonicalize.Environment._home env in
                  findExternalWith findFirstTypeNamed name id canMod

                Right _ ->
                  pure (Left "FoundType unhandled.")
      where
        findExternalWith findFn name listAccess canMod = do
          details <- Ext.CompileProxy.loadProject root

          case Ext.Dev.Project.lookupModulePath details (ModuleName._module canMod) of
            Nothing ->
              case Ext.Dev.Project.lookupPkgName details (ModuleName._module canMod) of
                Nothing ->
                  pure (Left "Package lookup failed")

                Just pkgName -> do
                  maybeCurrentVersion <- Ext.Dev.Package.getCurrentlyUsedOrLatestVersion "." pkgName

                  case maybeCurrentVersion of
                    Nothing ->
                      pure (Left "Failed to find package version.")

                    Just version -> do
                      packageCache <- Stuff.getPackageCache
                      let home = Stuff.package packageCache pkgName version
                      let path = home Path.</> "src" Path.</> ModuleName.toFilePath (ModuleName._module canMod) Path.<.> "elm"
                      loadedFile <- Ext.CompileProxy.loadPkgFileSource pkgName home path

                      case loadedFile of
                        Right (_, sourceMod) -> do
                          listAccess sourceMod
                            & findFn name
                            & fmap (\(A.At region _) -> Right (PointRegion path region))
                            & Maybe.fromMaybe (Left "Could not find region for package.")
                            & pure

                        Left err -> do
                            pure (Left "Failed to find package version.")

            Just targetPath -> do
              loadedFile <- Ext.CompileProxy.loadFileSource root targetPath
              case loadedFile of
                Right (_, sourceMod) -> do
                  listAccess sourceMod
                    & findFn name
                    & fmap (\(A.At region _) -> Right (PointRegion targetPath region))
                    & Maybe.fromMaybe (Left $ "I was trying to find a function corresponding to \"" ++ Name.toChars name ++ "\", and failed, obviously.")
                    & pure

                Left err ->
                    pure (Left (show err))



-- Match the AST node at the specified position

data SearchResult
  = FoundNothing
  | FoundExpr Can.Expr [Can.Pattern]
  | FoundPattern Can.Pattern
  | FoundType Src.Type
  deriving (Show)

findTypeAtPoint :: A.Position -> Src.Module -> SearchResult
findTypeAtPoint point srcMod =
  findAnnotation point (Src._values srcMod)
    & orFind (findUnion point) (Src._unions srcMod)
    & orFind (findAlias point) (Src._aliases srcMod)

findDeclAtPoint :: A.Position -> Can.Decls -> SearchResult
findDeclAtPoint point decls =
  case decls of
    Can.SaveTheEnvironment ->
      FoundNothing
    Can.Declare def next ->
      case findDef point [] def of
        FoundNothing ->
          findDeclAtPoint point next
        found ->
          found
    Can.DeclareRec def defs next ->
      -- TODO: check defs too!
      case findDef point [] def of
        FoundNothing ->
          findFirstInList (findDef point []) defs
            & orFind (findDeclAtPoint point) next
        found ->
          found

findDef :: A.Position -> [Can.Pattern] -> Can.Def -> SearchResult
findDef point foundPatterns def =
  case def of
    Can.Def locatedName patterns expr ->
      findFirstInList (findPattern point) patterns
        & orFind (findExpr point (patterns ++ foundPatterns)) expr
    Can.TypedDef locatedName freeVars patternsWithTypes expr type_ ->
      findFirstInList (findPattern point) patterns
        & orFind (findExpr point (patterns ++ foundPatterns)) expr
      where
        patterns = map fst patternsWithTypes

findExpr :: A.Position -> [Can.Pattern] -> Can.Expr -> SearchResult
findExpr point foundPatterns expr@(A.At region expr_) =
  if withinRegion point region
    then case refineExprMatch point foundPatterns expr_ of
      FoundNothing ->
        FoundExpr expr foundPatterns
      refined ->
        refined
    else FoundNothing

withinRegion :: A.Position -> A.Region -> Bool
withinRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow) && (row == endRow && col <= endCol || row < endRow)

refineExprMatch :: A.Position -> [Can.Pattern] -> Can.Expr_ -> SearchResult
refineExprMatch point foundPatterns expr_ =
  case expr_ of
    Can.List exprs ->
      findFirstInList dive exprs
    Can.Negate expr ->
      dive expr
    Can.Binop name canName otherName annotation exprOne exprTwo ->
      dive exprOne
        & orFind dive exprTwo
    Can.Lambda patterns expr ->
      findFirstInList (findPattern point) patterns
        & orFind (extendAndDive patterns) expr
    Can.Call expr exprs ->
      findFirstInList dive exprs
        & orFind dive expr
    Can.If listTupleExprs expr ->
      findFirstInList
        ( \(one, two) ->
            dive one
              & orFind dive two
        )
        listTupleExprs
        & orFind dive expr
    Can.Let def expr ->
      findDef point foundPatterns def
        & orFind (extendAndDive [defNamePattern def]) expr
    Can.LetRec defs expr ->
      findFirstInList (findDef point foundPatterns) defs
        & orFind (extendAndDive (map defNamePattern defs)) expr
    Can.LetDestruct pattern one two ->
      findPattern point pattern
        & orFind dive one
        & orFind (extendAndDive [pattern]) two
    Can.Case expr branches ->
      dive expr
        & orFind
          ( findFirstInList $
              \(Can.CaseBranch pattern expr) ->
                findPattern point pattern
                  & orFind (extendAndDive [pattern]) expr
          )
          branches
    Can.Access expr locatedName ->
      dive expr
    Can.Update name expr fields ->
      fields
        & Map.toAscList
        & findFirstInList
          (\(fieldName, Can.FieldUpdate region fieldExpr) -> dive fieldExpr)
        & orFind dive expr
    Can.Record fields ->
      fields
        & Map.toAscList
        & findFirstInList
          (\(fieldName, fieldExpr) -> dive fieldExpr)
    Can.Tuple one two maybeThree ->
      dive one
        & orFind dive two
        & orFind (maybe FoundNothing dive) maybeThree
    _ -> FoundNothing
  where
    dive = findExpr point foundPatterns

    extendAndDive newPatterns = findExpr point (newPatterns ++ foundPatterns)

defNamePattern :: Can.Def -> Can.Pattern
defNamePattern def =
  case def of
    Can.Def (A.At region name) _ _ ->
      A.At region $ Can.PVar name
    Can.TypedDef (A.At region name) _ _ _ _ ->
      A.At region $ Can.PVar name

findPattern :: A.Position -> Can.Pattern -> SearchResult
findPattern point pattern@(A.At region pattern_) =
  if withinRegion point region
    then case refinePatternMatch point pattern_ of
      FoundNothing ->
        FoundPattern pattern
      refined ->
        refined
    else FoundNothing

refinePatternMatch :: A.Position -> Can.Pattern_ -> SearchResult
refinePatternMatch point pattern_ =
  case pattern_ of
    Can.PAlias subPattern _ ->
      dive subPattern
    Can.PTuple a b c ->
      diveList ([a, b] ++ Maybe.maybeToList c)
    Can.PList subPatterns ->
      diveList subPatterns
    Can.PCons a b ->
      diveList [a, b]
    Can.PCtor _ _ _ _ _ args ->
      args
        & map Can._arg
        & diveList
    _ ->
      FoundNothing
  where
    dive = findPattern point

    diveList = findFirstInList dive

orFind :: (t -> SearchResult) -> t -> SearchResult -> SearchResult
orFind toNewResult val existing =
  case existing of
    FoundNothing ->
      toNewResult val
    _ ->
      existing

findFirstInList :: (t -> SearchResult) -> [t] -> SearchResult
findFirstInList toNewResult vals =
  case vals of
    [] ->
      FoundNothing
    (top : remain) ->
      case toNewResult top of
        FoundNothing ->
          findFirstInList toNewResult remain
        searchResult ->
          searchResult

findRegion :: A.Position -> (a -> SearchResult) -> [A.Located a] -> SearchResult
findRegion point findFn =
  findFirstInList
    ( \(A.At region node) ->
        if withinRegion point region
          then findFn node
          else FoundNothing
    )

findAnnotation :: A.Position -> [A.Located Src.Value] -> SearchResult
findAnnotation point =
  findFirstInList
    ( \(A.At _ (Src.Value _ _ _ typeAnn)) ->
        maybe FoundNothing (findType point) typeAnn
    )

findAlias :: A.Position -> [A.Located Src.Alias] -> SearchResult
findAlias point = findRegion point (\(Src.Alias _ _ type_) -> findType point type_)

findUnion :: A.Position -> [A.Located Src.Union] -> SearchResult
findUnion point =
  findRegion
    point
    ( \(Src.Union _ _ ctors) ->
        findFirstInList (findFirstInList (findType point) . snd) ctors
    )

findType :: A.Position -> Src.Type -> SearchResult
findType point tipe@(A.At region type_) =
  if withinRegion point region
    then case refineTypeMatch point type_ of
      FoundNothing ->
        FoundType tipe
      refined ->
        refined
    else FoundNothing

refineTypeMatch :: A.Position -> Src.Type_ -> SearchResult
refineTypeMatch point type_ =
  case type_ of
    Src.TLambda arg ret ->
      dive arg
        & orFind dive ret
    Src.TVar _ ->
      FoundNothing
    Src.TType _ _ tvars ->
      findFirstInList dive tvars
    Src.TTypeQual _ _ _ tvars ->
      findFirstInList dive tvars
    Src.TRecord fields extRecord ->
      -- TODO: Check extRecord
      findFirstInList (dive . snd) fields
    Src.TUnit ->
      FoundNothing
    Src.TTuple a b rest ->
      findFirstInList dive (a : b : rest)
  where
    dive = findType point

-- Classify matched expression or type so we know where to search


data LocatedValue
  = Local Name
  | External ModuleName.Canonical Name
  | Ctor ModuleName.Canonical Name


getLocatedDetails :: Can.Expr -> Maybe LocatedValue
getLocatedDetails (A.At region expr) =
  case expr of
    Can.VarLocal name ->                                Just (Local name)
    Can.VarTopLevel mod name ->                         Just (External mod name)
    Can.VarKernel oneName twoName ->                    Nothing
    Can.VarForeign mod name ann ->                      Just (External mod name)
    Can.VarCtor _ mod name idx ann ->                   Just (Ctor mod name)
    Can.VarDebug mod name ann ->                        Just (External mod name)
    Can.VarOperator firstName mod name ann ->           Just (External mod name)
    Can.Chr _ ->                                        Nothing
    Can.Str _ ->                                        Nothing
    Can.Int i ->                                        Nothing
    Can.Float f ->                                      Nothing
    Can.List listExprs ->                               Nothing
    Can.Negate expr ->                                  Nothing
    Can.Binop otherName mod name ann exprOne exprTwo -> Nothing
    Can.Lambda patterns expr ->                         Nothing
    Can.Call expr exprs ->                              Nothing
    Can.If branches return ->                           Nothing
    Can.Let def expr ->                                 Nothing
    Can.LetRec defs expr ->                             Nothing
    Can.LetDestruct pattern one two ->                  Nothing
    Can.Case expr caseBranch ->                         Nothing
    Can.Accessor name ->                                Nothing
    Can.Access expr locatedName ->                      Nothing
    Can.Update name expr fieldUpdates ->                Nothing
    Can.Record fields ->                                Nothing
    Can.Unit ->                                         Nothing
    Can.Tuple one two three ->                          Nothing
    Can.Shader shader types ->                          Nothing



-- Find the definition


findFirstValueNamed :: Name.Name -> [A.Located Src.Value] -> Maybe (A.Located Src.Value)
findFirstValueNamed name list =
  case list of
    [] ->
      Nothing
    (top@(A.At _ ((Src.Value (A.At _ valName) _ _ _))) : remain) ->
      if name == valName
        then Just top
        else findFirstValueNamed name remain

findFirstCtorNamed :: Name.Name -> [A.Located Src.Union] -> Maybe (A.Located Name.Name)
findFirstCtorNamed name =
  findFirstJust findUnion
  where
    findUnion (A.At _ ((Src.Union _ _ ctors))) =
      findFirstJust findCtor ctors

    findCtor (nameAt@(A.At _ ctorName), _) =
      if ctorName == name
        then Just nameAt
        else Nothing

findFirstPatternIntroducing :: Name.Name -> [Can.Pattern] -> Maybe Can.Pattern
findFirstPatternIntroducing name =
  findFirstJust (findPatternIntroducing name)

findPatternIntroducing :: Name.Name -> Can.Pattern -> Maybe Can.Pattern
findPatternIntroducing name pattern@(A.At _ pattern_) =
  case pattern_ of
    Can.PVar pname ->
      if pname == name
        then Just pattern
        else Nothing
    Can.PRecord names ->
      if name `elem` names
        then Just pattern
        else Nothing
    Can.PAlias subPattern aliasName ->
      if aliasName == name
        then Just pattern
        else findPatternIntroducing name subPattern
    Can.PTuple a b c ->
      inList ([a, b] ++ Maybe.maybeToList c)
    Can.PList subPatterns ->
      inList subPatterns
    Can.PCons a b ->
      inList [a, b]
    Can.PCtor _ _ _ _ _ args ->
      args
        & map Can._arg
        & inList
    _ ->
      Nothing
  where
    inList =
      findFirstJust (findPatternIntroducing name)


findFirstTypeNamed :: Name.Name -> Src.Module -> Maybe (A.Located ())
findFirstTypeNamed name mod =
  findFirstJust findAlias (Src._aliases mod)
  <|> findFirstJust findUnion (Src._unions mod)

  where

    findAlias (A.At region (Src.Alias (A.At _ aliasName) vars _)) =
      if name == aliasName then
        Just (A.At region ())

      else
        List.find (\(A.At _ varName) -> name == varName) vars
        & fmap (\(A.At varRegion _)-> A.At varRegion ())


    findUnion (A.At region (Src.Union (A.At _ unionName) vars _)) =
      if name == unionName then
        Just (A.At region ())

      else
        List.find (\(A.At _ varName) -> name == varName) vars
        & fmap (\(A.At varRegion _)-> A.At varRegion ())


-- Helpers

findFirstJust :: (t -> Maybe a) -> [t] -> Maybe a
findFirstJust fn vals =
  case vals of
    [] ->
      Nothing
    top : remain ->
      case fn top of
        Nothing ->
          findFirstJust fn remain
        otherwise ->
          otherwise



{- GATHER USED MODULES -}


{-|

  Given a root and a target file, return a set of all modules that are used.

-}
usedModules :: FilePath -> FilePath -> IO (Maybe (Set.Set ModuleName.Canonical))
usedModules root path = do
    (Ext.CompileProxy.Single source warnings interfaces canonical compiled) <- Ext.CompileProxy.loadSingle root path
    pure (fmap Ext.Dev.Find.Canonical.usedModules canonical)
    
    

{-| References -}

references :: FilePath -> Watchtower.Editor.PointLocation -> IO (Either String [PointRegion])
references root (Watchtower.Editor.PointLocation path point) = do
  (Ext.CompileProxy.Single source warnings interfaces canonical compiled) <- Ext.CompileProxy.loadSingle root path
  case source of
    Left err ->
       pure (Left (show err))

    Right srcMod -> do
      let foundType = findTypeAtPoint point srcMod

      found <-
        case foundType of
          FoundNothing ->
            case canonical of
              Just canMod -> pure $ findDeclAtPoint point (Can._decls canMod)
              Nothing -> pure foundType
          existing -> pure existing

      appendFile "/tmp/lsp.log" ("    point: " ++ show point ++ "\n    type: " ++ show found ++ "\n\n")

      case found of
        FoundNothing -> pure (Left $ "Found nothing for: " ++ show srcMod)
        FoundExpr expr patterns -> do
          case getLocatedDetails expr of
            Nothing -> pure (Left "Found no location details for expr.")
            Just (Local localName) -> pure (Left "BOOOOO.")
            Just (External extCanMod name) -> pure (Left "BOOOOO 2.")
            Just (Ctor extCanMod name) -> pure (Left "Not implemented")
        FoundPattern (A.At _ (Can.PCtor extCanMod _ _ ctorName _ _)) -> pure (Left "Not implemented")
        FoundPattern pattern -> pure (Left ("Found pattern: " ++ show pattern))

        FoundType tipe -> do
          canonicalizationEnvResult <- Ext.CompileProxy.loadCanonicalizeEnv root path srcMod
          case canonicalizationEnvResult of
            Nothing -> pure (Left "did not canonicalize env")

            Just env -> do
              let (_, eitherCanType) = Reporting.Result.run $ Canonicalize.Type.canonicalize env tipe

              case eitherCanType of
                Left err ->
                  pure (Left "FoundType canonicalization error.")

                Right (Can.TType extCanMod name _) -> do
                  details <- Ext.CompileProxy.loadProject root
                  let mod = ModuleName._module extCanMod
                  let importers = Ext.Dev.Project.importersOf details mod

                  Control.Monad.foldM
                    (lookupUsageOfType root details mod name)
                    []
                    importers
                  & fmap Right

                Right (Can.TAlias extCanMod name _ _) -> do
                  details <- Ext.CompileProxy.loadProject root

                  case Ext.Dev.Project.lookupModulePath details (ModuleName._module extCanMod) of
                    Nothing -> pure (Left "Could not find module")
                    Just path -> do
                      usages <- Control.Monad.foldM (lookupUsageOfType root details (ModuleName._module extCanMod) name) [] (Set.singleton (ModuleName._module extCanMod))

                      pure (Right usages)

                Right _ ->
                  pure (Left "FoundType unhandled.")

findAllValuesNamed :: [A.Located Src.Value] -> Name.Name -> [A.Located Src.Value] -> [A.Located Src.Value]
findAllValuesNamed found name list =
  case list of
    [] -> 
      found

    (top@(A.At _ ((Src.Value (A.At _ valName) _ _ _))) : remain) ->
      if name == valName then 
        findAllValuesNamed (top : found) name remain
        else findAllValuesNamed found name remain





data TypeUsage = 
    TypeUsage 
        { _usageName :: A.Located Name
        , _usageDef :: Can.Def
        , _usageType :: Can.Type
        , _isExposed :: Bool
        }
        deriving (Show)

-- Finds moderately well for type aliases
-- not for custom types that are not opaque wrappers for w/e reason
lookupUsageOfType :: 
    String 
    -> Elm.Details.Details 
    -> ModuleName.Raw 
    -> Name.Name
    -> [PointRegion]
    -> ModuleName.Raw 
    -> IO [PointRegion]
lookupUsageOfType root details originalModule targetTypeName foundMap importerModuleName = do
  case Ext.Dev.Project.lookupModulePath details importerModuleName of 
    Nothing -> do
      case Ext.Dev.Project.lookupPkgName details importerModuleName of
        Nothing ->
          -- This should probably be logged
          pure foundMap

        Just pkgName -> do
          maybeCurrentVersion <- Ext.Dev.Package.getCurrentlyUsedOrLatestVersion "." pkgName

          case maybeCurrentVersion of
            Nothing ->
              -- This should probably be logged
              pure foundMap

            Just version -> do
              packageCache <- Stuff.getPackageCache
              let home = Stuff.package packageCache pkgName version
              let path = home Path.</> "src" Path.</> ModuleName.toFilePath importerModuleName Path.<.> "elm"
              loadedFile <- Ext.CompileProxy.loadPkgFileSource pkgName home path

              case loadedFile of
                Right (_, sourceMod) -> do
                  let importers = Ext.Dev.Project.importersOf details importerModuleName
                  Control.Monad.foldM (lookupUsageOfType root details originalModule targetTypeName) foundMap importers

                Left err -> do
                  -- This should probably be logged
                  pure foundMap
        
    Just path -> do
      single <- Ext.CompileProxy.loadSingle root path
      let (Ext.CompileProxy.Single source warnings maybeInterfaces maybeCanonical compiled) = single
      
      case maybeCanonical of
        Nothing -> do
             -- This should probably be logged
            pure foundMap

        Just (Can.Module name exports docs decls unions aliases binops effects) -> do
            let thereIsAnExposedAlias = hasExposedAlias originalModule targetTypeName exports aliases
            foundMapWithAliases 
              <- if thereIsAnExposedAlias then do 
                   let importers = Ext.Dev.Project.importersOf details (ModuleName._module name)
                   Control.Monad.foldM (lookupUsageOfType root details originalModule targetTypeName) foundMap importers
                 else do
                   pure foundMap

            let missingTypes = Ext.Dev.Help.toMissingTypeLookup single
            let foundTypeUsages = usedValueInDecls exports missingTypes originalModule targetTypeName decls []
            case foundTypeUsages of
              [] ->
                pure foundMapWithAliases

              _ ->
                if not thereIsAnExposedAlias then do
                    -- If there was an exposed alias, we're already searching downstream modules
                    let importers = Ext.Dev.Project.importersOf details (ModuleName._module name)
                    finalFound <- Control.Monad.foldM (lookupUsageOfType root details originalModule targetTypeName) foundMapWithAliases importers
                    
                    pure (map (\a -> PointRegion path a) foundTypeUsages ++ finalFound)
                    

                else
                    pure (map (\a -> PointRegion path a) foundTypeUsages ++ foundMapWithAliases)


hasExposedAlias originalMpodule targetTypeName exports aliases =
    Map.foldrWithKey
        (\aliasName (Can.Alias names aliasType) found ->
            if found then
                found
            else 
                typeIsUsed originalMpodule targetTypeName aliasType
                    && Ext.Dev.Help.isExposed aliasName exports
        ) 
        False 
        aliases


usedValueInDecls ::
    Can.Exports 
    -> Map.Map Name.Name Can.Type
    -> ModuleName.Raw 
    -> Name.Name
    -> Can.Decls 
    -> [ A.Region ]
    -> [ A.Region ]
usedValueInDecls exports missingTypes moduleName targetTypeName decls found =
    case decls of
        Can.Declare def moarDecls ->
            (usedTypeInDef exports missingTypes moduleName targetTypeName def found)
                & usedValueInDecls exports missingTypes moduleName targetTypeName moarDecls
        
        Can.DeclareRec def defs moarDecls ->
            List.foldl (\gathered innerDef -> usedTypeInDef exports missingTypes moduleName targetTypeName innerDef gathered) 
                found (def : defs)
                & usedValueInDecls exports missingTypes moduleName targetTypeName moarDecls

        Can.SaveTheEnvironment ->
            found


usedTypeInDef ::
    Can.Exports 
    -> Map.Map Name.Name Can.Type
    -> ModuleName.Raw 
    -> Name.Name
    -> Can.Def
    -> [ A.Region ]
    -> [ A.Region ]
usedTypeInDef exports missingTypes moduleName targetTypeName def found =
    case def of
        Can.Def locatedName patterns expr ->
            case Map.lookup (A.toValue locatedName) missingTypes of
                Nothing -> findReferencesInExpression moduleName targetTypeName expr found

                Just tipe ->
                    if typeIsUsed moduleName targetTypeName tipe then
                        A.toRegion locatedName : findReferencesInExpression moduleName targetTypeName expr found

                    else
                        findReferencesInExpression  moduleName targetTypeName expr found

        Can.TypedDef locatedName freeVars patternTypes expr returnTipe ->
            let 
                tipe = Ext.Dev.Help.toFunctionType (fmap snd patternTypes) returnTipe
            in
            if typeIsUsed moduleName targetTypeName tipe then
                A.toRegion locatedName : findReferencesInExpression moduleName targetTypeName expr found

            else
                findReferencesInExpression moduleName targetTypeName expr found


typeIsUsed :: ModuleName.Raw -> Name.Name -> Can.Type -> Bool
typeIsUsed targetModuleName targetTypeName canType =
    case canType of 
        Can.TLambda one two ->
            typeIsUsed targetModuleName targetTypeName one 
                || typeIsUsed targetModuleName targetTypeName two

        Can.TVar _ ->
            False

        Can.TType moduleName name children ->
            if ModuleName._module moduleName == targetModuleName 
                && name == targetTypeName 
            then 
                True
            else 
                List.any (typeIsUsed targetModuleName targetTypeName) children

        Can.TRecord fieldMap maybeName ->
            case maybeName of
                Just name ->
                    name == targetTypeName 

                Nothing ->
                    False
                      -- Map.foldr
                      --   (\(Can.FieldType _ fieldType) isFound ->
                      --       if isFound then 
                      --           isFound 
                      --       else 
                      --           typeIsUsed targetModuleName targetTypeName fieldType
                      --   ) False fieldMap 

        Can.TUnit ->
            False

        Can.TTuple one two Nothing ->
           typeIsUsed targetModuleName targetTypeName one 
                || typeIsUsed targetModuleName targetTypeName two

        Can.TTuple one two (Just three) ->
            typeIsUsed targetModuleName targetTypeName one 
                || typeIsUsed targetModuleName targetTypeName two
                || typeIsUsed targetModuleName targetTypeName three

        Can.TAlias moduleName name vars (Can.Holey holeyType) ->
             if ModuleName._module moduleName == targetModuleName 
                && name == targetTypeName 
            then 
                True
            else 
                typeIsUsed targetModuleName targetTypeName holeyType

        Can.TAlias moduleName name vars (Can.Filled holeyType) ->
            if ModuleName._module moduleName == targetModuleName 
                && name == targetTypeName 
            then 
                True
            else 
                typeIsUsed targetModuleName targetTypeName holeyType


findReferencesInExpression :: ModuleName.Raw -> Name.Name -> Can.Expr -> [ A.Region ] -> [ A.Region ]
findReferencesInExpression targetModuleName targetTypeName (A.At region expr) found =
  case expr of
    Can.VarLocal name -> 
      if name == targetTypeName then 
        region : found
      else 
        found
    Can.VarTopLevel mod name ->
      if ModuleName._module mod == targetModuleName && name == targetTypeName then 
        region : found
      else 
        found
    Can.VarKernel oneName twoName -> found
    Can.VarForeign mod name ann -> 
      if ModuleName._module mod == targetModuleName && name == targetTypeName then 
        region : found
      else 
        found

    Can.VarCtor _ mod name idx ann ->
      if ModuleName._module mod == targetModuleName && name == targetTypeName then 
        region : found
      else 
        found
    Can.VarDebug mod name ann -> found
    Can.VarOperator firstName mod name ann -> found
    Can.Chr _ -> found
    Can.Str _ -> found
    Can.Int i -> found
    Can.Float f -> found
    Can.List listExprs -> found
    Can.Negate expr -> found
    Can.Binop otherName mod name ann exprOne exprTwo ->
      findReferencesInExpression targetModuleName targetTypeName exprOne 
        (findReferencesInExpression targetModuleName targetTypeName exprTwo found)
    Can.Lambda patterns expr -> findReferencesInExpression targetModuleName targetTypeName expr found
    Can.Call expr exprs ->
      List.foldl (\gathered innerExpr -> findReferencesInExpression targetModuleName targetTypeName innerExpr gathered) found exprs
    Can.If branches return ->
      List.foldl (\gathered (one, two) -> 
          findReferencesInExpression targetModuleName targetTypeName one 
            (findReferencesInExpression targetModuleName targetTypeName two gathered)
        ) found branches
    Can.Let def expr -> found
    Can.LetRec defs expr -> found
    Can.LetDestruct pattern one two -> found
    Can.Case expr caseBranch -> found
    Can.Accessor name -> found
    Can.Access expr locatedName -> found
    Can.Update name expr fieldUpdates -> found
    Can.Record fields -> found
    Can.Unit -> found
    Can.Tuple one two three -> found
    Can.Shader shader types -> found