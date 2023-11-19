{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Explain
  ( explain
  , explainAtLocation
  , encode
  )
where

{-|
Given a point location, return all the Types and values that are used 
in the definition at that point.



-}

import qualified AST.Source as Src
import qualified AST.Canonical as Can

import qualified Json.String
import qualified Elm.Package as Package
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer
import qualified Control.Monad as Monad

import qualified Reporting.Doc

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import Data.Name (Name)

import Data.Function ((&))
import qualified Json.Encode
import Json.Encode ((==>))

import qualified Ext.CompileProxy
import qualified Ext.Dev.Find.Source
import qualified Ext.Dev.Lookup
import qualified Ext.Dev.Json.Encode
import qualified Ext.Dev.Project

import qualified Elm.Details


import qualified Watchtower.Editor
import qualified Ext.Log



data Explanation =
    Explanation
        { _localizer :: Reporting.Render.Type.Localizer.Localizer
        , _modulename :: Name
        , _declaration :: DeclarationMetadata
        , _references :: [ Reference ]
        }

data DeclarationMetadata =
    DeclarationMetadata
        { _declarationName :: Name
        , _declarationType :: Maybe Can.Type
        , _declarationTypeComponents :: [ (TypeUsage, Ext.Dev.Lookup.LookupResult) ]
        , _declarationRange :: A.Region
        , _declarationRecursive :: Bool
        }


data Reference
    = ValueReference   
        { _source :: Source
        , _name :: Name
        , _type :: Can.Type
        }
    | TypeReference TypeUsage Ext.Dev.Lookup.LookupResult


data TypeUsage
    = TypeUsage ModuleName.Canonical Name [ Can.Type ]
    | AliasUsage ModuleName.Canonical Name [(Name, Can.Type)] Can.AliasType
    | Definition  ModuleName.Canonical Name Can.Type
    | UnionVariantUsage ModuleName.Canonical Name (Set.Set Name)
    deriving (Eq, Show)

data Source 
    = External ModuleName.Canonical
    | TopLevelDeclaration
    | LetValue
    deriving (Eq, Ord, Show)
  

explain :: Elm.Details.Details -> String -> ModuleName.Raw -> Name -> IO (Maybe Explanation)
explain details root modulename valueName = do
    case Ext.Dev.Project.lookupModulePath details modulename of
        Nothing ->
            pure Nothing
        
        Just modulePathName ->
            explainAtModulePath root modulePathName valueName


explainAtModulePath :: String -> FilePath -> Name -> IO (Maybe Explanation)
explainAtModulePath root modulePath valuename = do
    (Ext.CompileProxy.Single source warnings interfaces canonical compiled) <- Ext.CompileProxy.loadSingle root modulePath
    case (source, canonical) of
        (Right srcModule, Just canMod) -> do
            let localizer = Reporting.Render.Type.Localizer.fromModule srcModule
            let found = fmap (Ext.Dev.Find.Source.withCanonical canMod) (Ext.Dev.Find.Source.definitionNamed valuename srcModule)
            case found of
                Nothing ->
                    pure Nothing

                Just (Ext.Dev.Find.Source.FoundValue (Just def) (A.At pos val)) ->  do
                    let (refCollection@(ReferenceCollection refMap usedTypes refUnions usedModules)) = getFoundDefTypes def emptyRefCollection
                    
                    foundTypeRefs <- lookUpTypesByFragments root 
                                        (toLookupCollection refCollection)

                    Ext.Log.log Ext.Log.Misc (show (List.length foundTypeRefs))
                    let refs = Map.elems refMap
                    metadata <- getDeclarationMetadata root pos def
                    pure 
                        (Just 
                            (Explanation
                                localizer
                                (Src.getName srcModule)
                                metadata
                                (refs <> fmap pairToReference foundTypeRefs)
                            )
                        )
                
                Just (Ext.Dev.Find.Source.FoundUnion (Just union) (A.At pos srcUnion)) -> do
                    let moduleName = Can._name canMod
                    let usage = (UnionVariantUsage moduleName valuename Set.empty)
                    let lookupCollection = unionToLookupCollection moduleName valuename union

                    typeComponents <- lookUpTypesByFragments root lookupCollection
                    let metadata = DeclarationMetadata valuename (Just (Can.TVar valuename)) typeComponents pos False
                    
                    pure 
                        (Just 
                            (Explanation
                                localizer
                                (Src.getName srcModule)
                                metadata
                                []
                            )
                        )

                Just (Ext.Dev.Find.Source.FoundAlias (Just canAlias) (A.At pos alias_)) -> do
                    let (Can.Alias aliasVars aliasedType) = canAlias
                    let placeholderAliasType = Can.Holey aliasedType

                    let moduleName = Can._name canMod
                    let usage = AliasUsage moduleName valuename [] placeholderAliasType

                    let lookupCollection = LookupCollection (Map.singleton moduleName (Map.singleton valuename usage) )
                                                 & getExternalTypeUsages aliasedType

                    typeComponents <- lookUpTypesByFragments root lookupCollection
                    let metadata = DeclarationMetadata valuename (Just aliasedType) typeComponents pos False
                    
                    pure 
                        (Just 
                            (Explanation
                                localizer
                                (Src.getName srcModule)
                                metadata
                                []
                            )
                        )

                Just val -> do
                    pure Nothing

        (_, _) ->
             pure Nothing



explainAtLocation :: String -> Watchtower.Editor.PointLocation -> IO (Maybe Explanation)
explainAtLocation root location@(Watchtower.Editor.PointLocation path _) = do
    (Ext.CompileProxy.Single source warnings interfaces canonical compiled) <- Ext.CompileProxy.loadSingle root path
    case (source, canonical) of
        (Right srcModule, Just canMod) -> do
            let localizer = Reporting.Render.Type.Localizer.fromModule srcModule
            let found = fmap (Ext.Dev.Find.Source.withCanonical canMod) (Ext.Dev.Find.Source.definitionAtPoint location srcModule)
            case found of
                Nothing ->
                    pure Nothing

                Just (Ext.Dev.Find.Source.FoundValue (Just def) (A.At pos val)) -> 
                    do
                        let (refCollection@(ReferenceCollection refMap usedTypes refUnions usedModules)) = getFoundDefTypes def emptyRefCollection
                        
                       
                        foundTypeRefs <- lookUpTypesByFragments root 
                                            (toLookupCollection refCollection)

                        Ext.Log.log Ext.Log.Misc (show (List.length foundTypeRefs))
            
                        let refs = Map.elems refMap 

                        metadata <- getDeclarationMetadata root pos def

                        pure 
                            (Just 
                                (Explanation
                                    localizer
                                    (Src.getName srcModule)
                                    metadata
                                    (refs <>  fmap pairToReference foundTypeRefs)
                                )
                            )
                
                Just (Ext.Dev.Find.Source.FoundUnion (Just union) (A.At pos srcUnion)) ->
                    pure Nothing

                Just (Ext.Dev.Find.Source.FoundAlias (Just canAlias) (A.At pos alias_)) ->
                    pure Nothing
                
                _ ->
                    pure Nothing

        (_, _) ->
             pure Nothing




toLookupCollection :: ReferenceCollection -> LookupCollection
toLookupCollection (ReferenceCollection refMap types refUnions usedMods) =
    let
        -- This needs to come first because we want other instances 
        -- to overwrite it if available
        -- This is because UnionVariantUsage does not have type variable defs
        lookupCollectionWithUnions = 
            Map.foldrWithKey
                (\(canMod, unionName) (unionDef, usages) found -> 
                    let
                        new = UnionVariantUsage canMod unionName usages
                    in
                    addFragment new found
                )
                emptyLookup
                refUnions

        lookup =
            List.foldl (flip getExternalTypeUsages) lookupCollectionWithUnions types
    in
    Map.elems refMap
        & List.foldl getTypeLookups lookup

unionToLookupCollection :: ModuleName.Canonical -> Name.Name -> Can.Union -> LookupCollection
unionToLookupCollection moduleName unionName union =
    let
        variantList = Can._u_alts union
        externalTypes = 
            List.concatMap 
                (\(Can.Ctor _ _ _ types) -> types)
                variantList

        toUsage (Can.Ctor name _ _ types) =
            UnionVariantUsage moduleName unionName (Set.singleton name)

        variants = Map.fromList 
                    (List.map 
                        (\ctor ->
                            (unionName, toUsage ctor)
                        ) 
                        variantList
                    )

        baseCollection =  LookupCollection (Map.singleton moduleName variants)
    in
    List.foldr 
        getExternalTypeUsages
        baseCollection
        externalTypes


getTypeLookups :: LookupCollection -> Reference -> LookupCollection
getTypeLookups found ref =
   case ref of
      ValueReference source name type_ ->
            case source of
                External canModName ->
                    found
                        & addFragment (Definition canModName name type_)
                        & getExternalTypeUsages type_ 

                LetValue ->
                    getExternalTypeUsages type_ found

                TopLevelDeclaration ->
                    getExternalTypeUsages type_ found
      
      TypeReference fragment lookup ->
            found



getFragmentCanonicalName :: TypeUsage -> ModuleName.Canonical 
getFragmentCanonicalName frag =
    case frag of
        TypeUsage can name types ->
            can

        AliasUsage can name vars aliasType ->
            can

        Definition can name type_ ->
            can
        
        UnionVariantUsage can name usagse ->
            can


getFragmentName :: TypeUsage -> Name
getFragmentName frag =
    case frag of
        TypeUsage can name types ->
            name

        AliasUsage can name vars aliasType ->
            name

        Definition can name type_ ->
            name

        UnionVariantUsage can name usages ->
            name


data LookupCollection =
    LookupCollection
        { _fragments :: Map.Map ModuleName.Canonical (Map.Map Name TypeUsage)
        }
        deriving (Eq, Show)


emptyLookup :: LookupCollection
emptyLookup =
    LookupCollection (Map.empty)


addFragment :: TypeUsage -> LookupCollection -> LookupCollection
addFragment fragment (LookupCollection fragments) =
    LookupCollection 
        (Map.alter 
            (\maybeValue ->
                case maybeValue of
                    Nothing ->
                        Just (Map.singleton (getFragmentName fragment) fragment)
                    
                    Just set ->
                        Just (Map.insert (getFragmentName fragment) fragment set)
            )
            (getFragmentCanonicalName fragment)
            fragments
        )

getExternalTypeUsages :: Can.Type -> LookupCollection -> LookupCollection
getExternalTypeUsages canType collection =
    case canType of
        Can.TLambda one two ->
            collection
                & getExternalTypeUsages one
                & getExternalTypeUsages two

        Can.TVar name ->
            collection

        Can.TType canMod name varTypes ->
            let 
                newCollections = 
                        addFragment (TypeUsage canMod name varTypes)
                            collection
            in
            List.foldl 
                (\innerCollection typeVar -> 
                    getExternalTypeUsages typeVar innerCollection
                )
                newCollections
                varTypes

        Can.TRecord fields extensibleName ->
            Map.foldl 
                (\innerCollection (Can.FieldType _ fieldType) -> 
                    getExternalTypeUsages fieldType innerCollection
                )
                collection
                fields

        Can.TUnit ->
            collection

        Can.TTuple one two Nothing ->
              collection 
                & getExternalTypeUsages one 
                & getExternalTypeUsages two

        Can.TTuple one two (Just three) ->
            collection
                & getExternalTypeUsages one 
                & getExternalTypeUsages two 
                & getExternalTypeUsages three
        
        Can.TAlias canMod name varTypes aliasType ->
            addFragment (AliasUsage canMod name varTypes aliasType)
                collection


lookUpTypesByFragments :: String -> LookupCollection -> IO [ (TypeUsage, Ext.Dev.Lookup.LookupResult) ]
lookUpTypesByFragments root (LookupCollection fragments) =
    List.concat <$> Monad.mapM (findFragment root) (Map.toList fragments)
    

pairToReference :: (TypeUsage, Ext.Dev.Lookup.LookupResult) -> Reference
pairToReference (fragment, lookup) =
    TypeReference fragment lookup

findFragment :: String -> ( ModuleName.Canonical, Map.Map Name TypeUsage) -> IO [  (TypeUsage, Ext.Dev.Lookup.LookupResult) ]      
findFragment root (canMod, fragmentMap) =
    if isSkippable canMod then 
        pure []

    else do 
        definitions <- Ext.Dev.Lookup.lookupDefinitionMany root (ModuleName._module canMod) (Map.keys fragmentMap)
        pure 
            (List.foldl 
                (\gathered (fragname, frag) ->
                    case Map.lookup fragname definitions of
                        Nothing ->
                            gathered

                        Just fragDef ->
                            (frag, fragDef) : gathered
                ) 
                []
                (Map.toList fragmentMap)
            )

   
{-| Skip internalmodules

-}
isSkippable :: ModuleName.Canonical -> Bool
isSkippable (ModuleName.Canonical (Package.Name author project) modName) =
    let 
        projectString = Utf8.toChars project
    in
    Utf8.toChars author == "elm"
        && (projectString == "core"
             || projectString == "browser"
           )
    



getDeclarationMetadata root pos foundDef =
    case foundDef of
        Ext.Dev.Find.Source.Def def ->
            getCanDeclarationMetadata root pos def False

        Ext.Dev.Find.Source.DefRecursive def otherDefs ->
            getCanDeclarationMetadata root pos def True
            

getCanDeclarationMetadata root pos def recursive =
    case def of
        Can.Def (A.At _ name) patterns expr ->
            pure (DeclarationMetadata name Nothing [] pos recursive)
        
        Can.TypedDef (A.At _ name) freevars patterns expr type_ -> do 

            -- LookupCollection
            let lookupCollection = getExternalTypeUsages type_ emptyLookup

            typeComponents <- lookUpTypesByFragments root lookupCollection
            
            pure (DeclarationMetadata name (Just type_) typeComponents pos recursive)


{-| REFERENCE COLLECTION -}

data ReferenceCollection =
    ReferenceCollection 
        { _refs :: Map.Map (Source, Name) Reference
        , _types :: [ Can.Type ]
        , _unionRef ::  Map.Map (ModuleName.Canonical, Name) (Can.Union, Set.Set Name)
        , _used :: Set.Set (ModuleName.Canonical, Name)
        }


emptyRefCollection :: ReferenceCollection
emptyRefCollection =
    ReferenceCollection Map.empty [] Map.empty Set.empty


addRef :: Reference -> ReferenceCollection -> ReferenceCollection
addRef ref (ReferenceCollection refs usedTypes unions usedModules) =
    case ref of
        ValueReference mod name type_ ->
            ReferenceCollection 
                (Map.insert (mod, name) ref refs)
                usedTypes
                unions
                usedModules
        
        TypeReference typeUsage lookup ->
            let 
                mod = getFragmentCanonicalName typeUsage

                name = getFragmentName typeUsage
            in
            ReferenceCollection 
                (Map.insert (External mod, name) ref refs)
                usedTypes
                unions
                usedModules


addType :: Can.Type -> ReferenceCollection -> ReferenceCollection
addType type_ (ReferenceCollection refs usedTypes unions usedModules) =
    ReferenceCollection 
        refs
        (type_ : usedTypes)
        unions
        usedModules


addUnion :: ModuleName.Canonical -> Name -> Can.Union -> Name -> ReferenceCollection -> ReferenceCollection
addUnion canName typeName unionDef variantName (ReferenceCollection refs usedTypes unions usedModules) =
    ReferenceCollection 
        refs
        usedTypes
        (Map.alter 
            (\maybeValue -> 
             case maybeValue of
                    Nothing ->
                        Just (unionDef, Set.singleton variantName)
                    
                    Just (existingUnionDef, set) ->
                        Just (existingUnionDef, Set.insert variantName set)
                
            )
            (canName, typeName)
            unions
        )
        usedModules





{-| Traverse and build reference collection
-}



getFoundDefTypes :: Ext.Dev.Find.Source.Def -> ReferenceCollection -> ReferenceCollection
getFoundDefTypes foundDef collec =
    case foundDef of
        Ext.Dev.Find.Source.Def def ->
            getDefTypes True def collec

        Ext.Dev.Find.Source.DefRecursive def otherDefs ->
            getDefTypes True def collec


getDefTypes :: Bool -> Can.Def ->  ReferenceCollection ->  ReferenceCollection
getDefTypes isTopLevel foundDef collec =
    case foundDef of
        Can.Def locatedName patterns expr ->
            patterns
                & List.foldl (flip getPatternTypes) collec 
                & getExprTypes expr

        Can.TypedDef (A.At namePos name) freeVars patterns expr type_ ->
            patterns
                & List.foldl 
                    (\innerCollec (pattern, patternType) ->  
                        innerCollec
                            & addType patternType
                            & getPatternTypes pattern
                    ) 
                    collec
                & getExprTypes expr
                & addRef (ValueReference (if isTopLevel then TopLevelDeclaration else LetValue) name (gatherFunctionType patterns type_))


gatherFunctionType :: [ (Can.Pattern, Can.Type) ] -> Can.Type -> Can.Type
gatherFunctionType patterns resultType  =
    case patterns of
        [] -> resultType

        (pattern, type_) : remain ->
            Can.TLambda type_ 
                (gatherFunctionType remain resultType)
            

getPatternTypes :: Can.Pattern -> ReferenceCollection -> ReferenceCollection
getPatternTypes pattern collec =
    case A.toValue pattern of
        Can.PAnything ->
            collec

        Can.PVar name ->
            collec

        Can.PRecord names ->
            collec

        Can.PAlias innerPattern name ->
            getPatternTypes innerPattern collec

        Can.PUnit ->
            collec

        Can.PTuple onePattern twoPattern Nothing ->
             collec
                & getPatternTypes onePattern 
                & getPatternTypes twoPattern
        
        Can.PTuple onePattern twoPattern (Just threePattern) ->
            collec
                & getPatternTypes onePattern 
                & getPatternTypes twoPattern
                & getPatternTypes threePattern 

        Can.PList patterns ->
            patterns
                & List.foldl (flip getPatternTypes) collec 

        Can.PCons onePattern twoPattern ->
              collec
                & getPatternTypes onePattern 
                & getPatternTypes twoPattern

        Can.PBool union bool ->
            collec

        Can.PChr str ->
            collec

        Can.PStr str ->
            collec

        Can.PInt int ->
            collec

        Can.PCtor canName typeName union name index args ->
            -- We may want to return `Union` from ctor!
            collec
                & addUnion canName typeName union name



getExprTypes :: Can.Expr -> ReferenceCollection -> ReferenceCollection
getExprTypes expr collec =
    case A.toValue expr of
        Can.VarLocal name -> 
            collec

        Can.VarTopLevel modName name -> 
            collec

        Can.VarKernel internalName name -> 
            collec

        Can.VarForeign modName name annotation -> 
            collec
                & annotationToType modName name annotation

        Can.VarCtor opts modName name zeroBasedIndex annotation -> 
            collec
                & annotationToType modName name annotation

        Can.VarDebug modName name annotation -> 
            collec
                & annotationToType modName name annotation

        Can.VarOperator opName modName name annotation  -> 
            collec
                & annotationToType modName name annotation

        Can.Chr str -> 
            collec

        Can.Str str -> 
            collec

        Can.Int int -> 
            collec

        Can.Float float -> 
            collec

        Can.List innerExprs -> 
            List.foldl (flip getExprTypes) collec innerExprs

        Can.Negate inner -> 
            getExprTypes inner collec

        Can.Binop binName modName name annotation one two  -> 
            collec
                & annotationToType modName name annotation 
                & getExprTypes one 
                & getExprTypes two 

        Can.Lambda patterns inner -> 
            List.foldl (flip getPatternTypes) collec patterns
                & getExprTypes inner

        Can.Call inner argExprs -> 
            List.foldl (flip getExprTypes) collec argExprs
                & getExprTypes inner 

        Can.If ifExprs inner -> 
            List.foldl 
                (\innerCollec (oneExpr, twoExpr) -> 
                    innerCollec
                        & getExprTypes oneExpr 
                        & getExprTypes twoExpr 
                )
                collec
                ifExprs
                & getExprTypes inner

        Can.Let innerDef inner -> 
            collec 
                & getDefTypes False innerDef
                & getExprTypes inner

        Can.LetRec innerDefs inner -> 
            List.foldl 
                (\innerCollec letDef -> 
                    innerCollec
                        & getDefTypes False letDef
                )
                collec
                innerDefs
                & getExprTypes inner

        Can.LetDestruct pattern one two -> 
            collec 
                & getPatternTypes pattern 
                & getExprTypes one 
                & getExprTypes two

        Can.Case inner branches -> 
            List.foldl 
                (\innerCollec (Can.CaseBranch casePattern caseExpr) -> 
                    innerCollec
                        & getPatternTypes casePattern 
                        & getExprTypes caseExpr
                )
                collec
                branches 
                & getExprTypes inner
                 
        Can.Accessor name -> 
            collec

        Can.Access inner locatedName -> 
            getExprTypes inner collec

        Can.Update name inner fields -> 
            List.foldl 
                (\innerCollec (Can.FieldUpdate region fieldExpr) -> 
                    getExprTypes fieldExpr innerCollec
                )
                collec
                (Map.elems fields) 
                & getExprTypes inner

        Can.Record fields -> 
            Map.elems fields
                & List.foldl (flip getExprTypes) collec 

        Can.Unit -> 
            collec

        Can.Tuple one two Nothing -> 
            collec
                & getExprTypes one 
                & getExprTypes two 
        
        Can.Tuple one two (Just three) -> 
           collec
                & getExprTypes one 
                & getExprTypes two 
                & getExprTypes three


        Can.Shader source types -> 
            collec


annotationToType modName name (Can.Forall vars type_) collec =
    addRef (ValueReference (External modName) name type_ ) collec
          


{- JSON ENCODING -}

encode :: Explanation -> Json.Encode.Value
encode (Explanation localizer moduleName def references) =
    Json.Encode.object
        [ "moduleName" ==> Json.Encode.name moduleName
        , "definition" ==> encodeDefinitionMetadata localizer def
        -- , "facts" ==>
        --     Json.Encode.list
        --         (encodeFact localizer)
        --         references
        ]
    
encodeDefinitionMetadata :: Reporting.Render.Type.Localizer.Localizer ->  DeclarationMetadata -> Json.Encode.Value
encodeDefinitionMetadata localizer (DeclarationMetadata name maybeType typeComponents region recursive) =
    Json.Encode.object 
        [ "name" ==> Json.Encode.chars (Name.toChars name)
        , "type" ==> 
            (Json.Encode.object 
                [ "signature" ==>
                     case maybeType of
                        Nothing -> Json.Encode.null
                        Just canType ->
                            encodeCanType localizer canType

                , "components" ==>
                    (Json.Encode.object $
                        List.map
                            (\(typeUsage, typeLookup) ->  
                                (Name.toChars (getFragmentName typeUsage)) ==> 
                                     (Json.Encode.object (lookupToJsonFields localizer typeUsage typeLookup))
                            
                            )
                            typeComponents
                    )

                ]
            )
        , "region" ==> Watchtower.Editor.encodeRegion region

        -- Don't know if the below is useful
        -- , "recursive" ==>  Json.Encode.bool recursive
        ]   

toVars :: Can.Type -> [ Name ]
toVars canType =
    case canType of
        Can.TLambda one two ->
            toVars one <> toVars two

        Can.TVar name ->
            [ name ]

        Can.TType canMod name varTypes ->
            [ name ] ++ List.concatMap toVars varTypes

        Can.TRecord fields extensibleName ->
            List.concatMap (\(Can.FieldType _ fieldType) -> toVars fieldType) (Map.elems fields)

        Can.TUnit ->
            []

        Can.TTuple one two Nothing ->
            toVars one <> toVars two

        Can.TTuple one two (Just three) ->
            toVars one <> toVars two <> toVars three

        Can.TAlias canMod name varTypes aliasType ->
            [ name ]


lookupToJsonFields ::
    Reporting.Render.Type.Localizer.Localizer 
        -> TypeUsage 
        -> Ext.Dev.Lookup.LookupResult 
        -> [ (Json.String.String, Json.Encode.Value) ]
lookupToJsonFields localizer fragment lookup =
    [ "source" ==> 
        Ext.Dev.Json.Encode.moduleName (getFragmentCanonicalName fragment)
    , "definition" ==> 
       case lookup of
            Ext.Dev.Lookup.Union maybeComment canUnion ->
                encodeCanUnion localizer fragment maybeComment canUnion

            Ext.Dev.Lookup.Alias maybeComment alias ->
                encodeAlias localizer fragment maybeComment alias

            Ext.Dev.Lookup.Def comment ->
                encodeDefinition localizer fragment comment
    ]
    

encodeFact :: Reporting.Render.Type.Localizer.Localizer -> Reference -> Json.Encode.Value
encodeFact localizer ref =
    case ref of
        ValueReference source name canType ->
            Json.Encode.object 
                [ "source" ==> 
                    case source of
                        External modName ->
                            Ext.Dev.Json.Encode.moduleName modName

                        LetValue ->
                            Json.Encode.chars "let"

                        TopLevelDeclaration ->
                            Json.Encode.chars "declaration"
                , "name" ==> Json.Encode.chars (Name.toChars name)
                , "type" ==> encodeCanType localizer canType
                ]

        TypeReference fragment lookup ->
            Json.Encode.object (lookupToJsonFields localizer fragment lookup)


encodeComment (Src.Comment docComment) =
    Json.Encode.string 
        (Json.String.fromComment docComment)


encodeMaybe encoder maybe =
    case maybe of
        Nothing -> Json.Encode.null
        Just val ->
            encoder val


encodeDefinition :: Reporting.Render.Type.Localizer.Localizer -> TypeUsage ->  Src.Comment -> Json.Encode.Value
encodeDefinition localizer fragment comment  =
    Json.Encode.object 
        [ "type" ==> Json.Encode.chars "definition"
        , "comment" ==> encodeComment comment
        , "signature" ==> (case fragment of
                            Definition canName name type_ ->
                                encodeCanType localizer type_
                            _ -> 
                                Json.Encode.null
                    )
        ] 



encodeAlias :: Reporting.Render.Type.Localizer.Localizer -> TypeUsage ->  Maybe Src.Comment -> Can.Alias -> Json.Encode.Value
encodeAlias localizer fragment maybeComment (Can.Alias vars type_) =
    Json.Encode.object 
        [ "type" ==> Json.Encode.chars "alias"
        , "comment" ==> encodeMaybe encodeComment maybeComment
        , "name" ==> Json.Encode.name (getFragmentName fragment)
        , "args" ==> encodeVarList localizer vars fragment
        , "signature" ==> encodeCanType localizer type_
        , "fields" ==> encodeTypeDefinition localizer type_
        ] 


encodeTypeDefinition :: Reporting.Render.Type.Localizer.Localizer -> Can.Type -> Json.Encode.Value
encodeTypeDefinition localizer type_ =
    case type_ of
        Can.TLambda one two ->
            encodeCanType localizer type_
                
        Can.TVar name ->
            encodeCanType localizer type_

        Can.TType canMod name varTypes ->
            encodeCanType localizer type_

        Can.TRecord fields extensibleName ->
            Json.Encode.dict
                Json.String.fromName
                (\(Can.FieldType _ field) -> encodeTypeDefinition localizer field)
                fields

        Can.TUnit ->
            encodeCanType localizer type_

        Can.TTuple one two Nothing ->
            encodeCanType localizer type_

        Can.TTuple one two (Just three) ->
            encodeCanType localizer type_

        Can.TAlias canMod name varTypes aliasType ->
           encodeCanType localizer type_



encodeCanUnion :: Reporting.Render.Type.Localizer.Localizer -> TypeUsage -> Maybe Src.Comment -> Can.Union -> Json.Encode.Value
encodeCanUnion localizer fragment maybeComment (Can.Union vars ctors i opts) =
    Json.Encode.object 
        [ "type" ==> Json.Encode.chars "union"
        , "comment" ==> encodeMaybe encodeComment maybeComment
        , "name" ==> Json.Encode.name (getFragmentName fragment)
        , "args" ==> encodeVarList localizer vars fragment
        , "variants" ==> Json.Encode.list (encodeUnionVariant localizer) ctors
       
        ] 


encodeVarList :: Reporting.Render.Type.Localizer.Localizer -> [ Name ] -> TypeUsage -> Json.Encode.Value
encodeVarList localizer varNames fragment =
    case fragment of
        TypeUsage canName name varTypes ->
            Json.Encode.list
                (encodeVar localizer)
                (List.zip varNames varTypes)

        AliasUsage canName name varNameTypes aliasType ->
            let 
                varTypes = fmap snd varNameTypes
            in
            Json.Encode.list
                (encodeVar localizer)
                (List.zip varNames varTypes)

        UnionVariantUsage canName name usages ->
            Json.Encode.list id []

        Definition canName name type_ ->
            Json.Encode.list id []



encodeVar :: Reporting.Render.Type.Localizer.Localizer -> (Name, Can.Type) -> Json.Encode.Value
encodeVar localizer (name, type_) =
     Json.Encode.object 
        [ "name" ==> Json.Encode.name name
        , "type" ==> encodeCanType localizer type_
        ]


encodeUnionVariant :: Reporting.Render.Type.Localizer.Localizer -> Can.Ctor  -> Json.Encode.Value
encodeUnionVariant localizer (Can.Ctor name _ _ args) =
    Json.Encode.object
        [ "name" ==> Json.Encode.name name
        , "args" ==> Json.Encode.list (encodeCanType localizer) args
        ]

         
    

encodeCanType :: Reporting.Render.Type.Localizer.Localizer -> Can.Type -> Json.Encode.Value
encodeCanType localizer type_ =
    Json.Encode.chars
        (Reporting.Doc.toLine
            (Reporting.Render.Type.canToDoc localizer Reporting.Render.Type.None type_)
        )
    
    