{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find.Source
  ( definitionNamed
  , definitionAtPoint, Found(..)
  , withCanonical, Def(..)
  , potentialImportSourcesForName
  , importsForQual
  , references
  )
where

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Reporting.Annotation as A
import qualified Watchtower.Editor
import qualified Data.List as List


import Data.Name (Name)
import qualified Data.Map as Map
import Data.Function ((&))
import GHC.Base ((<|>))
import qualified Data.Maybe as Maybe
import Debug.Trace (traceShow)
import qualified Elm.ModuleName as ModuleName

data Found
    = FoundValue (Maybe Def) (A.Located Src.Value)
    | FoundUnion (Maybe Can.Union) (A.Located Src.Union)
    | FoundAlias (Maybe Can.Alias) (A.Located Src.Alias)
    | FoundTVar (A.Located Name)
    | FoundCtor (A.Located Name)
    | FoundPattern Src.Pattern
    | FoundDef Src.Def
    | FoundExternalOpts [Src.Import] Name
    | FoundImport Src.Import
    deriving (Show)


data Def
    = Def Can.Def
    | DefRecursive Can.Def [Can.Def]
    deriving (Show)


withCanonical :: Can.Module -> Found -> Found
withCanonical (Can.Module name exports docs decls unions aliases binops effects) found =
    case found of
        FoundValue _ value@(A.At loc (Src.Value (A.At _ name) patterns_ expr_ maybeType_)) ->
            FoundValue (getDefNamed name decls) value

        FoundUnion _ union@(A.At loc (Src.Union (A.At _ name) _ _)) ->
            FoundUnion (Map.lookup name unions) union

        FoundAlias _ (A.At loc alias_) ->
            FoundAlias (Map.lookup (toAliasName alias_) aliases) (A.At loc alias_)

        FoundCtor _ ->
            found

        FoundTVar _ ->
            found

        FoundPattern _ ->
            found

        FoundDef _ ->
            found

        FoundExternalOpts _ _ ->
            found

        FoundImport _ ->
            found



getDefNamed :: Name -> Can.Decls -> Maybe Def
getDefNamed name decls =
    case decls of
        Can.SaveTheEnvironment -> Nothing

        Can.Declare def moarDecls ->
            if defNamed name def then
                Just (Def def)
            else
                getDefNamed name moarDecls

        Can.DeclareRec def internalDefs moarDecls ->
            if defNamed name def then
                Just (DefRecursive def internalDefs)
            else
                getDefNamed name moarDecls


defNamed :: Name -> Can.Def -> Bool
defNamed name def =
    case def of
        Can.Def (A.At _ defName) _ _ ->
            name == defName

        Can.TypedDef (A.At _ defName) _ _ _ _ ->
            name == defName


definitionNamed :: Name -> Src.Module -> Maybe Found
definitionNamed valueName (Src.Module name exports docs imports values unions aliases infixes effects) =
    find (ctorNamed valueName FoundCtor) unions
        <|> find (withName valueName toValueName (FoundValue Nothing)) values
        <|> find (withName valueName toUnionName (FoundUnion Nothing)) unions
        <|> find (withName valueName toAliasName (FoundAlias Nothing)) aliases


ctorNamed :: Name -> (A.Located Name -> Found) -> A.Located Src.Union -> Maybe Found
ctorNamed name toResult (A.At unionRegion (Src.Union _ _ ctors)) =
    find (withName name id toResult) (map fst ctors)


withName :: Name -> (a -> Name) ->  (A.Located a -> Found) ->  A.Located a -> Maybe Found
withName name getName toFound locatedItem@(A.At _ val) =
    if name == getName val then
        Just (toFound locatedItem)
    else
        Nothing


toValueName :: Src.Value -> Name
toValueName (Src.Value (A.At _ name) _ _ _) =
    name

toUnionName :: Src.Union -> Name
toUnionName (Src.Union (A.At _ name) _ _) =
    name

toAliasName :: Src.Alias -> Name
toAliasName (Src.Alias (A.At _ name) _ _) =
    name


definitionAtPoint :: Watchtower.Editor.PointLocation -> Src.Module -> Maybe Found
definitionAtPoint point srcMod@(Src.Module name exports docs imports values unions aliases infixes effects) =
    typeAtPoint point srcMod
        <|> varAtPoint point srcMod
        <|> find (unionCtorAtPoint point) unions
        <|> find (importAtPoint point) imports
        <|> find (atLocation point (FoundValue Nothing)) values
        <|> find (atLocation point (FoundUnion Nothing)) unions
        <|> find (atLocation point (FoundAlias Nothing)) aliases


atLocation :: Watchtower.Editor.PointLocation -> (A.Located a -> Found) -> A.Located a -> Maybe Found
atLocation (Watchtower.Editor.PointLocation _ point) toFound locatedItem@(A.At region _) =
    if withinRegion point region then
        Just (toFound locatedItem)

    else
        Nothing


find :: (a -> Maybe found) -> [ a ] -> Maybe found
find toResult =
    List.foldl
        (\found located ->
            case found of
                Nothing ->
                    toResult located

                Just _ ->
                    found

        )
        Nothing


importAtPoint :: Watchtower.Editor.PointLocation -> Src.Import -> Maybe Found
importAtPoint (Watchtower.Editor.PointLocation _ point) import_@(Src.Import (A.At region _) alias _) =
    if withinRegion point region || maybe False (withinRegion point . A.toRegion) alias then
        Just (FoundImport import_)

    else
        Nothing


unionCtorAtPoint :: Watchtower.Editor.PointLocation -> A.Located Src.Union -> Maybe Found
unionCtorAtPoint point (A.At region (Src.Union _ _ ctors)) =
    find (atLocation point FoundCtor) (map fst ctors)


withinRegion :: A.Position -> A.Region -> Bool
withinRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow)
        && (row == endRow && col <= endCol || row < endRow)


typeAtPoint :: Watchtower.Editor.PointLocation -> Src.Module -> Maybe Found
typeAtPoint point srcMod@(Src.Module name exports docs imports values unions aliases infixes effects) =
    find (\a -> typeAtPointInValue point a >>= foundFromType srcMod) values
        <|> find (\a -> typeAtPointInAlias point a >>= foundFromType srcMod) aliases
        <|> find (\a -> typeAtPointInUnion point a >>= foundFromType srcMod) unions


foundFromType :: Src.Module -> Src.Type -> Maybe Found
foundFromType srcMod@(Src.Module _ _ _ imports values unions aliases _ _) tipe@(A.At region type_) =
    case type_ of
        Src.TLambda _ _ ->             Nothing
        Src.TVar name ->               Just (FoundTVar (A.At region name))
        Src.TType _ name _ ->          definitionNamed name srcMod
                                           <|> (case potentialImportSourcesForName name imports of
                                                    opt : opts -> Just (FoundExternalOpts (opt : opts) name)
                                                    _ -> Nothing
                                                )
        Src.TTypeQual _ qual name _ -> (case importsForQual qual imports of
                                            opt : opts -> Just (FoundExternalOpts (opt : opts) name)
                                            _ -> Nothing
                                       )
        Src.TRecord _ _ ->             Nothing
        Src.TUnit ->                   Nothing
        Src.TTuple _ _ _ ->            Nothing


typeAtPointInValue :: Watchtower.Editor.PointLocation -> A.Located Src.Value -> Maybe Src.Type
typeAtPointInValue (Watchtower.Editor.PointLocation _ point) (A.At region (Src.Value _ _ _ typeAnn)) =
    if withinRegion point region then
        typeAnn >>= findType point

    else
        Nothing


typeAtPointInAlias :: Watchtower.Editor.PointLocation -> A.Located Src.Alias -> Maybe Src.Type
typeAtPointInAlias (Watchtower.Editor.PointLocation _ point) locatedItem@(A.At region (Src.Alias _ _ type_)) =
    if withinRegion point region then
        findType point type_

    else
        Nothing


typeAtPointInUnion :: Watchtower.Editor.PointLocation -> A.Located Src.Union -> Maybe Src.Type
typeAtPointInUnion (Watchtower.Editor.PointLocation _ point) (A.At region (Src.Union _ _ ctors))=
    if withinRegion point region then
        find (findType point) (concatMap snd ctors)

    else
        Nothing


findType :: A.Position -> Src.Type -> Maybe Src.Type
findType point tipe@(A.At region type_) =
    if withinRegion point region then
        case refineTypeMatch point tipe of
            Nothing -> Just tipe
            refined -> refined
    else
        Nothing


refineTypeMatch :: A.Position -> Src.Type -> Maybe Src.Type
refineTypeMatch point tipe@(A.At region type_) =
    case type_ of
        Src.TLambda arg ret ->          dive arg <|> find dive [ret]
        Src.TVar _ ->                   Nothing
        Src.TType _ _ tvars ->          find dive tvars
        Src.TTypeQual _ _ _ tvars ->    find dive tvars
        Src.TRecord fields extRecord -> find dive (map snd fields) -- TODO: Check extRecord
        Src.TUnit ->                    Nothing
        Src.TTuple a b rest ->          find dive (a : b : rest)

    where
        dive = findType point


potentialImportSourcesForName :: Name -> [Src.Import] -> [Src.Import]
potentialImportSourcesForName name =
    -- FIXME: if there is an explicit import, ignore others
    List.filter
        (\(Src.Import (A.At _ importName) alias exposing) ->
            case exposing of
                Src.Open ->
                    -- FIXME: should be True, if it exposes everything,
                    -- the name is potentially from there
                    False

                Src.Explicit exposed ->
                    List.any
                        (\a ->
                            case a of
                                Src.Upper (A.At _ uname) privacy ->
                                    name == uname

                                _ ->
                                    False
                       )
                       exposed
        )


importsForQual :: Name -> [Src.Import] -> [Src.Import]
importsForQual qual =
    List.filter
        (\(Src.Import (A.At _ importName) alias _) ->
            importName == qual || (fmap (\(A.At _ aliasName) -> aliasName) alias) == Just qual
        )


varAtPoint :: Watchtower.Editor.PointLocation -> Src.Module -> Maybe Found
varAtPoint point srcMod@(Src.Module name exports docs imports values unions aliases infixes effects) =
    find (exprAtPointInValue srcMod point) values


exprAtPointInValue
    :: Src.Module
    -> Watchtower.Editor.PointLocation
    -> A.Located Src.Value
    -> Maybe Found
exprAtPointInValue srcMod (Watchtower.Editor.PointLocation _ point) (A.At region (Src.Value _ patterns expr typeAnn)) =
    if withinRegion point region then
        findExpr srcMod point patterns expr

    else
        Nothing


findExpr :: Src.Module -> A.Position -> [Src.Pattern] -> Src.Expr -> Maybe Found
findExpr srcMod point foundPatterns expr@(A.At region _) =
    if withinRegion point region then
        refineExprMatch srcMod point foundPatterns expr

    else
        Nothing


refineExprMatch :: Src.Module -> A.Position -> [Src.Pattern] -> Src.Expr -> Maybe Found
refineExprMatch srcMod@(Src.Module _ _ _ imports _ _ _ _ _) point foundPatterns (A.At _ expr_) = do
    traceShow expr_ (pure ())

    case expr_ of
        Src.Chr _ ->
            Nothing

        Src.Str _ ->
            Nothing

        Src.Int _ ->
            Nothing

        Src.Float _ ->
            Nothing

        Src.Var _ name ->
            (find (findPatternIntroducing name) foundPatterns & fmap FoundPattern)
                <|> definitionNamed name srcMod
                <|> (case potentialImportSourcesForName name imports of
                        opt : opts -> Just (FoundExternalOpts (opt : opts) name)
                        _ -> Nothing
                    )

        Src.VarQual varType qual name ->
            case importsForQual qual imports of
                opt : opts -> Just (FoundExternalOpts (opt : opts) name)
                _ -> Nothing

        Src.List exprs ->
            find dive exprs

        Src.Op _ ->
            Nothing

        Src.Negate expr ->
            dive expr

        Src.Binops exprsAndNames expr ->
            find dive (map fst exprsAndNames)
                <|> dive expr

        Src.Lambda patterns expr ->
            find (findPattern srcMod point foundPatterns) patterns
                <|> extendAndDive patterns expr

        Src.Call expr exprs ->
            find dive exprs
                <|> dive expr

        Src.If listTupleExprs expr ->
            find (\(one, two) -> dive one <|> dive two) listTupleExprs
                <|> dive expr

        Src.Let defs expr ->
            find (findDef srcMod point foundPatterns . A.toValue) defs
                <|> dive expr

        Src.Case expr branches ->
            dive expr
                <|> find
                    (\(pattern, branchExpr) ->
                        findPattern srcMod point foundPatterns pattern
                            <|> extendAndDive [pattern] branchExpr
                    )
                    branches

        Src.Accessor _ ->
            Nothing

        Src.Access expr _ ->
            dive expr

        Src.Update _ fields ->
            find (\(_, fieldExpr) -> dive fieldExpr) fields

        Src.Record fields ->
            find (\(_, fieldExpr) -> dive fieldExpr) fields

        Src.Unit ->
            Nothing

        Src.Tuple exprA exprB exprs ->
            find dive (exprA : exprB : exprs)

        Src.Shader _ _ ->
            Nothing

    where
        dive = findExpr srcMod point foundPatterns

        extendAndDive newPatterns = findExpr srcMod point (newPatterns ++ foundPatterns)


findDef :: Src.Module -> A.Position -> [Src.Pattern] -> Src.Def -> Maybe Found
findDef srcMod point foundPatterns def =
    case def of
        Src.Define locatedName patterns expr type_ ->
            find (findPattern srcMod point foundPatterns) patterns
                <|> findExpr srcMod point (patterns ++ foundPatterns) expr
                <|> (type_ >>= findType point >>= foundFromType srcMod)
                <|> (if withinRegion point (A.toRegion locatedName) then
                        Just (FoundDef def)

                     else
                        Nothing
                    )

        Src.Destruct pattern expr ->
          findPattern srcMod point foundPatterns pattern
              <|> findExpr srcMod point (pattern : foundPatterns) expr
              <|> (if withinRegion point (A.toRegion pattern) then
                      Just (FoundDef def)

                   else
                      Nothing
                  )

findPattern :: Src.Module -> A.Position -> [Src.Pattern] -> Src.Pattern -> Maybe Found
findPattern srcMod point foundPatterns pattern@(A.At region _) =
    if withinRegion point region then
        refinePatternMatch srcMod point foundPatterns pattern

    else
        Nothing


refinePatternMatch :: Src.Module -> A.Position -> [Src.Pattern] -> Src.Pattern -> Maybe Found
refinePatternMatch srcMod@(Src.Module _ _ _ imports _ _ _ _ _) point foundPatterns (A.At _ pattern_) =
    case pattern_ of
        Src.PAnything ->
            Nothing

        Src.PVar name ->
            (find (findPatternIntroducing name) foundPatterns & fmap FoundPattern)
                <|> definitionNamed name srcMod
                <|> (case potentialImportSourcesForName name imports of
                        opt : opts -> Just (FoundExternalOpts (opt : opts) name)
                        _ -> Nothing
                    )

        Src.PRecord _ ->
            Nothing

        Src.PAlias pattern name ->
            findPattern srcMod point foundPatterns pattern

        Src.PUnit ->
            Nothing

        Src.PTuple a b rest ->
            find (findPattern srcMod point foundPatterns) (a : b : rest)

        Src.PCtor _ name patterns ->
            find (findPattern srcMod point foundPatterns) patterns
                <|> definitionNamed name srcMod
                <|> (case potentialImportSourcesForName name imports of
                        opt : opts -> Just (FoundExternalOpts (opt : opts) name)
                        _ -> Nothing
                    )

        Src.PCtorQual _ qual name patterns ->
            find (findPattern srcMod point foundPatterns) patterns
                <|> (case importsForQual qual imports of
                        opt : opts -> Just (FoundExternalOpts (opt : opts) name)
                        _ -> Nothing
                    )

        Src.PList patterns ->
            find (findPattern srcMod point foundPatterns) patterns

        Src.PCons a b ->
            find (findPattern srcMod point foundPatterns) [a, b]

        Src.PChr _ ->
            Nothing

        Src.PStr _ ->
            Nothing

        Src.PInt _ ->
            Nothing


findPatternIntroducing :: Name -> Src.Pattern -> Maybe Src.Pattern
findPatternIntroducing name pattern@(A.At _ pattern_) =
  case pattern_ of
    Src.PVar pname ->
        if pname == name then
            Just pattern

        else
            Nothing

    Src.PRecord names ->
        if name `elem` map (\(A.At _ name) -> name) names then
            Just pattern

        else
            Nothing

    Src.PAlias subPattern (A.At _ aliasName) ->
        if aliasName == name then
            Just pattern

        else
            findPatternIntroducing name subPattern

    Src.PTuple a b c ->
        find (findPatternIntroducing name) (a : b : c)

    Src.PList subPatterns ->
        find (findPatternIntroducing name) subPatterns

    Src.PCons a b ->
        findPatternIntroducing name a <|> findPatternIntroducing name b

    Src.PCtor _ _ args ->
        find (findPatternIntroducing name) args

    _ ->
        Nothing



{-| References -}


-- FIXME: currently only able to find externals
references :: ModuleName.Raw -> Name -> Src.Module -> [A.Region]
references moduleName name srcMod = do
    if moduleName == Src.getName srcMod then
        localReferenceNamed name srcMod

    else
        Src._imports srcMod
            & List.find
                (\(Src.Import (A.At _ importName) _ _) ->
                    importName == moduleName
                )
            & fmap (\a -> referenceNamed a name srcMod)
            & Maybe.fromMaybe []




referenceNamed :: Src.Import -> Name -> Src.Module -> [A.Region]
referenceNamed import_ name srcMod@(Src.Module _ _ _ imports values _ _ _ _) =
    List.concatMap (namedInValue import_ name) values


namedInValue :: Src.Import -> Name -> A.Located Src.Value -> [A.Region]
namedInValue import_ name (A.At _ (Src.Value _ _ expr type_)) =
    namedInExpr import_ name [] expr
        ++ (case type_ of
            Just tipe -> namedInType import_ name [] tipe
            Nothing -> []
           )

namedInExpr :: Src.Import -> Name -> [A.Region] -> Src.Expr -> [A.Region]
namedInExpr import_ name foundRegions (A.At region expr_) =
    case expr_ of
        Src.Chr _ ->
            foundRegions

        Src.Str _ ->
            foundRegions

        Src.Int _ ->
            foundRegions

        Src.Float _ ->
            foundRegions

        Src.Var _ varName ->
            case Src._exposing import_ of
                Src.Open ->
                    if varName == name then
                        region : foundRegions

                    else
                        foundRegions

                Src.Explicit exposed ->
                    if varName == name then
                        List.foldl
                            (\foundRegions exposedName ->
                                case exposedName of
                                    Src.Upper (A.At _ name_) _ ->
                                        if name_ == name then
                                            region : foundRegions

                                        else
                                            foundRegions

                                    Src.Lower (A.At _ name_) ->
                                        if name == name then
                                            region : foundRegions

                                        else
                                            foundRegions

                                    Src.Operator _ _ ->
                                        foundRegions
                            )
                            foundRegions
                            exposed

                    else
                        foundRegions

        Src.VarQual _ qual varName->
            let
                importName =
                    case Src._alias import_ of
                        Just alias -> A.toValue alias
                        Nothing -> A.toValue (Src._import import_)
            in
            if importName == qual && varName == name then
                region : foundRegions

            else
                foundRegions

        Src.List exprs ->
            List.foldl (namedInExpr import_ name) foundRegions exprs

        Src.Op opName ->
            case Src._exposing import_ of
                Src.Open ->
                    if opName == name then
                        region : foundRegions

                    else
                        foundRegions

                Src.Explicit exposed ->
                    if opName == name then
                        List.foldl
                            (\foundRegions exposedName ->
                                case exposedName of
                                    Src.Operator _ name_ ->
                                         if name_ == name then
                                            region : foundRegions

                                        else
                                            foundRegions

                                    _ ->
                                        foundRegions
                            )
                            foundRegions
                            exposed

                    else
                        foundRegions

        Src.Negate expr ->
            namedInExpr import_ name foundRegions expr

        Src.Binops exprsAndNames expr ->
            List.foldl
                (\foundRegions (expr_, _) ->
                    namedInExpr import_ name foundRegions expr_
                )
                (namedInExpr import_ name foundRegions expr)
                exprsAndNames

        Src.Lambda patterns expr ->
            namedInExpr import_ name foundRegions expr

        Src.Call expr exprs ->
            List.foldl
                (namedInExpr import_ name)
                (namedInExpr import_ name foundRegions expr)
                exprs

        Src.If listTupleExprs expr ->
            List.foldl
                (\foundRegions (one, two) ->
                    namedInExpr import_ name (namedInExpr import_ name foundRegions one) two
                )
                (namedInExpr import_ name foundRegions expr)
                listTupleExprs

        Src.Let defs expr ->
            List.foldl
                (\foundRegions (A.At _ def_) ->
                    case def_ of
                        Src.Define (A.At _ name_) _ expr_ type_ ->
                            namedInExpr import_ name foundRegions expr_
                                ++ (case type_ of
                                        Just tipe -> namedInType import_ name [] tipe
                                        Nothing -> []
                                   )

                        Src.Destruct pattern expr_ ->
                            namedInExpr import_ name foundRegions expr_
                )
                (namedInExpr import_ name foundRegions expr)
                defs

        Src.Case expr branches ->
            List.foldl
                (\foundRegions (pattern, branchExpr) ->
                    namedInExpr import_ name (namedInExpr import_ name foundRegions branchExpr) expr
                )
                (namedInExpr import_ name foundRegions expr)
                branches

        Src.Accessor _ ->
            foundRegions

        Src.Access expr _ ->
            namedInExpr import_ name foundRegions expr

        Src.Update _ fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) ->
                    namedInExpr import_ name foundRegions fieldExpr
                )
                foundRegions
                fields

        Src.Record fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) ->
                    namedInExpr import_ name foundRegions fieldExpr
                )
                foundRegions
                fields


        Src.Unit ->
            foundRegions

        Src.Tuple exprA exprB exprs ->
            List.foldl
                (namedInExpr import_ name)
                foundRegions
                (exprA : exprB : exprs)

        Src.Shader _ _ ->
            foundRegions


namedInType :: Src.Import -> Name -> [A.Region] -> Src.Type -> [A.Region]
namedInType import_ name foundRegions (A.At region type_) =
    case type_ of
        Src.TLambda arg ret -> localNamedInType name (localNamedInType name foundRegions arg) ret
        Src.TVar varName -> 
            if name == varName then 
                region : foundRegions
            else
                foundRegions

        Src.TType _ typeName tvars -> 
            case Src._exposing import_ of
                Src.Open ->
                    if typeName == name then
                        region : foundRegions

                    else
                        foundRegions

                Src.Explicit exposed ->
                    if typeName == name then
                        List.foldl
                            (\foundRegions exposedName ->
                                case exposedName of
                                    Src.Upper (A.At _ name_) _ ->
                                        if name_ == name then
                                            region : foundRegions

                                        else
                                            foundRegions

                                    Src.Lower (A.At _ name_) ->
                                        if name == name then
                                            region : foundRegions

                                        else
                                            foundRegions

                                    Src.Operator _ _ ->
                                        foundRegions
                            )
                            foundRegions
                            exposed

                    else
                        foundRegions

        Src.TTypeQual _ qual varName tvars -> 
            let
                importName =
                    case Src._alias import_ of
                        Just alias -> A.toValue alias
                        Nothing -> A.toValue (Src._import import_)
            in
            if importName == qual && varName == name then
                region : List.foldl (localNamedInType name) foundRegions tvars

            else
                List.foldl (localNamedInType name) foundRegions tvars

        Src.TRecord fields extRecord -> List.foldl (localNamedInType name) foundRegions (map snd fields)
        Src.TUnit -> foundRegions
        Src.TTuple a b rest -> List.foldl (localNamedInType name) (localNamedInType name (localNamedInType name foundRegions b) a) rest


localReferenceNamed :: Name -> Src.Module -> [A.Region]
localReferenceNamed name srcMod@(Src.Module _ _ _ imports values _ _ _ _) =
    List.concatMap (localNamedInValue name) values


localNamedInValue :: Name -> A.Located Src.Value -> [A.Region]
localNamedInValue name (A.At _ (Src.Value _ _ expr type_)) =
    localNamedInExpr name [] expr
        ++ (case type_ of
                Just tipe -> localNamedInType name [] tipe
                Nothing -> []
           )


localNamedInExpr :: Name -> [A.Region] -> Src.Expr -> [A.Region]
localNamedInExpr name foundRegions (A.At region expr_) =
    case expr_ of
        Src.Chr _ -> foundRegions
        Src.Str _ -> foundRegions
        Src.Int _ -> foundRegions
        Src.Float _ -> foundRegions
        Src.Var _ varName -> if varName == name then region : foundRegions else foundRegions
        Src.VarQual _ qual varName-> foundRegions
        Src.List exprs -> List.foldl (localNamedInExpr name) foundRegions exprs
        Src.Op opName -> if opName == name then region : foundRegions else foundRegions
        Src.Negate expr -> localNamedInExpr name foundRegions expr

        Src.Binops exprsAndNames expr ->
            List.foldl
                (\foundRegions (expr_, _) -> localNamedInExpr name foundRegions expr_)
                (localNamedInExpr name foundRegions expr)
                exprsAndNames

        Src.Lambda patterns expr -> localNamedInExpr name foundRegions expr
        Src.Call expr exprs ->
            List.foldl
                (localNamedInExpr name)
                (localNamedInExpr name foundRegions expr)
                exprs

        Src.If listTupleExprs expr ->
            List.foldl
                (\foundRegions (one, two) ->
                    localNamedInExpr name (localNamedInExpr name foundRegions one) two
                )
                (localNamedInExpr name foundRegions expr)
                listTupleExprs

        Src.Let defs expr ->
            List.foldl
                (\foundRegions (A.At _ def_) ->
                    case def_ of
                        Src.Define (A.At _ name_) _ expr_ type_ ->
                            localNamedInExpr name foundRegions expr_
                                ++ (case type_ of
                                        Just tipe -> localNamedInType name [] tipe
                                        Nothing -> []
                                   )


                        Src.Destruct pattern expr_ ->
                            localNamedInExpr name foundRegions expr_
                )
                (localNamedInExpr name foundRegions expr)
                defs

        Src.Case expr branches ->
            List.foldl
                (\foundRegions (pattern, branchExpr) ->
                    localNamedInExpr name (localNamedInExpr name foundRegions branchExpr) expr
                )
                (localNamedInExpr name foundRegions expr)
                branches

        Src.Accessor _ -> foundRegions
        Src.Access expr _ -> localNamedInExpr name foundRegions expr

        Src.Update _ fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) -> localNamedInExpr name foundRegions fieldExpr)
                foundRegions
                fields

        Src.Record fields ->
            List.foldl
                (\foundRegions (_, fieldExpr) -> localNamedInExpr name foundRegions fieldExpr)
                foundRegions
                fields


        Src.Unit -> foundRegions
        Src.Tuple exprA exprB exprs ->
            List.foldl (localNamedInExpr name)
                foundRegions
                (exprA : exprB : exprs)
        Src.Shader _ _ -> foundRegions


localNamedInType :: Name -> [A.Region] -> Src.Type -> [A.Region]
localNamedInType name foundRegions (A.At region type_) =
    case type_ of
        Src.TLambda arg ret -> localNamedInType name (localNamedInType name foundRegions arg) ret
        Src.TVar varName -> 
            if name == varName then 
                region : foundRegions
            else
                foundRegions

        Src.TType _ typeName tvars -> 
            if name == typeName then 
                region : List.foldl (localNamedInType name) foundRegions tvars
            else
                List.foldl (localNamedInType name) foundRegions tvars

        Src.TTypeQual _ _ _ tvars -> List.foldl (localNamedInType name) foundRegions tvars
        Src.TRecord fields extRecord -> List.foldl (localNamedInType name) foundRegions (map snd fields)
        Src.TUnit -> foundRegions
        Src.TTuple a b rest -> List.foldl (localNamedInType name) (localNamedInType name (localNamedInType name foundRegions b) a) rest

