{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find.Source
  ( definitionNamed
  , definitionAtPoint, Found(..)
  , withCanonical, Def(..)
  , typeAtPoint, FoundType(..)
  , potentialImportSourcesForName
  , importsForQual
  , varAtPoint, FoundVar(..)
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

data Found
    = FoundValue (Maybe Def) (A.Located Src.Value)
    | FoundUnion (Maybe Can.Union) (A.Located Src.Union)
    | FoundAlias (Maybe Can.Alias) (A.Located Src.Alias)
    | FoundCtor (A.Located Name)
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

        FoundUnion _ (union@(A.At loc (Src.Union (A.At _ name) _ _))) ->
            FoundUnion (Map.lookup name unions) union

        FoundAlias _ (A.At loc alias_) ->
            FoundAlias (Map.lookup (toAliasName alias_) aliases) (A.At loc alias_)

        FoundUnionCtor _ ->
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
        & orFind (withName valueName toValueName (FoundValue Nothing)) values
        & orFind (withName valueName toUnionName (FoundUnion Nothing)) unions
        & orFind (withName valueName toAliasName (FoundAlias Nothing)) aliases


ctorNamed :: Name -> (A.Located Name -> Found) -> A.Located Src.Union -> Maybe Found
ctorNamed name toResult (A.At unionRegion (Src.Union _ _ ctors)) =
    find (withName name id toResult) (map fst ctors)


withName :: Name -> (a -> Name) ->  (A.Located a -> Found) ->  A.Located a -> Maybe Found
withName name getName toFound (locatedItem@(A.At _ val)) =
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
definitionAtPoint point (Src.Module name exports docs imports values unions aliases infixes effects) =
    find (atLocation point (FoundValue Nothing)) values
        & orFind (atLocation point (FoundUnion Nothing)) unions
        & orFind (atLocation point (FoundAlias Nothing)) aliases


atLocation :: Watchtower.Editor.PointLocation -> (A.Located a -> Found) -> A.Located a -> Maybe Found
atLocation (Watchtower.Editor.PointLocation _ point) toFound (locatedItem@(A.At region _)) =
    if withinRegion point region then
        Just (toFound locatedItem)
    else
        Nothing

find :: (a -> Maybe found) -> [ a ] -> Maybe found
find toResult items =
    List.foldl
        (\found located ->
            case found of
                Nothing ->
                    (toResult located)

                Just _ ->
                    found

        ) Nothing items

orFind :: (a -> Maybe found) -> [ a ] -> Maybe found -> Maybe found
orFind toResult items previousResult =
    case previousResult of
        Nothing ->
           find toResult items

        _ ->
            previousResult


withinRegion :: A.Position -> A.Region -> Bool
withinRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow)
        && (row == endRow && col <= endCol || row < endRow)


data FoundType
    = FoundTVar (A.Located Name)
    | FoundTType A.Region Name
    | FoundTTypeQual A.Region Name Name
    deriving (Show)


typeAtPoint :: Watchtower.Editor.PointLocation -> Src.Module -> Maybe FoundType
typeAtPoint point srcMod@(Src.Module name exports docs imports values unions aliases infixes effects) =
    find (\a -> typeAtPointInValue point a >>= foundFromType srcMod) values
        & orFind (\a -> typeAtPointInAlias point a >>= foundFromType srcMod) aliases
        & orFind (\a -> typeAtPointInUnion point a >>= foundFromType srcMod) unions


foundFromType :: Src.Module -> Src.Type -> Maybe FoundType
foundFromType srcMod@(Src.Module _ _ _ imports values unions aliases _ _) tipe@(A.At region type_) =
    case type_ of
        Src.TLambda _ _ ->             Nothing
        Src.TVar name ->               Just (FoundTVar (A.At region name))
        Src.TType _ name _ ->          Just (FoundTType region name)
        Src.TTypeQual _ qual name _ -> Just (FoundTTypeQual region qual name)
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
        Src.TLambda arg ret ->          dive arg & orFind dive [ret]
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
            importName == qual || alias == Just qual
        )


data FoundVar
    = FoundVar Name
    | FoundVarQual Name Name
    deriving (Show)


varAtPoint :: Watchtower.Editor.PointLocation -> Src.Module -> Maybe FoundVar
varAtPoint point srcMod@(Src.Module name exports docs imports values unions aliases infixes effects) =
    find
        (\a ->
            exprAtPointInValue point a
                >>= (\a ->
                    case a of
                        Left _ -> Nothing -- FIXME: handle patterns
                        Right expr -> varFromExpr srcMod expr
                )
        )
        values


varFromExpr :: Src.Module -> Src.Expr -> Maybe FoundVar
varFromExpr srcMod@(Src.Module _ _ _ imports values unions aliases infixes effects) expr@(A.At _ expr_) =
    case expr_ of
        Src.Chr _ ->                    Nothing
        Src.Str _ ->                    Nothing
        Src.Int _ ->                    Nothing
        Src.Float _ ->                  Nothing
        Src.Var _ name ->               Just (FoundVar name)
        Src.VarQual varType mod name -> Just (FoundVarQual mod name)
        Src.List _ ->                   Nothing
        Src.Op _ ->                     Nothing
        Src.Negate _ ->                 Nothing
        Src.Binops _ _ ->               Nothing
        Src.Lambda _ _ ->               Nothing
        Src.Call _ _ ->                 Nothing
        Src.If _ _ ->                   Nothing
        Src.Let _ _ ->                  Nothing
        Src.Case _ _ ->                 Nothing
        Src.Accessor _ ->               Nothing
        Src.Access _ _ ->               Nothing
        Src.Update _ _ ->               Nothing
        Src.Record _ ->                 Nothing
        Src.Unit  ->                    Nothing
        Src.Tuple _ _ _ ->              Nothing
        Src.Shader _ _ ->               Nothing


exprAtPointInValue
    :: Watchtower.Editor.PointLocation
    -> A.Located Src.Value
    -> Maybe (Either (Src.Pattern, [Src.Pattern]) Src.Expr)
exprAtPointInValue (Watchtower.Editor.PointLocation _ point) (A.At region (Src.Value _ patterns expr typeAnn)) =
    if withinRegion point region then
        findExpr point patterns expr

    else
        Nothing


findExpr
    :: A.Position
    -> [Src.Pattern]
    -> Src.Expr
    -> Maybe (Either (Src.Pattern, [Src.Pattern]) Src.Expr)
findExpr point foundPatterns expr@(A.At region _) =
    if withinRegion point region then
        case refineExprMatch point foundPatterns expr of
            Nothing ->
                Just (Right expr)

            refined ->
                refined

    else
        Nothing


refineExprMatch
    :: A.Position
    -> [Src.Pattern]
    -> Src.Expr
    -> Maybe (Either (Src.Pattern, [Src.Pattern]) Src.Expr)
refineExprMatch point foundPatterns (A.At _ expr_) =
    case expr_ of
        Src.Chr _ ->
            Nothing

        Src.Str _ ->
            Nothing

        Src.Int _ ->
            Nothing

        Src.Float _ ->
            Nothing

        Src.Var _ _ ->
            Nothing

        Src.VarQual {} ->
            Nothing

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
            find
                (\a ->
                    findPattern point a
                        & fmap (\p -> Left (p, foundPatterns))
                )
                patterns
                <|> extendAndDive patterns expr

        Src.Call expr exprs ->
            find dive exprs
                <|> dive expr

        Src.If listTupleExprs expr ->
            find (\(one, two) -> dive one <|> dive two) listTupleExprs
                <|> dive expr

        Src.Let defs expr ->
            find (findDef point foundPatterns . A.toValue) defs
                <|>
                    extendAndDive
                        (Maybe.mapMaybe (defNamePattern . A.toValue) defs)
                        expr



        Src.Case expr branches ->
            dive expr
                & orFind
                    (\(pattern, branchExpr) ->
                        (findPattern point pattern
                            & fmap (\p -> Left (p, foundPatterns))
                        )
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
        dive = findExpr point foundPatterns

        extendAndDive newPatterns = findExpr point (newPatterns ++ foundPatterns)


findDef
    :: A.Position -> [Src.Pattern]
    -> Src.Def
    -> Maybe (Either (Src.Pattern, [Src.Pattern]) Src.Expr)
findDef point foundPatterns def =
    case def of
        Src.Define locatedName patterns expr _ ->
            find
                (\a ->
                    findPattern point a
                        & fmap (\p -> Left (p, foundPatterns))
                )
                patterns
                <|> findExpr point (patterns ++ foundPatterns) expr

        Src.Destruct pattern expr ->
          (findPattern point pattern
              & fmap (\p -> Left (p, foundPatterns))
          )
              <|> findExpr point (pattern : foundPatterns) expr


defNamePattern :: Src.Def -> Maybe Src.Pattern
defNamePattern def =
    case def of
        Src.Define (A.At region name) _ _ _ ->
            Just $ A.At region $ Src.PVar name

        Src.Destruct _ _ ->
            Nothing


findPattern :: A.Position -> Src.Pattern -> Maybe Src.Pattern
findPattern point pattern@(A.At region _) =
    if withinRegion point region then
        case refinePatternMatch point pattern of
            Nothing ->
                Just pattern

            refined ->
                refined

    else
        Nothing


refinePatternMatch :: A.Position -> Src.Pattern -> Maybe Src.Pattern
refinePatternMatch point (A.At _ pattern_) =
    case pattern_ of
        Src.PAnything -> Nothing
        Src.PVar _ -> Nothing
        Src.PRecord _ -> Nothing
        Src.PAlias pattern name -> findPattern point pattern
        Src.PUnit -> Nothing
        Src.PTuple a b rest -> find (findPattern point) (a : b : rest)
        Src.PCtor _ _ patterns -> find (findPattern point) patterns
        Src.PCtorQual _ _ _ patterns -> find (findPattern point) patterns
        Src.PList patterns -> find (findPattern point) patterns
        Src.PCons a b -> find (findPattern point) [a, b]
        Src.PChr _ -> Nothing
        Src.PStr _ -> Nothing
        Src.PInt _ -> Nothing



