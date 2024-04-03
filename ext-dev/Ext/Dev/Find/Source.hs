{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find.Source
  ( definitionNamed
  , definitionAtPoint, Found(..)
  , withCanonical, Def(..)
  , potentialImportSourcesForName
  , importsForQual
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


data Found
    = FoundValue (Maybe Def) (A.Located Src.Value)
    | FoundUnion (Maybe Can.Union) (A.Located Src.Union)
    | FoundAlias (Maybe Can.Alias) (A.Located Src.Alias)
    | FoundTVar (A.Located Name)
    | FoundCtor (A.Located Name)
    | FoundPattern Src.Pattern
    | FoundDef Src.Def
    | FoundExternalOpts [Src.Import] Name
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
            importName == qual || alias == Just qual
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
