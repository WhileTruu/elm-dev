{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find
  ( definition
  , definition2
  , references
  , encodeResult
  , PointRegion (..)
  , Found (..)
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
import qualified Canonicalize.Expression



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
  pure $ Left "Not implemented"



{- Find Definition -}


data Found
    = FoundType Src.Type
    | FoundValue (A.Located Src.Value)
    | FoundUnion (A.Located Src.Union)
    | FoundAlias (A.Located Src.Alias)
    | FoundTVar (A.Located Name)
    deriving (Show)


find :: (A.Located a -> Maybe found) -> [ A.Located a ] -> Maybe found
find toResult =
    -- TODO: stop search after finding?
    List.foldl 
        (\found located ->
            case found of
                Nothing ->
                    toResult located

                Just _ ->
                    found
        )
        Nothing 


-- Helpers


orFind :: (A.Located a -> Maybe found) -> [ A.Located a ] -> Maybe found -> Maybe found
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


atLocation :: Watchtower.Editor.PointLocation -> (A.Located a -> Found) -> A.Located a -> Maybe Found
atLocation (Watchtower.Editor.PointLocation _ point) toFound (locatedItem@(A.At region _)) =
    if withinRegion point region then
        Just (toFound locatedItem)

    else
        Nothing


definition2 :: Watchtower.Editor.PointLocation -> Src.Module -> Maybe Found
definition2 point srcMod@(Src.Module name exports docs imports values unions aliases infixes effects) =
    find (findAnnotation point (typeToFound srcMod)) values
        & orFind (findAlias point (typeToFound srcMod)) aliases
        & orFind (findUnion point (typeToFound srcMod)) unions
        & orFind (atLocation point FoundValue) values
        & orFind (atLocation point FoundUnion) unions
        & orFind (atLocation point FoundAlias) aliases


findAnnotation 
    :: Watchtower.Editor.PointLocation
    -> (Src.Type -> Found)
    -> A.Located Src.Value
    -> Maybe Found
findAnnotation (Watchtower.Editor.PointLocation _ point) toResult (A.At region (Src.Value _ _ _ typeAnn)) =
    if withinRegion point region then
        typeAnn >>= findType point & fmap toResult

    else
        Nothing


findAlias 
    :: Watchtower.Editor.PointLocation 
    -> (Src.Type -> Found) 
    -> A.Located Src.Alias 
    -> Maybe Found
findAlias (Watchtower.Editor.PointLocation _ point) toResult locatedItem@(A.At region (Src.Alias _ _ type_)) = 
    if withinRegion point region then
        findType point type_
        & fmap toResult

    else
        Nothing


findUnion 
    :: Watchtower.Editor.PointLocation 
    -> (Src.Type -> Found) 
    -> A.Located Src.Union 
    -> Maybe Found
findUnion (Watchtower.Editor.PointLocation _ point) toResult (A.At region (Src.Union _ _ ctors))=
    if withinRegion point region then
        find (fmap toResult . findType point) (concatMap snd ctors)

    else
        Nothing


findType :: A.Position -> Src.Type -> Maybe Src.Type
findType point tipe@(A.At region type_) =
    if withinRegion point region then
        case refineTypeMatch point tipe of
            Nothing ->
                Just tipe

            refined ->
                refined
    else
        Nothing


refineTypeMatch :: A.Position -> Src.Type -> Maybe Src.Type
refineTypeMatch point tipe@(A.At region type_) =
    case type_ of
        Src.TLambda arg ret -> 
            dive arg & orFind dive [ret]

        Src.TVar _ ->
            Nothing

        Src.TType _ _ tvars ->
            find dive tvars

        Src.TTypeQual _ _ _ tvars ->
            find dive tvars

        Src.TRecord fields extRecord ->
            -- TODO: Check extRecord
            find dive (map snd fields)

        Src.TUnit ->
            Nothing

        Src.TTuple a b rest ->
            find dive (a : b : rest)

    where
        dive = findType point 


typeToFound :: Src.Module -> Src.Type -> Found
typeToFound srcMod@(Src.Module _ _ _ _ values unions aliases _ _) tipe@(A.At (A.Region point _) type_) =
    case type_ of
        Src.TLambda _ _ ->
            FoundType tipe

        Src.TVar name ->
            -- TODO: To simplify refrerence finding, should TVar specify what 
            -- it's a TVar for (Alias, Union, etc)?
            find findAliasTVar aliases
            <|> find findUnionTVar unions
            & Maybe.fromMaybe (FoundTVar (A.At (A.toRegion tipe) name))

            where

              findAliasTVar (A.At region (Src.Alias (A.At _ _) args _)) =
                  if withinRegion point region then
                      find
                          (\tVar@(A.At _ varName) -> 
                              if withinRegion point region && name == varName then
                                  Just (FoundTVar tVar)
                              else
                                  Nothing
                          )
                          args
                  else
                    Nothing

              findUnionTVar (A.At region (Src.Union (A.At _ _) args _)) =
                  if withinRegion point region then
                      find
                          (\tVar@(A.At _ varName) -> 
                              if withinRegion point region && name == varName then
                                  Just (FoundTVar tVar)
                              else
                                  Nothing
                          )
                          args
                  else
                    Nothing

        Src.TType _ name _ ->
            definitionNamed name srcMod 
            & Maybe.fromMaybe (FoundType tipe)

        Src.TTypeQual _ _ _ _ -> 
            FoundType tipe

        Src.TRecord _ _ -> 
            FoundType tipe

        Src.TUnit -> 
            FoundType tipe

        Src.TTuple _ _ _ -> 
            FoundType tipe


definitionNamed :: Name -> Src.Module -> Maybe Found
definitionNamed valueName (Src.Module name exports docs imports values unions aliases infixes effects) =
    find (withName valueName toValueName FoundValue) values
        & orFind (withName valueName toUnionName FoundUnion) unions
        & orFind (withName valueName toAliasName FoundAlias) aliases


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



{-| References -}


references :: FilePath -> Watchtower.Editor.PointLocation -> IO (Either String [PointRegion])
references root (Watchtower.Editor.PointLocation path point) = do
    pure $ Left "Not implemented"