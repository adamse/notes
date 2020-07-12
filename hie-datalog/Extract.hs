{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language DuplicateRecordFields #-}
{-# language DisambiguateRecordFields #-}
{-# language OverloadedStrings #-}
module Extract
  ( main
  , Use(..)
  , Definition(..)
  ) where

import Prelude hiding (init)

import Debug.Trace

import Data.List (intercalate)

import Data.String (fromString)

import Data.Maybe (fromMaybe)

import Control.Monad (guard)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified HieTypes as Hie
import qualified HieBin as Hie

import qualified NameCache
import qualified UniqSupply
import qualified Module
import qualified Name
import qualified Avail
import qualified FieldLabel
import qualified FastString
import qualified SrcLoc
import qualified FastString

main :: IO ()
main = do
  namecache <- init
  (hiefileres, namecache') <- Hie.readHieFile namecache "Extract.hie"
  let hiefile = Hie.hie_file_result hiefileres
  print ("source", Hie.hie_hs_file hiefile)

  let exports = exportedVals hiefile
  putStrLn "exports:"
  mapM_ (putStrLn . ("  " <>) . displayDefinition) exports

  let topvals = topVals hiefile
  putStrLn "top-level names:"
  mapM_ (putStrLn . ("  " <>) . Name.getOccString . fst) topvals

  let uses = internalUses hiefile
  putStrLn "internal uses:"
  mapM_ (putStrLn . ("  " <>) . displayUse) uses

  let modul = Hie.hie_module hiefile
  writeDefinedFacts "exported" exports
  writeDefinedFacts "defined" (map (definitionFromInternalName "main" modul . fst) topvals)
  writeUsesFacts uses

  print ("hie_asts", Map.keys (Hie.getAsts (Hie.hie_asts hiefile)))

  pure ()

writeUsesFacts :: [Use] -> IO ()
writeUsesFacts uses = do
  writeFile "uses.facts" $
    unlines $
    map formatUseFact uses
  where
    formatUseFact Use{modul,name,loc,what} =
      intercalate "\t" $
        [ modul
        , name
        -- , show (fst loc)
        -- , show (snd loc)
        ] <> case what of
          Definition{modul,name} -> [modul, name]

writeDefinedFacts :: String -> [Definition] -> IO ()
writeDefinedFacts relation defns = do
  writeFile (relation <> ".facts") $
    unlines $
    map formatDefn defns
  where
    formatDefn Definition{modul,name,loc=mloc} =
      let loc = fromMaybe (-1,-1) mloc in
      intercalate "\t" [modul, name, show (fst loc), show (snd loc)]


init :: IO NameCache.NameCache
init = do
  uniqsupply <- UniqSupply.mkSplitUniqSupply 'z'
  pure $ NameCache.initNameCache uniqsupply []

displayLoc :: (Int, Int) -> String
displayLoc (x, y) = show x <> ":" <> show y

data Use = Use
  { package :: String
  -- ^ package used in
  , modul :: String
  -- ^ module usekd in
  , name :: String
  -- ^ top level name used in
  , loc :: (Int, Int)
  -- ^ location of use
  , what :: Definition
  -- ^ what was used
  }
  deriving (Show)

displayUse :: Use -> String
displayUse Use{package,modul,name,loc,what} =
  intercalate " " $
    [package, modul, name, displayLoc loc, "-->", displayDefinition what]

overAsts :: Monoid r => (Hie.HieAST x -> r) -> Hie.HieAST x -> r
overAsts f ast = f ast <> foldMap (overAsts f) (Hie.nodeChildren ast)

asdf = ("adf", topVals)

-- | Find top level names and their declaration
topVals :: Hie.HieFile -> [(Name.Name, Hie.HieAST Hie.TypeIndex)]
topVals hie = case ast of
  Just ast -> overAsts findTop ast
  _ -> []
  where
    modul = Hie.hie_module hie
    ast = Map.lookup (fromString (Hie.hie_hs_file hie)) (Hie.getAsts (Hie.hie_asts hie))

    isTop idents =
      not $ Set.null $
      Set.filter
        (\case
          (Hie.ValBind _ Hie.ModuleScope _) -> True
          (Hie.PatternBind Hie.ModuleScope _ _) -> True
          -- (Hie.Decl{}) -> True
          _ -> False)
        idents

    -- findTop :: Hie.HieAST x -> [(Hie.HieAST x, Name.Name)]
    findTop ast = do
      guard (Set.member ("FunBind", "HsBindLR") (Hie.nodeAnnotations (Hie.nodeInfo ast)))
      name <- findIdentifiers isTop ast
      pure (name, ast)

findIdentifiers :: (Set.Set Hie.ContextInfo -> Bool) -> Hie.HieAST x -> [Name.Name]
findIdentifiers check ast =
  foldMap
    (\case
      (Right nm, Hie.IdentifierDetails{identInfo})
        | check identInfo -> [nm]
      _ -> [])
    (Map.toList (Hie.nodeIdentifiers (Hie.nodeInfo ast))) <>
  foldMap (findIdentifiers check) (Hie.nodeChildren ast)

internalUses :: Hie.HieFile -> [Use]
internalUses hie =
  concatMap
    (\(nm, ast) -> overAsts (findInternalCall nm) ast)
    (topVals hie)
  where
    modul = Hie.hie_module hie
    ast = Map.lookup (fromString (Hie.hie_hs_file hie)) (Hie.getAsts (Hie.hie_asts hie))

    findInternalCall topname ast =
      Map.elems $
      Map.mapMaybeWithKey
        (\k v -> case k of
          Right nm
            | Set.member Hie.Use (Hie.identInfo v)
            , not (Name.isInternalName nm)
            -- , Name.nameModule_maybe nm == Just modul
            ->
              Just Use
                { package = "main"
                , modul = Module.moduleNameString (Module.moduleName modul)
                , name = Name.getOccString topname
                , loc =
                  let loc = SrcLoc.realSrcSpanStart (Hie.nodeSpan ast)
                  in (SrcLoc.srcLocLine loc, SrcLoc.srcLocCol loc)

                , what = definitionFromInternalName "main" (Name.nameModule nm) nm
                }
          _ -> Nothing)
        (Hie.nodeIdentifiers (Hie.nodeInfo ast))

data Definition = Definition
  { package :: String
  , modul :: String
  , name :: String
  , loc :: Maybe (Int, Int)
  }
  deriving (Show)

displayDefinition :: Definition -> String
displayDefinition Definition{package,modul,name,loc} =
  intercalate " " $
    [package, modul, name] <>
    maybe [] (pure . displayLoc) loc

-- | internal names do not know which module they belong to
definitionFromInternalName :: String -> Module.Module -> Name.Name -> Definition
definitionFromInternalName pkg mod nm = Definition
  { package = pkg
  , modul = Module.moduleNameString (Module.moduleName mod)
  , name = Name.getOccString nm
  , loc = case SrcLoc.srcSpanStart (SrcLoc.getLoc nm) of
      SrcLoc.RealSrcLoc loc -> Just (SrcLoc.srcLocLine loc, SrcLoc.srcLocCol loc)
      _ -> Nothing
  }

definitionFromName :: String -> Name.Name -> Definition
definitionFromName pkg name = definitionFromInternalName pkg (Name.nameModule name) name

exportedVals :: Hie.HieFile -> [Definition]
exportedVals hie = concatMap f (Hie.hie_exports hie)
  where
    modul = Hie.hie_module hie

    mk nm = definitionFromInternalName "main" modul nm

    f = \case
      Avail.Avail name -> [mk name]
      Avail.AvailTC tyname pieces fields ->
        map mk (getValNames tyname pieces) ++
        map (mk . FieldLabel.flSelector) fields

    -- remove the type name from the list
    getValNames ty [] = []
    getValNames ty (tym:rest)
      | ty == tym = rest
      | otherwise = tym:rest
