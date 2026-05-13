{-|
Module      : GlassJar
Description : API for computing and reporting differences between analyzable inputs.
Copyright   : (c) Flechazo, 2026
License     : MIT
Maintainer  : 2558755403@qq.com

Defines behavior for reading archive or filesystem inputs,
classifying structural entry changes, and rendering diff reports
in text, git-diff, JSON, or HTML format.
-}

{-# LANGUAGE OverloadedStrings #-}

module GlassJar
  ( -- * Re-exports from GlassJar.Types
    module GlassJar.Types

    -- * Re-exports from GlassJar.Html
  , formatReportHtml

    -- * Re-exports from GlassJar.Decompile
  , module GlassJar.Decompile

    -- * Re-exports from GlassJar.Report
  , formatReport
  , formatReportGitDiff
  , formatReportJson

    -- * Core Functions
  , readJar
  , readInput
  , diffJars
  , diffJarsWith
  , groupInnerClassDiffs

    -- * Diff Settings
  , DiffSettings (..)
  , defaultDiffSettings
  ) where

import GlassJar.Types
import GlassJar.Html (formatReportHtml)
import GlassJar.Decompile
import GlassJar.Report (formatReport, formatReportGitDiff, formatReportJson)
import GlassJar.Internal (decodeLenient, digestToHex, isClassEntry, stripClassExt, toEntryPath)

import Codec.Archive.Zip
  ( Entry (..)
  , fromEntry
  , toArchiveOrFail
  , zEntries
  )
import Crypto.Hash (hash)
import Data.Bits (complement, shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (foldl', isSuffixOf, partition, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word8)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>), splitExtension, takeFileName)

-------------------------------------------------------------------------------
-- Core Settings
-------------------------------------------------------------------------------

-- | Controls structural-diff comparison behavior.
data DiffSettings = DiffSettings
  { dsUseCrcComparison :: !Bool
  , dsIgnoreClassSameSize :: !Bool
  , dsGroupInnerClasses :: !Bool
  } deriving (Show, Eq)

-- | Provides default settings for structural diff comparison.
defaultDiffSettings :: DiffSettings
defaultDiffSettings = DiffSettings
  { dsUseCrcComparison = True
  , dsIgnoreClassSameSize = False
  , dsGroupInnerClasses = True
  }

-------------------------------------------------------------------------------
-- Core: Reading inputs
-------------------------------------------------------------------------------

-- | Returns the list of analyzable entries found at @path@.
--
-- Supports archive files (JAR/ZIP-like), directories, and single files.
readJar :: FilePath -> IO (Either String [JarEntry])
readJar = readInput

-- | Returns analyzable entries from @path@ using automatic input-kind detection.
readInput :: FilePath -> IO (Either String [JarEntry])
readInput path = do
  isDir <- doesDirectoryExist path
  if isDir
    then Right <$> readDirectoryEntries path
    else do
      isFile <- doesFileExist path
      if not isFile
        then pure $ Left $ "Input path does not exist: " <> path
        else readFileOrArchive path

readFileOrArchive :: FilePath -> IO (Either String [JarEntry])
readFileOrArchive path = do
  raw <- BSL.readFile path
  case toArchiveOrFail raw of
    Right archive ->
      let entries = filter (not . isDirectoryArchiveEntry) (zEntries archive)
      in pure $ Right $ map toJarEntryArchive entries
    Left _ ->
      pure $ Right [toStandaloneFileEntry path raw]

readDirectoryEntries :: FilePath -> IO [JarEntry]
readDirectoryEntries root = do
  rels <- listFilesRecursive root ""
  mapM (toJarEntryFromFile root) rels

listFilesRecursive :: FilePath -> FilePath -> IO [FilePath]
listFilesRecursive root rel = do
  let current = if null rel then root else root </> rel
  names <- listDirectory current
  paths <- mapM walk names
  pure (concat paths)
  where
    walk name = do
      let relPath = if null rel then name else rel </> name
          fullPath = root </> relPath
      isDir <- doesDirectoryExist fullPath
      if isDir
        then listFilesRecursive root relPath
        else pure [relPath]

toJarEntryFromFile :: FilePath -> FilePath -> IO JarEntry
toJarEntryFromFile root rel = do
  let fp = root </> rel
  content <- BSL.readFile fp
  let normName = toEntryPath rel
  pure JarEntry
    { entryName = T.pack normName
    , entrySize = fromIntegral (BSL.length content)
    , entryContent = content
    , entryHash = digestToHex (hash (BSL.toStrict content))
    }

toJarEntryArchive :: Entry -> JarEntry
toJarEntryArchive entry =
  let content = fromEntry entry
  in JarEntry
    { entryName = T.pack (toEntryPath (eRelativePath entry))
    , entrySize = fromIntegral (eUncompressedSize entry)
    , entryContent = content
    , entryHash = digestToHex (hash (BSL.toStrict content))
    }

toStandaloneFileEntry :: FilePath -> BSL.ByteString -> JarEntry
toStandaloneFileEntry path content =
  JarEntry
    { entryName = T.pack (takeFileName path)
    , entrySize = fromIntegral (BSL.length content)
    , entryContent = content
    , entryHash = digestToHex (hash (BSL.toStrict content))
    }

isDirectoryArchiveEntry :: Entry -> Bool
isDirectoryArchiveEntry e =
  let rel = eRelativePath e
  in null rel || "/" `isSuffixOf` rel || "\\" `isSuffixOf` rel

-------------------------------------------------------------------------------
-- Core: Diffing algorithm
-------------------------------------------------------------------------------

-- | Returns structural differences using default settings.
diffJars :: [JarEntry] -> [JarEntry] -> [JarDiff]
diffJars = diffJarsWith defaultDiffSettings

-- | Returns structural differences with explicit settings.
diffJarsWith :: DiffSettings -> [JarEntry] -> [JarEntry] -> [JarDiff]
diffJarsWith settings oldEntries newEntries =
  let oldMap = buildIndex oldEntries
      newMap = buildIndex newEntries

      addedRaw =
        [ JarDiff name Added Nothing (Just (entryHash ne)) Nothing (Just (entryContent ne))
        | (name, ne) <- Map.toList newMap
        , name `Map.notMember` oldMap
        ]

      removedRaw =
        [ JarDiff name Removed (Just (entryHash oe)) Nothing (Just (entryContent oe)) Nothing
        | (name, oe) <- Map.toList oldMap
        , name `Map.notMember` newMap
        ]

      modifiedRaw =
        [ JarDiff name Modified (Just (entryHash oe)) (Just (entryHash ne))
            (Just (entryContent oe)) (Just (entryContent ne))
        | (name, oe) <- Map.toList oldMap
        , Just ne <- [Map.lookup name newMap]
        , entriesDiffer settings name oe ne
        ]

      merged = addedRaw ++ removedRaw ++ modifiedRaw
  in sortOn diffEntry merged

entriesDiffer :: DiffSettings -> T.Text -> JarEntry -> JarEntry -> Bool
entriesDiffer settings name oldE newE
  | isClassEntry name && dsIgnoreClassSameSize settings && entrySize oldE == entrySize newE = False
  | dsUseCrcComparison settings = crc32Lazy (entryContent oldE) /= crc32Lazy (entryContent newE)
  | otherwise = entryHash oldE /= entryHash newE

buildIndex :: [JarEntry] -> Map.Map T.Text JarEntry
buildIndex = Map.fromList . map (\e -> (entryName e, e))

classGroupKey :: T.Text -> T.Text
classGroupKey name
  | not (isClassEntry name) = name
  | otherwise =
      let raw = T.unpack name
          (stem, ext) = splitExtension raw
          groupedStem = takeWhile (/= '$') stem
      in T.pack (groupedStem <> ext)

-- | Groups inner class diffs together under their outer class entry.
groupInnerClassDiffs :: [JarDiff] -> [JarDiff]
groupInnerClassDiffs diffs =
  let indexedDiffs = zip [0 :: Int ..] diffs
      grouped = foldl' step Map.empty indexedDiffs
      orderedGroups =
        sortOn
          (\(anchorIx, _, _) -> anchorIx)
          [ (groupAnchor acc, k, orderedGroupItems k (reverse (groupItemsRev acc)))
          | (k, acc) <- Map.toList grouped
          ]
  in [ toMerged (k, xs) | (_, k, xs) <- orderedGroups ]
  where
    step m (ix, d) =
      let k = classGroupKey (diffEntry d)
      in Map.alter (updateAcc ix d k) k m

    updateAcc ix d k Nothing =
      Just GroupAcc
        { groupItemsRev = [d]
        , groupFirstIx = ix
        , groupOuterIx = if diffEntry d == k then Just ix else Nothing
        }
    updateAcc ix d k (Just acc) =
      let outerIx' =
            case groupOuterIx acc of
              Just v -> Just v
              Nothing ->
                if diffEntry d == k
                  then Just ix
                  else Nothing
      in Just acc
          { groupItemsRev = d : groupItemsRev acc
          , groupOuterIx = outerIx'
          }

    orderedGroupItems k xs =
      let (outer, rest0) = partition (\d -> entryEqCI (diffEntry d) k) xs
          rest = sortOn classDepth rest0
      in outer ++ rest

    groupAnchor acc =
      case groupOuterIx acc of
        Just ix -> ix
        Nothing -> groupFirstIx acc

    toMerged (k, xs)
      | length xs <= 1 = head xs
      | otherwise =
          let oldChunks =
                [ "// " <> diffEntry d <> "\n" <> decodeLenient c <> "\n"
                | d <- xs
                , Just c <- [diffOldContent d]
                ]
              newChunks =
                [ "// " <> diffEntry d <> "\n" <> decodeLenient c <> "\n"
                | d <- xs
                , Just c <- [diffNewContent d]
                ]
              oldTxt = T.concat oldChunks
              newTxt = T.concat newChunks
              oldBs = if T.null oldTxt then Nothing else Just (BSL.fromStrict (TE.encodeUtf8 oldTxt))
              newBs = if T.null newTxt then Nothing else Just (BSL.fromStrict (TE.encodeUtf8 newTxt))
              dt
                | oldBs == Nothing && newBs /= Nothing = Added
                | oldBs /= Nothing && newBs == Nothing = Removed
                | otherwise = Modified
              oldHash = fmap (digestToHex . hash . BSL.toStrict) oldBs
              newHash = fmap (digestToHex . hash . BSL.toStrict) newBs
          in JarDiff
              { diffEntry = k
              , diffType = dt
              , diffOldHash = oldHash
              , diffNewHash = newHash
              , diffOldContent = oldBs
              , diffNewContent = newBs
              }

data GroupAcc = GroupAcc
  { groupItemsRev :: ![JarDiff]
  , groupFirstIx :: !Int
  , groupOuterIx :: !(Maybe Int)
  }

entryEqCI :: T.Text -> T.Text -> Bool
entryEqCI a b = T.toLower a == T.toLower b

classDepth :: JarDiff -> Int
classDepth d =
  T.length (T.filter (== '$') (stripClassExt (diffEntry d)))

crc32Lazy :: BSL.ByteString -> Word32
crc32Lazy bs = complement (BS.foldl' step 0xFFFFFFFF (BSL.toStrict bs))
  where
    poly :: Word32
    poly = 0xEDB88320

    step :: Word32 -> Word8 -> Word32
    step crc b = foldl' iter (crc `xor` fromIntegral b) [1 .. 8 :: Int]

    iter :: Word32 -> Int -> Word32
    iter c _ =
      if odd c
        then (c `shiftR` 1) `xor` poly
        else c `shiftR` 1
