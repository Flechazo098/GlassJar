{-|
Module      : GlassJar.Types
Description : Core data types and JSON instances for GlassJar.
Copyright   : (c) Flechazo, 2026
License     : MIT
Maintainer  : 2558755403@qq.com

Defines core public data types for JAR entry diffs and report output
format selection.
-}

{-# LANGUAGE OverloadedStrings #-}

module GlassJar.Types
  ( -- * Types
    JarEntry (..)
  , DiffType (..)
  , JarDiff (..)
  , DiffLine (..)
  , OutputFormat (..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteArray.Encoding as BAE (Base (..), convertToBase)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Represents an entry within a JAR archive, identified by its
-- relative path, uncompressed byte size, uncompressed raw content,
-- and a content hash.
data JarEntry = JarEntry
  { entryName    :: !T.Text          -- ^ Relative path of the entry inside the JAR.
  , entrySize    :: !Int             -- ^ Size of the uncompressed content, in bytes.
  , entryContent :: !BSL.ByteString  -- ^ Uncompressed raw content.
  , entryHash    :: !T.Text          -- ^ Hex-encoded content digest.
  } deriving (Show, Eq, Ord)

-- | Classifies the kind of structural change for a single entry
-- between an old and a new JAR.
data DiffType
  = Added      -- ^ The entry exists only in the new JAR.
  | Removed    -- ^ The entry exists only in the old JAR.
  | Modified   -- ^ The entry exists in both JARs, but the content differs.
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Records a single structural difference between two JAR files.
--
-- @diffOldHash@ is 'Nothing' for 'Added' entries.
--
-- @diffNewHash@ is 'Nothing' for 'Removed' entries.
--
-- Both hash fields are 'Just' for 'Modified' entries.
--
-- @diffOldContent@ and @diffNewContent@ carry the uncompressed raw
-- content for 'Modified' entries, otherwise 'Nothing'.
data JarDiff = JarDiff
  { diffEntry      :: !T.Text              -- ^ Entry path that differs.
  , diffType       :: !DiffType            -- ^ Kind of difference.
  , diffOldHash    :: !(Maybe T.Text)      -- ^ Content hash from the old JAR, when applicable.
  , diffNewHash    :: !(Maybe T.Text)      -- ^ Content hash from the new JAR, when applicable.
  , diffOldContent :: !(Maybe BSL.ByteString)  -- ^ Old content, for content diff.
  , diffNewContent :: !(Maybe BSL.ByteString)  -- ^ New content, for content diff.
  } deriving (Show, Eq)

-- | Represents one line in a unified-diff hunk.
data DiffLine
  = DiffKeep !T.Text  -- ^ Unchanged context line.
  | DiffAdd  !T.Text  -- ^ Added line.
  | DiffDel  !T.Text  -- ^ Deleted line.
  deriving (Show, Eq, Ord)

-- | Selects the report format for diff output.
data OutputFormat
  = FormatGitDiff  -- ^ Unified-diff layout resembling @git diff@ output.
  | FormatText     -- ^ Plain text suitable for terminal display.
  | FormatJson     -- ^ Single-line JSON document.
  | FormatHtml     -- ^ Self-contained HTML document.
  deriving (Show, Eq, Ord, Enum, Bounded)

-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance Aeson.ToJSON DiffType where
  toJSON Added    = Aeson.String "added"
  toJSON Removed  = Aeson.String "removed"
  toJSON Modified = Aeson.String "modified"

instance Aeson.ToJSON JarDiff where
  toJSON d = Aeson.object
    [ "entry"       Aeson..= diffEntry d
    , "type"        Aeson..= diffType d
    , "old_hash"    Aeson..= diffOldHash d
    , "new_hash"    Aeson..= diffNewHash d
    , "old_content" Aeson..= fmap contentToBase64 (diffOldContent d)
    , "new_content" Aeson..= fmap contentToBase64 (diffNewContent d)
    ]

instance Aeson.ToJSON JarEntry where
  toJSON e = Aeson.object
    [ "name"    Aeson..= entryName e
    , "size"    Aeson..= entrySize e
    , "hash"    Aeson..= entryHash e
    , "content" Aeson..= contentToBase64 (entryContent e)
    ]

-- Encodes lazy ByteString as a base64-encoded JSON String.
contentToBase64 :: BSL.ByteString -> Aeson.Value
contentToBase64 = Aeson.String . TE.decodeUtf8 . BAE.convertToBase BAE.Base64 . BSL.toStrict

