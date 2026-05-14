{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : GlassJar.Types
-- Description : Core data types and JSON instances for GlassJar.
-- Copyright   : (c) Flechazo, 2026
-- License     : MIT
-- Maintainer  : 2558755403@qq.com
--
-- Defines core public data types for JAR entry diffs and report output
-- format selection.
module GlassJar.Types
  ( -- * Types
    JarEntry (..),
    DiffType (..),
    JarDiff (..),
    DiffLine (..),
    OutputFormat (..),
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteArray.Encoding as BAE (Base (..), convertToBase)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Represents an entry within a JAR archive, identified by its
-- relative path, uncompressed byte size, uncompressed raw content,
-- and a content hash.
data JarEntry = JarEntry
  { -- | Relative path of the entry inside the JAR.
    entryName :: !T.Text,
    -- | Size of the uncompressed content, in bytes.
    entrySize :: !Int,
    -- | Uncompressed raw content.
    entryContent :: !BSL.ByteString,
    -- | Hex-encoded content digest.
    entryHash :: !T.Text
  }
  deriving (Show, Eq, Ord)

-- | Classifies the kind of structural change for a single entry
-- between an old and a new JAR.
data DiffType
  = -- | The entry exists only in the new JAR.
    Added
  | -- | The entry exists only in the old JAR.
    Removed
  | -- | The entry exists in both JARs, but the content differs.
    Modified
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
  { -- | Entry path that differs.
    diffEntry :: !T.Text,
    -- | Kind of difference.
    diffType :: !DiffType,
    -- | Content hash from the old JAR, when applicable.
    diffOldHash :: !(Maybe T.Text),
    -- | Content hash from the new JAR, when applicable.
    diffNewHash :: !(Maybe T.Text),
    -- | Old content, for content diff.
    diffOldContent :: !(Maybe BSL.ByteString),
    -- | New content, for content diff.
    diffNewContent :: !(Maybe BSL.ByteString)
  }
  deriving (Show, Eq)

-- | Represents one line in a unified-diff hunk.
data DiffLine
  = -- | Unchanged context line.
    DiffKeep !T.Text
  | -- | Added line.
    DiffAdd !T.Text
  | -- | Deleted line.
    DiffDel !T.Text
  deriving (Show, Eq, Ord)

-- | Selects the report format for diff output.
data OutputFormat
  = -- | Unified-diff layout resembling @git diff@ output.
    FormatGitDiff
  | -- | Plain text suitable for terminal display.
    FormatText
  | -- | Single-line JSON document.
    FormatJson
  | -- | Self-contained HTML document.
    FormatHtml
  deriving (Show, Eq, Ord, Enum, Bounded)

-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance Aeson.ToJSON DiffType where
  toJSON Added = Aeson.String "added"
  toJSON Removed = Aeson.String "removed"
  toJSON Modified = Aeson.String "modified"

instance Aeson.ToJSON JarDiff where
  toJSON d =
    Aeson.object
      [ "entry" Aeson..= diffEntry d,
        "type" Aeson..= diffType d,
        "old_hash" Aeson..= diffOldHash d,
        "new_hash" Aeson..= diffNewHash d,
        "old_content" Aeson..= fmap contentToBase64 (diffOldContent d),
        "new_content" Aeson..= fmap contentToBase64 (diffNewContent d)
      ]

instance Aeson.ToJSON JarEntry where
  toJSON e =
    Aeson.object
      [ "name" Aeson..= entryName e,
        "size" Aeson..= entrySize e,
        "hash" Aeson..= entryHash e,
        "content" Aeson..= contentToBase64 (entryContent e)
      ]

-- Encodes lazy ByteString as a base64-encoded JSON String.
contentToBase64 :: BSL.ByteString -> Aeson.Value
contentToBase64 = Aeson.String . TE.decodeUtf8 . BAE.convertToBase BAE.Base64 . BSL.toStrict
