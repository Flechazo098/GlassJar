{-|
Module      : GlassJar.Internal
Description : Shared utility functions for GlassJar modules.
Copyright   : (c) Flechazo, 2026
License     : MIT
Maintainer  : 2558755403@qq.com
-}

{-# LANGUAGE OverloadedStrings #-}

module GlassJar.Internal
  ( digestToHex
  , digestToHexByteString
  , isClassEntry
  , stripClassExt
  , decodeLenient
  , toEntryPath
  ) where

import Crypto.Hash (Digest, MD5)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.FilePath (normalise)

-- | Normalizes a file path to use forward slashes.
toEntryPath :: FilePath -> FilePath
toEntryPath = map slash . normalise
  where
    slash '\\' = '/'
    slash c = c

-- | Encodes a 'Digest' as a lowercase hexadecimal 'T.Text'.
digestToHex :: Digest MD5 -> T.Text
digestToHex = TE.decodeUtf8 . digestToHexByteString

-- | Encodes a 'Digest' as a hex 'BS.ByteString'.
digestToHexByteString :: Digest a -> BS.ByteString
digestToHexByteString = convertToBase Base16

-- | Returns 'True' when the entry name refers to a Java class file.
isClassEntry :: T.Text -> Bool
isClassEntry t = ".class" `T.isSuffixOf` T.toLower t

-- | Strips the @.class@ suffix from an entry name, if present.
stripClassExt :: T.Text -> T.Text
stripClassExt name =
  if ".class" `T.isSuffixOf` T.toLower name
    then T.dropEnd 6 name
    else name

-- | Decodes a lazy 'BSL.ByteString' as UTF-8, replacing invalid bytes
-- with the Unicode replacement character.
decodeLenient :: BSL.ByteString -> T.Text
decodeLenient = TE.decodeUtf8With lenientDecode . BSL.toStrict
