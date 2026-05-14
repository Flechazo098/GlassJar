{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : GlassJar.Report
-- Description : Report formatters for JAR diff results.
-- Copyright   : (c) Flechazo, 2026
-- License     : MIT
-- Maintainer  : 2558755403@qq.com
--
-- Provides plain-text, git-diff, and JSON formatters for structural
-- diff results.
module GlassJar.Report
  ( formatReport,
    formatReportGitDiff,
    formatReportJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GlassJar.Diff (lineDiff)
import GlassJar.Types (DiffLine (..), DiffType (..), JarDiff (..))
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (..),
    ConsoleLayer (..),
    SGR (..),
    setSGRCode,
  )

-------------------------------------------------------------------------------
-- Plain-text format
-------------------------------------------------------------------------------

-- | Returns a plain-text report for structural differences.
formatReport :: [JarDiff] -> T.Text
formatReport [] = "No differences found. The two inputs are identical.\n"
formatReport diffs =
  let summary = formatSummary diffs
      details = T.unlines $ map formatDiffLine diffs
   in summary <> "\n" <> details

countDiffs :: [JarDiff] -> (Int, Int, Int)
countDiffs =
  foldl'
    ( \(a, r, m) d -> case diffType d of
        Added -> (a + 1, r, m)
        Removed -> (a, r + 1, m)
        Modified -> (a, r, m + 1)
    )
    (0, 0, 0)

formatSummary :: [JarDiff] -> T.Text
formatSummary diffs =
  let (a, r, m) = countDiffs diffs
      total = a + r + m
   in T.unlines
        [ replicateStr 45 "=",
          "  GlassJar Diff Report",
          replicateStr 45 "=",
          "  Total differences: " <> T.pack (show total),
          "    Added:    " <> T.pack (show a),
          "    Removed:  " <> T.pack (show r),
          "    Modified: " <> T.pack (show m),
          replicateStr 45 "="
        ]

formatDiffLine :: JarDiff -> T.Text
formatDiffLine d =
  let (icon, label) = case diffType d of
        Added -> ("+", "ADDED" :: T.Text)
        Removed -> ("-", "REMOVED")
        Modified -> ("~", "MODIFIED")
      line = "  " <> icon <> " [" <> label <> "] " <> diffEntry d
   in case diffType d of
        Modified -> line <> "\n" <> formatContentDiff (diffOldContent d) (diffNewContent d)
        _ -> line

formatContentDiff :: Maybe BSL.ByteString -> Maybe BSL.ByteString -> T.Text
formatContentDiff (Just old) (Just new) =
  if isUtf8 old && isUtf8 new
    then T.unlines $ map renderDiffLine $ lineDiff 3 old new
    else "    [binary content differs; non-UTF-8]\n"
formatContentDiff _ _ = ""

renderDiffLine :: DiffLine -> T.Text
renderDiffLine (DiffKeep l) = "    " <> l
renderDiffLine (DiffAdd l) = "  + " <> l
renderDiffLine (DiffDel l) = "  - " <> l

replicateStr :: Int -> T.Text -> T.Text
replicateStr = T.replicate

-------------------------------------------------------------------------------
-- JSON format
-------------------------------------------------------------------------------

-- | Returns a single-line JSON object for diff results.
formatReportJson :: [JarDiff] -> T.Text
formatReportJson diffs =
  let (a, r, m) = countDiffs diffs
      jsonValue =
        Aeson.object
          [ "summary"
              Aeson..= Aeson.object
                [ "added" Aeson..= a,
                  "removed" Aeson..= r,
                  "modified" Aeson..= m,
                  "total" Aeson..= (a + r + m :: Int)
                ],
            "differences" Aeson..= diffs
          ]
      lbs = Aeson.encode jsonValue
   in TE.decodeUtf8 $ BSL.toStrict lbs

-------------------------------------------------------------------------------
-- Git-diff format
-------------------------------------------------------------------------------

-- | Returns a unified-diff-style report for structural differences.
formatReportGitDiff :: [JarDiff] -> T.Text
formatReportGitDiff [] = "No differences found. The two inputs are identical.\n"
formatReportGitDiff diffs =
  let (a, r, m) = countDiffs diffs
      total = a + r + m

      header =
        T.unlines
          [ replicateStr 45 "=",
            "  GlassJar Diff Report",
            replicateStr 45 "=",
            "  Total differences: " <> T.pack (show total),
            "    Added:    " <> greenCode <> T.pack (show a) <> resetCode,
            "    Removed:  " <> redCode <> T.pack (show r) <> resetCode,
            "    Modified: " <> yellowCode <> T.pack (show m) <> resetCode,
            replicateStr 45 "="
          ]
      diffHeader =
        T.unlines
          [ "--- a/old",
            "+++ b/new",
            "@@ entries @@"
          ]
      details = T.unlines $ map formatGitDiffLine diffs
   in header <> "\n" <> diffHeader <> "\n" <> details

formatGitDiffLine :: JarDiff -> T.Text
formatGitDiffLine d = case diffType d of
  Added -> greenCode <> "  + " <> diffEntry d <> resetCode
  Removed -> redCode <> "  - " <> diffEntry d <> resetCode
  Modified ->
    let body = case (diffOldContent d, diffNewContent d) of
          (Just old, Just new) ->
            T.unlines
              [ yellowCode <> "  ~ " <> diffEntry d <> resetCode,
                yellowCode <> "--- a/" <> diffEntry d <> resetCode,
                yellowCode <> "+++ b/" <> diffEntry d <> resetCode
              ]
              <> formatColorDiff old new
          _ -> yellowCode <> "  ~ " <> diffEntry d <> resetCode
     in body

formatColorDiff :: BSL.ByteString -> BSL.ByteString -> T.Text
formatColorDiff old new =
  if isUtf8 old && isUtf8 new
    then T.unlines $ map renderColorDiffLine $ lineDiff 3 old new
    else yellowCode <> "@@ binary content differs (non-UTF-8) @@" <> resetCode

renderColorDiffLine :: DiffLine -> T.Text
renderColorDiffLine (DiffKeep l) = " " <> l
renderColorDiffLine (DiffAdd l) = greenCode <> "+" <> l <> resetCode
renderColorDiffLine (DiffDel l) = redCode <> "-" <> l <> resetCode

greenCode, redCode, yellowCode, resetCode :: T.Text
greenCode = T.pack $ setSGRCode [SetColor Foreground Vivid Green]
redCode = T.pack $ setSGRCode [SetColor Foreground Vivid Red]
yellowCode = T.pack $ setSGRCode [SetColor Foreground Vivid Yellow]
resetCode = T.pack $ setSGRCode [Reset]

-- Returns True when content is valid UTF-8.
isUtf8 :: BSL.ByteString -> Bool
isUtf8 = either (const False) (const True) . TE.decodeUtf8' . BSL.toStrict
