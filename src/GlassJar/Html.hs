{-|
Module      : GlassJar.Html
Description : HTML diff report generation for GlassJar.
Copyright   : (c) Flechazo, 2026
License     : MIT
Maintainer  : 2558755403@qq.com

Defines behavior for rendering a self-contained HTML5 diff report
from a list of 'JarDiff' values.
-}

{-# LANGUAGE OverloadedStrings #-}

module GlassJar.Html
  ( formatReportHtml
  ) where

import GlassJar.Diff (lineDiff)
import GlassJar.Types (DiffLine (..), DiffType (..), JarDiff (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isPrint)
import Data.List (foldl', nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

-------------------------------------------------------------------------------
-- Top-level HTML report
-------------------------------------------------------------------------------

-- | Returns a complete HTML5 report document for differences.
--
-- Returns a valid document when the input list is empty.
formatReportHtml :: [JarDiff] -> T.Text
formatReportHtml diffs =
  let (added, removed, modified, _, rowsBuilder) =
        foldl' accum (0, 0, 0, 0, mempty) diffs
      total = added + removed + modified

      htmlBuilder =
        B.fromText "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n" <>
        B.fromText "  <meta charset=\"UTF-8\">\n" <>
        B.fromText "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" <>
        B.fromText "  <title>GlassJar Diff Report</title>\n" <>
        B.fromText "  <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">\n" <>
        B.fromText "  <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>\n" <>
        B.fromText "  <link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap\" rel=\"stylesheet\">\n" <>
        B.fromText "  <link id=\"hljs-theme-dark\" rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github-dark-dimmed.min.css\">\n" <>
        B.fromText "  <link id=\"hljs-theme-light\" rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css\" disabled>\n" <>
        B.fromText "  <style>\n" <>
        htmlThemeCss <>
        B.fromText "  </style>\n" <>
        B.fromText "</head>\n<body class=\"view-split\">\n" <>
        B.fromText "  <main class=\"page\">\n" <>
        B.fromText "  <h1>GlassJar Diff Report</h1>\n" <>
        B.fromText "  <div class=\"toolbar\">\n" <>
        B.fromText "    <button id=\"theme-toggle\" class=\"btn\" title=\"切换深色或浅色主题\">切换浅色</button>\n" <>
        B.fromText "    <button id=\"hash-toggle\" class=\"btn\" title=\"显示或隐藏哈希列\">隐藏哈希</button>\n" <>
        B.fromText "    <button id=\"view-toggle\" class=\"btn\" title=\"切换统一/分栏视图\">切换为统一视图</button>\n" <>
        B.fromText "    <button id=\"open-toggle\" class=\"btn\" title=\"展开或收起所有差异详情\">全部展开</button>\n" <>
        B.fromText "  </div>\n" <>
        htmlSummary added removed modified total <>
        B.fromText "  <div class=\"entry-list\">\n" <>
        rowsBuilder <>
        B.fromText "  </div>\n" <>
        B.fromText "  <script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js\"></script>\n" <>
        B.fromText "  <script>\n" <>
        htmlThemeJs <>
        B.fromText "  </script>\n" <>
        B.fromText "  </main>\n" <>
        B.fromText "</body>\n</html>\n"
  in TL.toStrict $ B.toLazyText htmlBuilder

-------------------------------------------------------------------------------
-- Internal helpers
-------------------------------------------------------------------------------

-- Represents one side-by-side diff row.
data SplitRow = SplitRow
  { rowOldNo    :: !(Maybe Int)
  , rowOldText  :: !(Maybe T.Text)
  , rowOldClass :: !T.Text
  , rowNewNo    :: !(Maybe Int)
  , rowNewText  :: !(Maybe T.Text)
  , rowNewClass :: !T.Text
  }

-- Aggregates per-type counts and appends the HTML row for a diff.
accum :: (Int, Int, Int, Int, B.Builder) -> JarDiff -> (Int, Int, Int, Int, B.Builder)
accum (a, r, m, i, b) d =
  case diffType d of
    Added    -> (a + 1, r,     m,     i + 1, b <> htmlDiffCard i d)
    Removed  -> (a,     r + 1, m,     i + 1, b <> htmlDiffCard i d)
    Modified -> (a,     r,     m + 1, i + 1, b <> htmlDiffCard i d)

-- Builds the @\<div class=\"summary\"\>@ block.
htmlSummary :: Int -> Int -> Int -> Int -> B.Builder
htmlSummary added removed modified total =
  let card cls count label =
        B.fromText "    <div class=\"summary-card " <> B.fromText cls <> B.fromText "\">\n" <>
        B.fromText "      <div class=\"count\">" <> B.fromText (T.pack $ show count) <> B.fromText "</div>\n" <>
        B.fromText "      <div class=\"label\">" <> B.fromText label <> B.fromText "</div>\n" <>
        B.fromText "    </div>\n"
  in B.fromText "  <div class=\"summary\">\n" <>
     card "added"    added    "Added" <>
     card "removed"  removed  "Removed" <>
     card "modified" modified "Modified" <>
     card "total"    total    "Total" <>
     B.fromText "  </div>\n"

-- Builds one diff entry card.
htmlDiffCard :: Int -> JarDiff -> B.Builder
htmlDiffCard i d =
  let (cssClass, badgeClass, badgeLabel) = case diffType d of
        Added    -> ("added"    :: T.Text, "badge-added",    "ADDED"    :: T.Text)
        Removed  -> ("removed",             "badge-removed",  "REMOVED"  )
        Modified -> ("modified",            "badge-modified", "MODIFIED" )
      escapedName = htmlEscape (diffEntry d)
      entryId = T.pack ("entry-" <> show i)
      oldHash = fromMaybe "N/A" (diffOldHash d)
      newHash = fromMaybe "N/A" (diffNewHash d)
      (addCount, delCount) = detailChangeCounts d
  in B.fromText "    <section id=\"" <> B.fromText entryId <> B.fromText "\" class=\"entry " <> B.fromText cssClass <> B.fromText "\">\n" <>
     B.fromText "      <div class=\"entry-head\">\n" <>
     B.fromText "        <div class=\"head-left\"><span class=\"badge " <> B.fromText badgeClass <> B.fromText "\">" <>
     B.fromText badgeLabel <> B.fromText "</span><a class=\"entry-path entry-path-link\" href=\"?focus=" <> B.fromText entryId <> B.fromText "\" target=\"_blank\" rel=\"noopener\">" <> B.fromText escapedName <> B.fromText "</a></div>\n" <>
     B.fromText "        <div class=\"head-right\">\n" <>
     renderChangeStats addCount delCount <>
     B.fromText "          <span class=\"hash-pill hash-old\">old " <> B.fromText oldHash <> B.fromText "</span>\n" <>
     B.fromText "          <span class=\"hash-pill hash-new\">new " <> B.fromText newHash <> B.fromText "</span>\n" <>
     B.fromText "        </div>\n" <>
     B.fromText "      </div>\n" <>
     (if hasDetailContent d then htmlDiffDetailsBlock d else mempty) <>
     B.fromText "    </section>\n"

-- Returns True when a diff entry can render detail content.
hasDetailContent :: JarDiff -> Bool
hasDetailContent d = case diffType d of
  Added    -> maybe False (const True) (diffNewContent d)
  Removed  -> maybe False (const True) (diffOldContent d)
  Modified -> maybe False (const True) (diffOldContent d)
           && maybe False (const True) (diffNewContent d)

-- Builds details block for one diff entry.
htmlDiffDetailsBlock :: JarDiff -> B.Builder
htmlDiffDetailsBlock d =
  let detailsBody = renderDetailBody d
  in B.fromText "      <details class=\"entry-details\">" <>
     B.fromText "<summary>查看差异详情</summary>" <>
     B.fromText "<div class=\"anim-wrapper\"><div class=\"anim-content\">" <>
     detailsBody <>
     B.fromText "</div></div></details>\n"

-- Renders the change-stat chips.
renderChangeStats :: Int -> Int -> B.Builder
renderChangeStats addCount delCount =
  B.fromText "          <span class=\"diff-stats\">" <>
     B.fromText "<span class=\"stat-add\">+" <> B.fromText (T.pack (show addCount)) <> B.fromText "</span>" <>
     B.fromText "<span class=\"stat-del\">-" <> B.fromText (T.pack (show delCount)) <> B.fromText "</span>" <>
     renderStatSquares addCount delCount <>
     B.fromText "</span>\n"

-- Renders GitHub-style change-stat squares.
renderStatSquares :: Int -> Int -> B.Builder
renderStatSquares addCount delCount =
  let total = addCount + delCount
      cellCount = 5 :: Int
      rawAdd = if total == 0 then 0 else (addCount * cellCount + total `div` 2) `div` total
      addCells = max 0 (min cellCount rawAdd)
      delCells = cellCount - addCells
      addBuilder = foldl' (<>) mempty (replicate addCells (B.fromText "<span class=\"sq filled-add\"></span>"))
      delBuilder = foldl' (<>) mempty (replicate delCells (B.fromText "<span class=\"sq filled-del\"></span>"))
      neutralBuilder = foldl' (<>) mempty (replicate cellCount (B.fromText "<span class=\"sq\"></span>"))
      cells
        | total == 0 = neutralBuilder
        | otherwise = addBuilder <> delBuilder <> foldl' (<>) mempty (replicate (cellCount - addCells - delCells) (B.fromText "<span class=\"sq\"></span>"))
  in B.fromText "<span class=\"squares-map\" aria-hidden=\"true\">" <>
     cells <>
     B.fromText "</span>"

-- Returns change counts for detail views.
detailChangeCounts :: JarDiff -> (Int, Int)
detailChangeCounts d = case diffType d of
  Added ->
    let n = maybe 0 (countLogicalLines . decodeDetailText d True) (diffNewContent d)
    in (n, 0)
  Removed ->
    let n = maybe 0 (countLogicalLines . decodeDetailText d False) (diffOldContent d)
    in (0, n)
  Modified ->
    case (diffOldContent d, diffNewContent d) of
      (Just old, Just new) ->
        let oldTxt = decodeDetailTextFromBytes d old
            newTxt = decodeDetailTextFromBytes d new
            ds = lineDiff 5 (BSL.fromStrict (TE.encodeUtf8 oldTxt)) (BSL.fromStrict (TE.encodeUtf8 newTxt))
            addN = length [() | DiffAdd _ <- ds]
            delN = length [() | DiffDel _ <- ds]
        in (addN, delN)
      _ -> (0, 0)

-- Decodes added/removed detail text.
decodeDetailText :: JarDiff -> Bool -> BSL.ByteString -> T.Text
decodeDetailText d _isAdded bs = decodeDetailTextFromBytes d bs

-- Decodes bytes to detail text according to entry kind.
decodeDetailTextFromBytes :: JarDiff -> BSL.ByteString -> T.Text
decodeDetailTextFromBytes d bs
  | isUtf8Content bs = TE.decodeUtf8With lenientDecode (BSL.toStrict bs)
  | entryLooksClass d = classSymbolView bs
  | otherwise = ""

-- Counts logical lines in a text block.
countLogicalLines :: T.Text -> Int
countLogicalLines t =
  let ls = T.lines t
  in if null ls then 0 else length ls

-- Renders detail body according to diff type.
renderDetailBody :: JarDiff -> B.Builder
renderDetailBody d = case diffType d of
  Modified ->
    case (diffOldContent d, diffNewContent d) of
      (Just old, Just new) ->
        if isUtf8Content old && isUtf8Content new
          then renderHtmlContentDiff old new
          else if entryLooksClass d
            then renderHtmlTextDiff (classSymbolView old) (classSymbolView new)
            else B.fromText "<div class=\"binary-note\">Binary content differs (non-UTF-8).</div>"
      _ -> B.fromText "<div class=\"binary-note\">Content payload is unavailable.</div>"
  Added ->
    case diffNewContent d of
      Just new ->
        if isUtf8Content new
          then renderAddedRemovedTextView True (TE.decodeUtf8With lenientDecode (BSL.toStrict new))
          else if entryLooksClass d
            then renderAddedRemovedTextView True (classSymbolView new)
            else B.fromText "<div class=\"binary-note\">Binary content added (non-UTF-8).</div>"
      _ -> B.fromText "<div class=\"binary-note\">Content payload is unavailable.</div>"
  Removed ->
    case diffOldContent d of
      Just old ->
        if isUtf8Content old
          then renderAddedRemovedTextView False (TE.decodeUtf8With lenientDecode (BSL.toStrict old))
          else if entryLooksClass d
            then renderAddedRemovedTextView False (classSymbolView old)
            else B.fromText "<div class=\"binary-note\">Binary content removed (non-UTF-8).</div>"
      _ -> B.fromText "<div class=\"binary-note\">Content payload is unavailable.</div>"

-- Renders line-level diff lines as a split view.
renderHtmlContentDiff :: BSL.ByteString -> BSL.ByteString -> B.Builder
renderHtmlContentDiff old new =
  renderHtmlTextDiff
    (TE.decodeUtf8With lenientDecode (BSL.toStrict old))
    (TE.decodeUtf8With lenientDecode (BSL.toStrict new))

-- Renders line-level diff for two text values.
renderHtmlTextDiff :: T.Text -> T.Text -> B.Builder
renderHtmlTextDiff oldText newText =
  let oldBs = BSL.fromStrict (TE.encodeUtf8 oldText)
      newBs = BSL.fromStrict (TE.encodeUtf8 newText)
      rows = buildSplitRows (lineDiff 5 oldBs newBs)
      unifiedRows = buildUnifiedRows (lineDiff 5 oldBs newBs)
  in B.fromText "<div class=\"code-pane view-split-el\"><div class=\"diff-grid view-split\">" <>
     B.fromText "<div class=\"grid-head\"></div><div class=\"grid-head\">Old</div><div class=\"grid-head\"></div><div class=\"grid-head\">New</div>" <>
     foldl' (<>) mempty (map renderSplitRow rows) <>
     B.fromText "</div></div>" <>
     B.fromText "<div class=\"code-pane view-unified-el\"><div class=\"diff-grid view-unified\">" <>
     B.fromText "<div class=\"grid-head\">Line</div><div class=\"grid-head\">Diff</div>" <>
     foldl' (<>) mempty (map renderUnifiedRow unifiedRows) <>
     B.fromText "</div></div>"

-- Renders one-sided added or removed text.
renderAddedRemovedTextView :: Bool -> T.Text -> B.Builder
renderAddedRemovedTextView isAdded txt =
  let sideText = if isAdded then "Added content" else "Removed content"
      cls = if isAdded then "add" else "del"
      numbered = zip [1 :: Int ..] (T.lines txt)
  in B.fromText "<div class=\"code-pane one-side-wrapper\"><div class=\"one-side-title\">" <>
     B.fromText sideText <> B.fromText "</div><div class=\"one-side-grid\">" <>
     foldl' (<>) mempty (map (renderOneSideRow cls) numbered) <>
     B.fromText "</div></div>"

-- Renders one row in one-sided view.
renderOneSideRow :: T.Text -> (Int, T.Text) -> B.Builder
renderOneSideRow cls (n, t) =
  let cellCls = diffCellClass cls
  in B.fromText "<div class=\"row one-side-row\">" <>
  B.fromText "<div class=\"ln " <> B.fromText cellCls <> B.fromText "\">" <>
  B.fromText (T.pack (show n)) <> B.fromText "</div>" <>
  B.fromText "<div class=\"code " <> B.fromText cellCls <> B.fromText "\">" <>
  B.fromText (htmlEscape t) <> B.fromText "</div>" <>
  B.fromText "</div>"

-- Builds split rows from unified diff lines with line numbers.
buildSplitRows :: [DiffLine] -> [SplitRow]
buildSplitRows = go 1 1
  where
    go _ _ [] = []
    go oldNo newNo (DiffKeep t : rest) =
      SplitRow (Just oldNo) (Just t) "ctx" (Just newNo) (Just t) "ctx"
      : go (oldNo + 1) (newNo + 1) rest
    go oldNo newNo rest =
      let (changeBlock, tailBlock) = span isChangeLine rest
          dels = [t | DiffDel t <- changeBlock]
          adds = [t | DiffAdd t <- changeBlock]
          (rows, oldNo', newNo') = pairChangeBlock oldNo newNo dels adds
      in rows ++ go oldNo' newNo' tailBlock

    isChangeLine (DiffKeep _) = False
    isChangeLine _ = True

pairChangeBlock :: Int -> Int -> [T.Text] -> [T.Text] -> ([SplitRow], Int, Int)
pairChangeBlock oldNo newNo dels adds = go oldNo newNo dels adds []
  where
    go oldIdx newIdx [] [] acc = (reverse acc, oldIdx, newIdx)
    go oldIdx newIdx (d : ds) (a : as) acc =
      go
        (oldIdx + 1)
        (newIdx + 1)
        ds
        as
        (SplitRow (Just oldIdx) (Just d) "del" (Just newIdx) (Just a) "add" : acc)
    go oldIdx newIdx (d : ds) [] acc =
      go
        (oldIdx + 1)
        newIdx
        ds
        []
        (SplitRow (Just oldIdx) (Just d) "del" Nothing Nothing "empty" : acc)
    go oldIdx newIdx [] (a : as) acc =
      go
        oldIdx
        (newIdx + 1)
        []
        as
        (SplitRow Nothing Nothing "empty" (Just newIdx) (Just a) "add" : acc)

-- Builds rows for unified mode.
buildUnifiedRows :: [DiffLine] -> [(Maybe Int, T.Text, T.Text)]
buildUnifiedRows = go 1 1
  where
    go _ _ [] = []
    go oldNo newNo (DiffKeep t : rest) =
      (Just oldNo, "ctx", "  " <> t) : go (oldNo + 1) (newNo + 1) rest
    go oldNo newNo (DiffDel t : rest) =
      (Just oldNo, "del", "- " <> t) : go (oldNo + 1) newNo rest
    go oldNo newNo (DiffAdd t : rest) =
      (Just newNo, "add", "+ " <> t) : go oldNo (newNo + 1) rest

-- Renders one split row.
renderSplitRow :: SplitRow -> B.Builder
renderSplitRow r =
  B.fromText "<div class=\"row split-row\">" <>
  B.fromText "<div class=\"ln " <> B.fromText (diffCellClass (rowOldClass r)) <> B.fromText "\">" <>
  B.fromText (renderLineNo (rowOldNo r)) <> B.fromText "</div>" <>
  B.fromText "<div class=\"code " <> B.fromText (diffCellClass (rowOldClass r)) <> B.fromText "\">" <>
  B.fromText (renderCodeText (rowOldText r)) <> B.fromText "</div>" <>
  B.fromText "<div class=\"ln " <> B.fromText (diffCellClass (rowNewClass r)) <> B.fromText "\">" <>
  B.fromText (renderLineNo (rowNewNo r)) <> B.fromText "</div>" <>
  B.fromText "<div class=\"code " <> B.fromText (diffCellClass (rowNewClass r)) <> B.fromText "\">" <>
  B.fromText (renderCodeText (rowNewText r)) <> B.fromText "</div>" <>
  B.fromText "</div>"

-- Renders one unified row.
renderUnifiedRow :: (Maybe Int, T.Text, T.Text) -> B.Builder
renderUnifiedRow (mNo, cls, txt) =
  let cellCls = diffCellClass cls
  in B.fromText "<div class=\"row unified-row\">" <>
  B.fromText "<div class=\"ln " <> B.fromText cellCls <> B.fromText "\">" <>
  B.fromText (renderLineNo mNo) <> B.fromText "</div>" <>
  B.fromText "<div class=\"code " <> B.fromText cellCls <> B.fromText "\">" <>
  B.fromText (htmlEscape txt) <> B.fromText "</div>" <>
  B.fromText "</div>"

-- Maps logical row types to visual cell classes.
diffCellClass :: T.Text -> T.Text
diffCellClass cls
  | cls == "add" = "cell-add"
  | cls == "del" = "cell-del"
  | cls == "empty" = "cell-empty"
  | otherwise = ""

-- Renders a line number cell.
renderLineNo :: Maybe Int -> T.Text
renderLineNo Nothing  = ""
renderLineNo (Just n) = T.pack (show n)

-- Renders a code cell.
renderCodeText :: Maybe T.Text -> T.Text
renderCodeText Nothing  = ""
renderCodeText (Just t) = htmlEscape t

-- Returns True when entry path indicates a Java class file.
entryLooksClass :: JarDiff -> Bool
entryLooksClass d = ".class" `T.isSuffixOf` diffEntry d

-- Returns a symbol-oriented class view from binary class bytes.
classSymbolView :: BSL.ByteString -> T.Text
classSymbolView bs =
  let strs = take 220 (extractClassUtf8Strings (BSL.toStrict bs))
      scored = sortOn (Down . T.length) strs
      body = if null scored then ["// no UTF-8 symbols found"] else map ("  " <>) scored
  in T.unlines $
    [ "// class symbol view (constant-pool strings)"
    , "// source is non-UTF-8 binary; this is a fallback view"
    ] ++ body

-- Extracts UTF-8 constant strings from a class file constant pool.
extractClassUtf8Strings :: BS.ByteString -> [T.Text]
extractClassUtf8Strings bs =
  case parseClassPool bs of
    Nothing -> []
    Just xs ->
      let cleaned = map cleanToken xs
      in nub $ filter validToken cleaned
  where
    cleanToken = T.filter (\c -> isPrint c || c == '\t')
    validToken t = T.length t >= 3
                && T.length t <= 220
                && T.any (\c -> c /= '\0' && c /= '\r') t

-- Parses CONSTANT_Utf8 entries in a class file.
parseClassPool :: BS.ByteString -> Maybe [T.Text]
parseClassPool bs = do
  magic <- readU4 bs 0
  if magic /= 0xCAFEBABE then Nothing else do
    cpCount <- readU2 bs 8
    let start = 10
    snd <$> go start 1 cpCount []
  where
    go off i cpCount acc
      | i >= cpCount = Just (off, reverse acc)
      | otherwise = do
          tag <- readU1 bs off
          case tag of
            1 -> do
              len <- readU2 bs (off + 1)
              strBytes <- slice bs (off + 3) len
              let t = TE.decodeUtf8With lenientDecode strBytes
              go (off + 3 + len) (i + 1) cpCount (t : acc)
            3  -> go (off + 5)  (i + 1) cpCount acc
            4  -> go (off + 5)  (i + 1) cpCount acc
            5  -> go (off + 9)  (i + 2) cpCount acc
            6  -> go (off + 9)  (i + 2) cpCount acc
            7  -> go (off + 3)  (i + 1) cpCount acc
            8  -> go (off + 3)  (i + 1) cpCount acc
            9  -> go (off + 5)  (i + 1) cpCount acc
            10 -> go (off + 5)  (i + 1) cpCount acc
            11 -> go (off + 5)  (i + 1) cpCount acc
            12 -> go (off + 5)  (i + 1) cpCount acc
            15 -> go (off + 4)  (i + 1) cpCount acc
            16 -> go (off + 3)  (i + 1) cpCount acc
            17 -> go (off + 5)  (i + 1) cpCount acc
            18 -> go (off + 5)  (i + 1) cpCount acc
            19 -> go (off + 3)  (i + 1) cpCount acc
            20 -> go (off + 3)  (i + 1) cpCount acc
            _  -> Nothing

-- Reads one unsigned byte.
readU1 :: BS.ByteString -> Int -> Maybe Int
readU1 bs off
  | off < 0 || off >= BS.length bs = Nothing
  | otherwise = Just (fromIntegral (BS.index bs off))

-- Reads one unsigned 16-bit value.
readU2 :: BS.ByteString -> Int -> Maybe Int
readU2 bs off = do
  b1 <- readU1 bs off
  b2 <- readU1 bs (off + 1)
  pure (b1 * 256 + b2)

-- Reads one unsigned 32-bit value.
readU4 :: BS.ByteString -> Int -> Maybe Int
readU4 bs off = do
  b1 <- readU1 bs off
  b2 <- readU1 bs (off + 1)
  b3 <- readU1 bs (off + 2)
  b4 <- readU1 bs (off + 3)
  pure (((b1 * 256 + b2) * 256 + b3) * 256 + b4)

-- Slices bytes with bounds checking.
slice :: BS.ByteString -> Int -> Int -> Maybe BS.ByteString
slice bs off len
  | off < 0 || len < 0 = Nothing
  | off + len > BS.length bs = Nothing
  | otherwise = Just (BS.take len (BS.drop off bs))

-- Replaces XML-significant characters with entity references.
htmlEscape :: T.Text -> T.Text
htmlEscape = T.concatMap escapeChar
  where
    escapeChar '<'  = "&lt;"
    escapeChar '>'  = "&gt;"
    escapeChar '&'  = "&amp;"
    escapeChar '"'  = "&quot;"
    escapeChar '\'' = "&#39;"
    escapeChar c    = T.singleton c

-------------------------------------------------------------------------------
-- Embedded CSS
-------------------------------------------------------------------------------

-- Returns the complete CSS style sheet as a 'B.Builder'.
htmlThemeCss :: B.Builder
htmlThemeCss =
  let css = T.unlines
        [ ":root {"
        , "  --bg: #0d1117;"
        , "  --panel: #161b22;"
        , "  --panel-soft: #010409;"
        , "  --text: #e6edf3;"
        , "  --muted: #8b949e;"
        , "  --border: #30363d;"
        , "  --add-bg: rgba(46, 160, 67, 0.16);"
        , "  --add-text: #3fb950;"
        , "  --del-bg: rgba(248, 81, 73, 0.16);"
        , "  --del-text: #f85149;"
        , "  --ln-bg: #0d1117;"
        , "  --empty-bg: rgba(148, 163, 184, 0.06);"
        , "  --diff-overlay-add: rgba(46, 160, 67, 0.17);"
        , "  --diff-overlay-del: rgba(248, 81, 73, 0.17);"
        , "  --diff-overlay-add-empty: rgba(46, 160, 67, 0.22);"
        , "  --diff-overlay-del-empty: rgba(248, 81, 73, 0.22);"
        , "  --font-ui: 'Inter', 'Noto Sans SC', 'Segoe UI', Helvetica, Arial, sans-serif;"
        , "  --font-mono: 'JetBrains Mono', 'Cascadia Code', 'Consolas', monospace;"
        , "  --scroll-track: #0d1117;"
        , "  --scroll-thumb: #30363d;"
        , "  --scroll-thumb-hover: #3d444d;"
        , "  --square-empty: #21262d;"
        , "}"
        , "body[data-theme='light'] {"
        , "  --bg: #f6f8fa;"
        , "  --panel: #ffffff;"
        , "  --panel-soft: #f3f4f6;"
        , "  --text: #1f2328;"
        , "  --muted: #59636e;"
        , "  --border: #d0d7de;"
        , "  --add-bg: rgba(46, 160, 67, 0.13);"
        , "  --add-text: #1f883d;"
        , "  --del-bg: rgba(207, 34, 46, 0.13);"
        , "  --del-text: #cf222e;"
        , "  --ln-bg: #f6f8fa;"
        , "  --empty-bg: rgba(89, 99, 110, 0.05);"
        , "  --diff-overlay-add: rgba(46, 160, 67, 0.15);"
        , "  --diff-overlay-del: rgba(207, 34, 46, 0.15);"
        , "  --scroll-track: #f6f8fa;"
        , "  --scroll-thumb: #d0d7de;"
        , "  --scroll-thumb-hover: #b8c0c8;"
        , "  --square-empty: #d8dee4;"
        , "}"
        , "* { margin: 0; padding: 0; box-sizing: border-box; }"
        , "* { scrollbar-width: thin; scrollbar-color: var(--scroll-thumb) var(--scroll-track); }"
        , "*::-webkit-scrollbar { width: 10px; height: 10px; }"
        , "*::-webkit-scrollbar-track { background: var(--scroll-track); }"
        , "*::-webkit-scrollbar-thumb { background: var(--scroll-thumb); border: 2px solid var(--scroll-track); border-radius: 6px; }"
        , "*::-webkit-scrollbar-thumb:hover { background: var(--scroll-thumb-hover); }"
        , "body {"
        , "  background-color: var(--bg);"
        , "  color: var(--text);"
        , "  font-family: var(--font-ui);"
        , "  -webkit-font-smoothing: antialiased;"
        , "  text-rendering: optimizeLegibility;"
        , "}"
        , ".page { max-width: 1280px; margin: 32px auto; padding: 0 20px; }"
        , "h1 { margin: 0 0 14px; font-size: 25px; font-weight: 650; letter-spacing: 0.2px; border-bottom: 1px solid var(--border); padding-bottom: 12px; }"
        , ".toolbar { display: flex; gap: 8px; justify-content: flex-end; margin-bottom: 16px; }"
        , ".btn { border: 1px solid var(--border); border-radius: 4px; background: var(--panel); color: var(--text); padding: 5px 12px; font-size: 12px; font-weight: 500; cursor: pointer; }"
        , ".btn:hover { background: #21262d; }"
        , ".summary { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 10px; margin-bottom: 18px; }"
        , ".summary-card { border: 1px solid var(--border); border-radius: 4px; background: var(--panel); text-align: center; padding: 12px 10px; transition: transform .18s ease, background .2s ease; }"
        , ".summary-card:hover { transform: translateY(-2px); background: #21262d; }"
        , ".summary-card .count { font-size: 30px; line-height: 1.05; font-weight: 600; }"
        , ".summary-card .label { margin-top: 6px; font-size: 10px; text-transform: uppercase; letter-spacing: 1.8px; color: var(--muted); }"
        , ".summary-card.added .count { color: var(--add-text); }"
        , ".summary-card.removed .count { color: var(--del-text); }"
        , ".summary-card.modified .count { color: #d29922; }"
        , ".summary-card.total .count { color: var(--muted); }"
        , ".entry-list { display: flex; flex-direction: column; gap: 0; }"
        , ".entry { border: 1px solid var(--border); border-radius: 4px; margin-bottom: 16px; overflow: hidden; background: var(--panel); }"
        , ".entry-head { padding: 12px 16px; background: var(--panel-soft); display: flex; align-items: center; justify-content: space-between; gap: 12px; font-size: 13px; }"
        , ".head-left { display: flex; align-items: center; gap: 8px; min-width: 0; font-family: var(--font-ui); }"
        , ".entry-path { min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; font-weight: 500; letter-spacing: 0.05px; }"
        , ".entry-path-link { color: var(--text); text-decoration: none; cursor: pointer; }"
        , ".entry-path-link:hover { text-decoration: underline; color: #58a6ff; }"
        , ".badge { padding: 2px 8px; border-radius: 8px; font-size: 11px; font-weight: 600; color: #fff; }"
        , ".badge-added { background: #2da44e; }"
        , ".badge-removed { background: #cf222e; }"
        , ".badge-modified { background: #d29922; }"
        , ".head-right { display: flex; align-items: center; gap: 12px; flex-wrap: wrap; justify-content: flex-end; }"
        , ".diff-stats { display: flex; align-items: center; gap: 4px; font-size: 12px; font-weight: 600; }"
        , ".stat-add { color: var(--add-text); }"
        , ".stat-del { color: var(--del-text); }"
        , ".squares-map { display: flex; gap: 1px; margin-left: 4px; }"
        , ".sq { width: 8px; height: 8px; background: var(--square-empty); border-radius: 1px; }"
        , ".sq.filled-add { background: #2da44e; }"
        , ".sq.filled-del { background: #cf222e; }"
        , ".hash-pill { font-family: ui-monospace, monospace; font-size: 11px; color: var(--muted); padding: 2px 6px; border: 1px solid var(--border); border-radius: 4px; }"
        , ".hash-old { color: var(--del-text); }"
        , ".hash-new { color: var(--add-text); }"
        , "body.hide-hash .hash-pill { display: none; }"
        , ".entry-details summary { padding: 8px 16px; font-size: 12px; color: var(--muted); cursor: pointer; list-style: none; border-top: 1px solid var(--border); user-select: none; display: flex; align-items: center; }"
        , ".entry-details summary::before { content: '\\25B6'; display: inline-block; width: 18px; font-size: 10px; transition: transform 0.2s cubic-bezier(0.4, 0, 0.2, 1); }"
        , ".entry-details.is-open summary::before { transform: rotate(90deg); }"
        , ".anim-wrapper { display: grid; grid-template-rows: 0fr; transition: grid-template-rows 0.3s ease-in-out; }"
        , ".entry-details.is-open .anim-wrapper { grid-template-rows: 1fr; }"
        , ".anim-content { overflow: hidden; }"
        , ".code-pane { font-family: var(--font-mono); font-size: 12px; line-height: 20px; background: var(--bg); max-height: 560px; overflow: auto; }"
        , ".diff-grid { display: grid; }"
        , ".view-split.diff-grid { grid-template-columns: 45px 1fr 45px 1fr; }"
        , ".view-unified.diff-grid, .one-side-grid { grid-template-columns: 45px 1fr; }"
        , ".grid-head { position: sticky; top: 0; z-index: 2; background: var(--panel); color: var(--muted); border-bottom: 1px solid var(--border); padding: 5px 8px; font-size: 11px; text-transform: uppercase; letter-spacing: 1px; }"
        , ".row { display: contents; }"
        , ".ln { text-align: right; padding-right: 12px; color: var(--muted); background: var(--ln-bg); user-select: none; border-right: 1px solid var(--border); }"
        , ".code { white-space: pre-wrap; word-break: break-word; position: relative; padding-left: 25px; min-height: 20px; color: var(--text); isolation: isolate; }"
        , ".code.cell-add, .ln.cell-add { background-color: var(--add-bg); }"
        , ".code.cell-del, .ln.cell-del { background-color: var(--del-bg); }"
        , ".code.cell-add::after, .code.cell-del::after { content: ''; position: absolute; inset: 0; z-index: 2; pointer-events: none; }"
        , ".code.cell-add::after { background: var(--diff-overlay-add); }"
        , ".code.cell-del::after { background: var(--diff-overlay-del); }"
        , ".code.cell-empty, .ln.cell-empty {"
        , "  background-color: var(--empty-bg);"
        , "}"
        , ".ln.cell-empty { color: transparent; }"
        , ".code.cell-empty { color: transparent; }"
        , ".code > span { position: relative; z-index: 1; }"
        , ".cell-del.code::before { content: '-'; position: absolute; left: 8px; font-weight: 600; color: var(--del-text); z-index: 3; }"
        , ".cell-add.code::before { content: '+'; position: absolute; left: 8px; font-weight: 600; color: var(--add-text); z-index: 3; }"
        , ".view-unified-el .code.cell-del::before, .view-unified-el .code.cell-add::before { content: none; }"
        , ".cell-empty.code::before { content: ''; }"
        , ".code.hljs { display: block; background: transparent !important; padding-left: 25px; color: inherit; position: relative; z-index: 1; }"
        , ".code .hljs-comment, .code .hljs-quote { opacity: 0.92; }"
        , ".code .hljs-string, .code .hljs-number, .code .hljs-keyword, .code .hljs-title, .code .hljs-type { text-shadow: none; }"
        , ".row:hover .code, .row:hover .ln { filter: brightness(1.3); }"
        , ".one-side-wrapper { border-top: 1px solid var(--border); }"
        , ".one-side-title { padding: 8px 10px; font-size: 11px; color: var(--muted); border-bottom: 1px solid var(--border); text-transform: uppercase; letter-spacing: 1px; }"
        , ".one-side-grid { display: grid; }"
        , ".binary-note { padding: 12px; color: var(--muted); font-size: 12px; }"
        , "body.view-unified .view-split-el { display: none; }"
        , "body:not(.view-unified) .view-unified-el { display: none; }"
        , "body.focus-mode .page { max-width: 100vw; margin: 0; padding: 0; }"
        , "body.focus-mode h1, body.focus-mode .summary { display: none; }"
        , "body.focus-mode .toolbar { margin: 0; padding: 10px 12px; position: sticky; top: 0; z-index: 12; background: var(--panel); border-bottom: 1px solid var(--border); }"
        , "body.focus-mode .entry-list { margin: 0; }"
        , "body.focus-mode .entry { margin: 0; border-left: none; border-right: none; border-bottom: none; border-radius: 0; min-height: calc(100vh - 52px); }"
        , "body.focus-mode .code-pane { max-height: none; height: calc(100vh - 140px); }"
        , "@media (max-width: 980px) {"
        , "  .page { padding: 0 12px; }"
        , "  .summary { grid-template-columns: repeat(2, minmax(0, 1fr)); }"
        , "  .entry-head { flex-direction: column; align-items: flex-start; }"
        , "  .head-right { width: 100%; justify-content: flex-start; }"
        , "  .view-split.diff-grid { grid-template-columns: 42px 1fr 42px 1fr; }"
        , "  .view-unified.diff-grid, .one-side-grid { grid-template-columns: 42px 1fr; }"
        , "}"
        ]
  in B.fromText css

-------------------------------------------------------------------------------
-- Embedded JavaScript
-------------------------------------------------------------------------------

-- Returns the client-side script for theme and view toggles.
htmlThemeJs :: B.Builder
htmlThemeJs =
  let js = T.unlines
        [ "(function(){"
        , "  var body = document.body;"
        , "  var themeBtn = document.getElementById('theme-toggle');"
        , "  var hashBtn = document.getElementById('hash-toggle');"
        , "  var viewBtn = document.getElementById('view-toggle');"
        , "  var openBtn = document.getElementById('open-toggle');"
        , "  var hljsDark = document.getElementById('hljs-theme-dark');"
        , "  var hljsLight = document.getElementById('hljs-theme-light');"
        , "  function safeGet(key) {"
        , "    try { return localStorage.getItem(key); } catch (_) { return null; }"
        , "  }"
        , "  function safeSet(key, value) {"
        , "    try { localStorage.setItem(key, value); } catch (_) {}"
        , "  }"
        , "  function applyTheme(isLight) {"
        , "    if (isLight) {"
        , "      body.setAttribute('data-theme', 'light');"
        , "    } else {"
        , "      body.removeAttribute('data-theme');"
        , "    }"
        , "    if (hljsDark && hljsLight) {"
        , "      hljsDark.disabled = isLight;"
        , "      hljsLight.disabled = !isLight;"
        , "    }"
        , "    safeSet('glassjar-theme-light', isLight ? '1' : '0');"
        , "    themeBtn.textContent = isLight ? '切换深色' : '切换浅色';"
        , "  }"
        , "  function highlightCode() {"
        , "    if (!window.hljs || !window.hljs.highlightElement) { return; }"
        , "    var blocks = document.querySelectorAll('.code');"
        , "    blocks.forEach(function(el){"
        , "      if (el.classList.contains('cell-empty')) { return; }"
        , "      if (el.dataset.hlDone === '1') { return; }"
        , "      var raw = el.textContent || '';"
        , "      if (!raw.trim()) { el.dataset.hlDone = '1'; return; }"
        , "      el.textContent = raw;"
        , "      el.classList.add('hljs');"
        , "      window.hljs.highlightElement(el);"
        , "      el.dataset.hlDone = '1';"
        , "    });"
        , "  }"
        , "  function applyHashHidden(hidden) {"
        , "    body.classList.toggle('hide-hash', hidden);"
        , "    safeSet('glassjar-hide-hash', hidden ? '1' : '0');"
        , "    hashBtn.textContent = hidden ? '显示哈希' : '隐藏哈希';"
        , "  }"
        , "  function applyView(unified) {"
        , "    body.classList.toggle('view-unified', unified);"
        , "    safeSet('glassjar-view-unified', unified ? '1' : '0');"
        , "    viewBtn.textContent = unified ? '切换为分栏视图' : '切换为统一视图';"
        , "  }"
        , "  function setDetailState(el, shouldOpen) {"
        , "    if (shouldOpen) {"
        , "      el.open = true;"
        , "      requestAnimationFrame(function(){ el.classList.add('is-open'); });"
        , "    } else {"
        , "      el.classList.remove('is-open');"
        , "      setTimeout(function(){ el.open = false; }, 300);"
        , "    }"
        , "  }"
        , "  var savedThemeLight = safeGet('glassjar-theme-light') === '1';"
        , "  var savedHideHash = safeGet('glassjar-hide-hash') === '1';"
        , "  var savedUnified = safeGet('glassjar-view-unified') === '1';"
        , "  applyTheme(savedThemeLight);"
        , "  applyHashHidden(savedHideHash);"
        , "  applyView(savedUnified);"
        , "  highlightCode();"
        , "  var focusId = new URLSearchParams(window.location.search).get('focus');"
        , "  if (focusId) {"
        , "    body.classList.add('focus-mode');"
        , "    document.querySelectorAll('.entry-list .entry').forEach(function(el){"
        , "      if (el.id !== focusId) { el.style.display = 'none'; }"
        , "    });"
        , "    var focused = document.getElementById(focusId);"
        , "    if (focused) {"
        , "      focused.querySelectorAll('details.entry-details').forEach(function(d){ d.open = true; d.classList.add('is-open'); });"
        , "      var p = focused.querySelector('.entry-path');"
        , "      if (p) { document.title = 'GlassJar Focus - ' + p.textContent; }"
        , "    }"
        , "  }"
        , "  document.querySelectorAll('details.entry-details').forEach(function(el){"
        , "    var summary = el.querySelector('summary');"
        , "    if (!summary) { return; }"
        , "    summary.addEventListener('click', function(ev){"
        , "      ev.preventDefault();"
        , "      setDetailState(el, !el.classList.contains('is-open'));"
        , "    });"
        , "  });"
        , "  themeBtn.addEventListener('click', function(){"
        , "    applyTheme(!body.hasAttribute('data-theme'));"
        , "  });"
        , "  hashBtn.addEventListener('click', function(){"
        , "    applyHashHidden(!body.classList.contains('hide-hash'));"
        , "  });"
        , "  viewBtn.addEventListener('click', function(){"
        , "    applyView(!body.classList.contains('view-unified'));"
        , "  });"
        , "  openBtn.addEventListener('click', function(){"
        , "    var details = document.querySelectorAll('details.entry-details');"
        , "    var shouldOpen = Array.prototype.some.call(details, function(el){ return !el.classList.contains('is-open'); });"
        , "    details.forEach(function(el){ setDetailState(el, shouldOpen); });"
        , "    openBtn.textContent = shouldOpen ? '全部收起' : '全部展开';"
        , "  });"
        , "})();"
        ]
  in B.fromText js

-- Returns True when content is valid UTF-8.
isUtf8Content :: BSL.ByteString -> Bool
isUtf8Content = either (const False) (const True) . TE.decodeUtf8' . BSL.toStrict
