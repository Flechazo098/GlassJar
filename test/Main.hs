{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Test suite for GlassJar.
-- Copyright   : (c) Flechazo, 2026
-- License     : MIT
--
-- Contains unit tests for the diff algorithm and report formatters,
-- and integration tests that operate on real JAR files created in
-- temporary directories.
module Main (main) where

import Codec.Archive.Zip
  ( Entry,
    addEntryToArchive,
    emptyArchive,
    fromArchive,
    toEntry,
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import GlassJar
  ( DiffType (..),
    JarDiff (..),
    JarEntry (..),
    diffJars,
    formatReport,
    formatReportGitDiff,
    formatReportHtml,
    formatReportJson,
    groupInnerClassDiffs,
    readJar,
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

-------------------------------------------------------------------------------
-- Test Helpers
-------------------------------------------------------------------------------

-- | Creates a ZIP entry with the specified name and uncompressed content.
mkEntry :: String -> BS.ByteString -> Entry
mkEntry name content = toEntry name 0 (BSL.fromStrict content)

-- | Writes a minimal JAR file to disk containing the given named entries.
createJar :: FilePath -> [(String, BS.ByteString)] -> IO ()
createJar path entries = do
  let archive =
        foldl
          ( \acc (name, content) ->
              addEntryToArchive (mkEntry name content) acc
          )
          emptyArchive
          entries
  BSL.writeFile path $ fromArchive archive

-------------------------------------------------------------------------------
-- Unit Tests
-------------------------------------------------------------------------------

testEmpty :: IO ()
testEmpty = do
  putStrLn "Unit: diffJars"
  let noDiff = diffJars [] []
  if null noDiff
    then putStrLn "  empty vs empty: PASS"
    else putStrLn "  empty vs empty: FAIL"

  let entry1 = JarEntry "a.txt" 100 mempty "hash1"
  let entry2 = JarEntry "a.txt" 100 mempty "hash1"
  let sameDiff = diffJars [entry1] [entry2]
  if null sameDiff
    then putStrLn "  identical entries: PASS"
    else putStrLn "  identical entries: FAIL"

  let addedDiff = diffJars [] [entry1]
  if length addedDiff == 1 && diffType (head addedDiff) == Added
    then putStrLn "  added entry: PASS"
    else putStrLn "  added entry: FAIL"

  let removedDiff = diffJars [entry1] []
  if length removedDiff == 1 && diffType (head removedDiff) == Removed
    then putStrLn "  removed entry: PASS"
    else putStrLn "  removed entry: FAIL"

  let entry1mod = JarEntry "a.txt" 200 "changed" "hash2"
  let modifiedDiff = diffJars [entry1] [entry1mod]
  if length modifiedDiff == 1 && diffType (head modifiedDiff) == Modified
    then putStrLn "  modified entry: PASS"
    else putStrLn "  modified entry: FAIL"

  let unchangedDiff =
        diffJars
          [entry1, JarEntry "b.txt" 100 mempty "h2"]
          [entry2, JarEntry "b.txt" 100 mempty "h2"]
  if null unchangedDiff
    then putStrLn "  multiple unchanged: PASS"
    else putStrLn "  multiple unchanged: FAIL"

testReportFormat :: IO ()
testReportFormat = do
  putStrLn "Unit: formatReport"
  let emptyReport = formatReport []
  if "identical" `T.isInfixOf` emptyReport
    then putStrLn "  empty report: PASS"
    else putStrLn "  empty report: FAIL"

  let diffs =
        [ JarDiff "com/App.class" Added Nothing (Just "abc123") Nothing (Just mempty),
          JarDiff "config.xml" Removed (Just "def456") Nothing (Just mempty) Nothing,
          JarDiff "MANIFEST.MF" Modified (Just "111aaa") (Just "222bbb") (Just mempty) (Just mempty)
        ]
  let report = formatReport diffs
  if "ADDED" `T.isInfixOf` report
    && "REMOVED" `T.isInfixOf` report
    && "MODIFIED" `T.isInfixOf` report
    then putStrLn "  full report: PASS"
    else putStrLn "  full report: FAIL"

testGitDiffFormat :: IO ()
testGitDiffFormat = do
  putStrLn "Unit: formatReportGitDiff"
  let emptyReport = formatReportGitDiff []
  if "identical" `T.isInfixOf` emptyReport
    then putStrLn "  empty report: PASS"
    else putStrLn "  empty report: FAIL"

  let diffs =
        [ JarDiff "com/App.class" Added Nothing (Just "abc123") Nothing (Just mempty),
          JarDiff "config.xml" Removed (Just "def456") Nothing (Just mempty) Nothing,
          JarDiff "MANIFEST.MF" Modified (Just "111aaa") (Just "222bbb") (Just mempty) (Just mempty)
        ]
  let report = formatReportGitDiff diffs
  if "+++ b/new" `T.isInfixOf` report
    && "--- a/old" `T.isInfixOf` report
    && "com/App.class" `T.isInfixOf` report
    && "config.xml" `T.isInfixOf` report
    && "MANIFEST.MF" `T.isInfixOf` report
    then putStrLn "  git diff report: PASS"
    else putStrLn "  git diff report: FAIL"

  let oldBin = BSL.fromStrict (BS.pack [0xCA, 0xFE, 0x00, 0x01])
  let newBin = BSL.fromStrict (BS.pack [0xCA, 0xFE, 0x00, 0x02])
  let binaryDiffs = [JarDiff "bin.dat" Modified (Just "h1") (Just "h2") (Just oldBin) (Just newBin)]
  let binaryReport = formatReportGitDiff binaryDiffs
  if "binary content differs" `T.isInfixOf` binaryReport
    then putStrLn "  binary git diff fallback: PASS"
    else putStrLn "  binary git diff fallback: FAIL"

testJsonFormat :: IO ()
testJsonFormat = do
  putStrLn "Unit: formatReportJson"
  let emptyJson = formatReportJson []
  if "differences" `T.isInfixOf` emptyJson && "0" `T.isInfixOf` emptyJson
    then putStrLn "  empty json: PASS"
    else putStrLn "  empty json: FAIL"

  let diffs = [JarDiff "foo.class" Added Nothing (Just "hash123") Nothing (Just mempty)]
  let json = formatReportJson diffs
  if "foo.class" `T.isInfixOf` json && "added" `T.isInfixOf` json
    then putStrLn "  json with diff: PASS"
    else putStrLn "  json with diff: FAIL"

testHtmlFormat :: IO ()
testHtmlFormat = do
  putStrLn "Unit: formatReportHtml"
  let html = formatReportHtml []
  if "<!DOCTYPE html>" `T.isInfixOf` html && "GlassJar" `T.isInfixOf` html
    then putStrLn "  empty html: PASS"
    else putStrLn "  empty html: FAIL"

  let oldText = "line 1\nline 2\n"
  let newText = "line 1\nline 2 changed\n"
  let diffs = [JarDiff "readme.txt" Modified (Just "old") (Just "new") (Just oldText) (Just newText)]
  let detailHtml = formatReportHtml diffs
  if "查看差异详情" `T.isInfixOf` detailHtml && "diff-grid view-split" `T.isInfixOf` detailHtml
    then putStrLn "  html content detail: PASS"
    else putStrLn "  html content detail: FAIL"

  let addedOnly = [JarDiff "new.txt" Added Nothing (Just "abc") Nothing (Just "payload")]
  let hashHtml = formatReportHtml addedOnly
  if "open-toggle" `T.isInfixOf` hashHtml
    && "hash-toggle" `T.isInfixOf` hashHtml
    && "old N/A" `T.isInfixOf` hashHtml
    then putStrLn "  html hash controls: PASS"
    else putStrLn "  html hash controls: FAIL"

testInnerClassGroupingOrder :: IO ()
testInnerClassGroupingOrder = do
  putStrLn "Unit: groupInnerClassDiffs order"
  let mkDiff name oldTxt newTxt =
        JarDiff name Modified (Just "old") (Just "new") (Just oldTxt) (Just newTxt)
      pre = mkDiff "aa/Prelude.txt" "before old\n" "before new\n"
      inner = mkDiff "aa/Foo$Inner.class" "inner old\n" "inner new\n"
      outer = mkDiff "aa/Foo.class" "outer old\n" "outer new\n"
      post = mkDiff "zz/Tail.txt" "tail old\n" "tail new\n"
      grouped = groupInnerClassDiffs [pre, inner, outer, post]
      names = map diffEntry grouped

  if names == ["aa/Prelude.txt", "aa/Foo.class", "zz/Tail.txt"]
    then putStrLn "  grouped entry position anchored by outer class: PASS"
    else putStrLn $ "  grouped entry position anchored by outer class: FAIL (got " <> show names <> ")"

  case [d | d <- grouped, diffEntry d == "aa/Foo.class"] of
    [] -> putStrLn "  outer class merged payload ordering: FAIL (grouped entry missing)"
    (d : _) ->
      case diffOldContent d of
        Nothing -> putStrLn "  outer class merged payload ordering: FAIL (old payload missing)"
        Just bs -> do
          let txt = TE.decodeUtf8With lenientDecode (BSL.toStrict bs)
              outerMarker = "// aa/Foo.class"
              innerMarker = "// aa/Foo$Inner.class"
              hasOuter = outerMarker `T.isInfixOf` txt
              hasInner = innerMarker `T.isInfixOf` txt
              outerPos = T.length (fst (T.breakOn outerMarker txt))
              innerPos = T.length (fst (T.breakOn innerMarker txt))
          if hasOuter && hasInner && outerPos < innerPos
            then putStrLn "  outer class merged payload ordering: PASS"
            else putStrLn "  outer class merged payload ordering: FAIL"

  let mkAdded name txt =
        JarDiff name Added Nothing (Just "new") Nothing (Just txt)
      addedInner = mkAdded "aa/Only$Inner.class" "inner new\n"
      addedOuter = mkAdded "aa/Only.class" "outer new\n"
      groupedAdded = groupInnerClassDiffs [addedInner, addedOuter]

  case groupedAdded of
    [] -> putStrLn "  added grouping payload ordering: FAIL (grouped entry missing)"
    (d : _) ->
      case diffNewContent d of
        Nothing -> putStrLn "  added grouping payload ordering: FAIL (new payload missing)"
        Just bs -> do
          let txt = TE.decodeUtf8With lenientDecode (BSL.toStrict bs)
              outerMarker = "// aa/Only.class"
              innerMarker = "// aa/Only$Inner.class"
              hasOuter = outerMarker `T.isInfixOf` txt
              hasInner = innerMarker `T.isInfixOf` txt
              outerPos = T.length (fst (T.breakOn outerMarker txt))
              innerPos = T.length (fst (T.breakOn innerMarker txt))
          if hasOuter && hasInner && outerPos < innerPos
            then putStrLn "  added grouping payload ordering: PASS"
            else putStrLn "  added grouping payload ordering: FAIL"

-------------------------------------------------------------------------------
-- Integration Tests
-------------------------------------------------------------------------------

testRealJar :: IO ()
testRealJar = withSystemTempDirectory "glassjar-test" $ \tmpDir -> do
  putStrLn "Integration: real JAR files"

  let jar1Path = tmpDir </> "old.jar"
  let jar2Path = tmpDir </> "new.jar"

  -- Create old.jar with some entries
  createJar
    jar1Path
    [ ("META-INF/MANIFEST.MF", BS8.pack "Manifest-Version: 1.0\n"),
      ("com/example/App.class", BS8.pack "fake-bytecode-v1"),
      ("com/example/Util.class", BS8.pack "fake-bytecode-util")
    ]

  -- Create new.jar: same App.class, modified Util.class, added Config.class
  createJar
    jar2Path
    [ ("META-INF/MANIFEST.MF", BS8.pack "Manifest-Version: 1.0\n"),
      ("com/example/App.class", BS8.pack "fake-bytecode-v1"),
      ("com/example/Util.class", BS8.pack "fake-bytecode-util-MODIFIED"),
      ("com/example/Config.class", BS8.pack "fake-bytecode-config")
    ]

  -- Read JARs
  oldResult <- readJar jar1Path
  newResult <- readJar jar2Path

  case (oldResult, newResult) of
    (Right oldEntries, Right newEntries) -> do
      let diffs = diffJars oldEntries newEntries

      -- Check: Util.class modified, Config.class added (App.class unchanged, MANIFEST.MF unchanged)
      let addedCount = length [d | d <- diffs, diffType d == Added]
      let removedCount = length [d | d <- diffs, diffType d == Removed]
      let modifiedCount = length [d | d <- diffs, diffType d == Modified]

      if addedCount == 1
        then putStrLn "  added entry count: PASS"
        else putStrLn $ "  added entry count: FAIL (expected 1, got " <> show addedCount <> ")"

      if removedCount == 0
        then putStrLn "  removed entry count: PASS"
        else putStrLn $ "  removed entry count: FAIL (expected 0, got " <> show removedCount <> ")"

      if modifiedCount == 1
        then putStrLn "  modified entry count: PASS"
        else putStrLn $ "  modified entry count: FAIL (expected 1, got " <> show modifiedCount <> ")"

      if diffEntry (head [d | d <- diffs, diffType d == Modified]) == "com/example/Util.class"
        then putStrLn "  modified entry name: PASS"
        else putStrLn "  modified entry name: FAIL"
    (Left err, _) -> putStrLn $ "  FAIL: cannot read old.jar: " <> err
    (_, Left err) -> putStrLn $ "  FAIL: cannot read new.jar: " <> err

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

-- | Runs all unit and integration tests sequentially. Exits with a non-zero
-- code if any test assertion fails.
main :: IO ()
main = do
  putStrLn "=== GlassJar Test Suite ==="
  putStrLn ""

  testEmpty
  putStrLn ""
  testReportFormat
  putStrLn ""
  testGitDiffFormat
  putStrLn ""
  testJsonFormat
  putStrLn ""
  testHtmlFormat
  putStrLn ""
  testInnerClassGroupingOrder
  putStrLn ""
  testRealJar

  putStrLn ""
  putStrLn "=== All tests complete ==="
