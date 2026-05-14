{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : Command-line interface for GlassJar.
-- Copyright   : (c) Flechazo, 2026
-- License     : MIT
--
-- Defines command-line behavior for reading two JAR files, computing
-- structural differences, and printing a report in the selected format.
module Main where

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BSL
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified GlassJar
import Options.Applicative
  ( Parser,
    ParserInfo,
    ReadM,
    auto,
    eitherReader,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefault,
    showDefaultWith,
    str,
    strArgument,
    switch,
    value,
    (<**>),
  )
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStr, hSetEncoding, stderr, stdout, utf8)

-------------------------------------------------------------------------------
-- CLI Options
-------------------------------------------------------------------------------

-- | Stores parsed command-line arguments.
data Options = Options
  { -- | Path to the old input.
    optInput1 :: !FilePath,
    -- | Path to the new input.
    optInput2 :: !FilePath,
    -- | Output format for the report.
    optFormat :: !GlassJar.OutputFormat,
    optDecompiler :: !GlassJar.DecompilerBackend,
    optToolsDir :: !FilePath,
    optHideLambda :: !Bool,
    optDecompileJobs :: !Int,
    optDecompileBatchMode :: !GlassJar.DecompileBatchMode,
    optDecompileCacheDir :: !FilePath,
    optDisableDecompileCache :: !Bool,
    optIgnoreClassSameSize :: !Bool,
    optDisableCrc :: !Bool,
    optDisableInnerGroup :: !Bool
  }
  deriving (Show)

-- | Parses an output format option value.
--
-- Accepts @\"text\"@, @\"json\"@, @\"html\"@, @\"gitdiff\"@, or @\"git\"@
-- in any letter case.
readFormat :: ReadM GlassJar.OutputFormat
readFormat = eitherReader $ \s ->
  case map toLower s of
    "text" -> Right GlassJar.FormatText
    "json" -> Right GlassJar.FormatJson
    "html" -> Right GlassJar.FormatHtml
    "gitdiff" -> Right GlassJar.FormatGitDiff
    "git" -> Right GlassJar.FormatGitDiff
    _ -> Left $ "Invalid format '" <> s <> "'. Expected: gitdiff, text, json, or html"

-- | Parses a decompiler backend option value.
readBackend :: ReadM GlassJar.DecompilerBackend
readBackend = eitherReader $ \s ->
  case map toLower s of
    "auto" -> Right GlassJar.BackendAuto
    "cfr" -> Right GlassJar.BackendCfr
    "vineflower" -> Right GlassJar.BackendVineflower
    _ -> Left $ "Invalid decompiler backend '" <> s <> "'. Expected: auto, cfr, or vineflower"

-- | Parses a decompile batch mode option value.
readBatchMode :: ReadM GlassJar.DecompileBatchMode
readBatchMode = eitherReader $ \s ->
  case map toLower s of
    "auto" -> Right GlassJar.BatchAuto
    "on" -> Right GlassJar.BatchOn
    "off" -> Right GlassJar.BatchOff
    _ -> Left $ "Invalid decompile batch mode '" <> s <> "'. Expected: auto, on, or off"

-- | Constructs the full CLI argument parser.
optionsParser :: Parser Options
optionsParser =
  Options
    <$> strArgument
      ( metavar "INPUT1"
          <> help "Path to the old input (archive, directory, or file)"
      )
    <*> strArgument
      ( metavar "INPUT2"
          <> help "Path to the new input (archive, directory, or file)"
      )
    <*> option
      readFormat
      ( long "format"
          <> short 'f'
          <> metavar "FORMAT"
          <> value GlassJar.FormatGitDiff
          <> showDefaultWith showFormat
          <> help "Output format: gitdiff, text, json, or html"
      )
    <*> option
      readBackend
      ( long "decompiler"
          <> metavar "BACKEND"
          <> value GlassJar.BackendAuto
          <> showDefaultWith showBackend
          <> help "Class decompiler backend: auto, cfr, or vineflower"
      )
    <*> option
      str
      ( long "tools-dir"
          <> metavar "DIR"
          <> value "data"
          <> showDefault
          <> help "Directory containing cfr/vineflower jar files"
      )
    <*> switch
      ( long "hide-lambda"
          <> help "Hide lambda-related lines in decompiled class output"
      )
    <*> option
      auto
      ( long "decompile-jobs"
          <> metavar "N"
          <> value 4
          <> showDefault
          <> help "Number of parallel decompile workers"
      )
    <*> option
      readBatchMode
      ( long "decompile-batch-mode"
          <> metavar "MODE"
          <> value GlassJar.BatchAuto
          <> showDefaultWith showBatchMode
          <> help "Batch strategy: auto, on, or off"
      )
    <*> option
      str
      ( long "decompile-cache-dir"
          <> metavar "DIR"
          <> value ".glassjar-cache/decompile"
          <> showDefault
          <> help "Directory used for cross-run decompile cache"
      )
    <*> switch
      ( long "disable-decompile-cache"
          <> help "Disable decompile cache"
      )
    <*> switch
      ( long "ignore-class-same-size"
          <> help "Ignore class diffs when old/new uncompressed sizes are identical"
      )
    <*> switch
      ( long "disable-crc"
          <> help "Disable CRC32-based diff comparison and use hash comparison"
      )
    <*> switch
      ( long "disable-inner-group"
          <> help "Disable grouping of inner classes under their outer class"
      )

-- | Returns the lowercase spelling of an 'GlassJar.OutputFormat' value.
showFormat :: GlassJar.OutputFormat -> String
showFormat GlassJar.FormatGitDiff = "gitdiff"
showFormat GlassJar.FormatText = "text"
showFormat GlassJar.FormatJson = "json"
showFormat GlassJar.FormatHtml = "html"

-- | Returns the lowercase spelling of a 'GlassJar.DecompilerBackend' value.
showBackend :: GlassJar.DecompilerBackend -> String
showBackend GlassJar.BackendAuto = "auto"
showBackend GlassJar.BackendCfr = "cfr"
showBackend GlassJar.BackendVineflower = "vineflower"

-- | Returns the lowercase spelling of a 'GlassJar.DecompileBatchMode' value.
showBatchMode :: GlassJar.DecompileBatchMode -> String
showBatchMode GlassJar.BatchAuto = "auto"
showBatchMode GlassJar.BatchOn = "on"
showBatchMode GlassJar.BatchOff = "off"

-- | Returns parser metadata used by @--help@ output.
opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Diff two analyzable inputs and export the differences"
        <> header "glassjar - Structural and logical diff tool for archives, directories, and files"
    )

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

-- | Executes the command-line workflow.
--
-- Reads both JAR files, computes differences, and writes the report
-- to standard output.
--
-- Exit codes:
--
--   * 0 — no differences found
--   * 1 — differences found, or either JAR file could not be read
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  startNs <- getMonotonicTimeNSec
  options <- execParser opts
  let input1 = optInput1 options
      input2 = optInput2 options
      fmt = optFormat options
      diffSettings =
        GlassJar.defaultDiffSettings
          { GlassJar.dsUseCrcComparison = not (optDisableCrc options),
            GlassJar.dsIgnoreClassSameSize = optIgnoreClassSameSize options,
            GlassJar.dsGroupInnerClasses = not (optDisableInnerGroup options)
          }
      decompileSettings =
        GlassJar.defaultDecompileSettings
          { GlassJar.dcBackend = optDecompiler options,
            GlassJar.dcToolsDir = optToolsDir options,
            GlassJar.dcShowLambda = not (optHideLambda options),
            GlassJar.dcJobs = max 1 (optDecompileJobs options),
            GlassJar.dcBatchMode = optDecompileBatchMode options,
            GlassJar.dcUseCache = not (optDisableDecompileCache options),
            GlassJar.dcCacheDir = optDecompileCacheDir options,
            GlassJar.dcPreferOuterClassView = not (optDisableInnerGroup options)
          }

  -- Read both inputs
  updateProgressBar "Read inputs" 0 2
  oldResult <- GlassJar.readInput input1
  updateProgressBar "Read inputs" 1 2
  newResult <- GlassJar.readInput input2
  finishProgressBar "Read inputs" 2 2

  oldEntries <- case oldResult of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack err
      exitFailure
    Right es -> return es

  newEntries <- case newResult of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack err
      exitFailure
    Right es -> return es

  -- Compute diff
  updateProgressBar "Compute diff" 0 1
  let diffs = GlassJar.diffJarsWith diffSettings oldEntries newEntries
  finishProgressBar "Compute diff" 1 1
  logInfo $ "Diff result: " <> T.pack (show (length diffs)) <> " entries changed."
  let classDiffCount = length [() | d <- diffs, ".class" `T.isSuffixOf` T.toLower (GlassJar.diffEntry d)]
  logInfo $ "Decompiler backend: " <> T.pack (showBackend (optDecompiler options))
  decompileStartNs <- getMonotonicTimeNSec
  enrichedDiffs <-
    GlassJar.prepareDiffsWithDecompilersProgress
      decompileSettings
      (updateProgressBar "Decompile classes")
      diffs
  finishProgressBar "Decompile classes" classDiffCount classDiffCount
  decompileEndNs <- getMonotonicTimeNSec
  let failedDecompileCount = countDecompileFailures enrichedDiffs
      succeededDecompileCount = max 0 (classDiffCount - failedDecompileCount)
  logOk $
    "Decompiler result: success="
      <> T.pack (show succeededDecompileCount)
      <> ", failed="
      <> T.pack (show failedDecompileCount)
      <> ", elapsed="
      <> formatDurationMs (decompileEndNs - decompileStartNs)
      <> "."

  updateProgressBar "Group inner classes" 0 1
  let finalDiffs =
        if GlassJar.dsGroupInnerClasses diffSettings
          then GlassJar.groupInnerClassDiffs enrichedDiffs
          else enrichedDiffs
  finishProgressBar "Group inner classes" 1 1

  -- Output report
  updateProgressBar "Render report" 0 1
  let report = case fmt of
        GlassJar.FormatGitDiff -> GlassJar.formatReportGitDiff finalDiffs
        GlassJar.FormatText -> GlassJar.formatReport finalDiffs
        GlassJar.FormatJson -> GlassJar.formatReportJson finalDiffs
        GlassJar.FormatHtml -> GlassJar.formatReportHtml finalDiffs
  finishProgressBar "Render report" 1 1

  TIO.putStrLn report

  endNs <- getMonotonicTimeNSec
  logOk $ "Total elapsed: " <> formatDurationMs (endNs - startNs) <> "."

  -- Exit with non-zero code if there are differences
  unless (null finalDiffs) exitFailure

countDecompileFailures :: [GlassJar.JarDiff] -> Int
countDecompileFailures diffs =
  length
    [ ()
      | d <- diffs,
        any hasFailMarker [GlassJar.diffOldContent d, GlassJar.diffNewContent d]
    ]
  where
    hasFailMarker Nothing = False
    hasFailMarker (Just bs) =
      let t = TE.decodeUtf8With lenientDecode (BSL.toStrict bs)
       in "/* decompile failed */" `T.isInfixOf` t

-- | Draws one progress-bar frame for a workflow stage.
updateProgressBar :: String -> Int -> Int -> IO ()
updateProgressBar label current total = do
  let totalSafe = max 1 total
      clamped = max 0 (min current totalSafe)
      width = 28
      filled =
        if total <= 0
          then width
          else min width ((clamped * width) `div` totalSafe)
      pct :: Int
      pct = if total <= 0 then 100 else (clamped * 100) `div` totalSafe
      doneSeg = ansiGreenS <> replicate filled '#' <> ansiResetS
      todoSeg = ansiDimS <> replicate (width - filled) '.' <> ansiResetS
      pctColor =
        if pct >= 100
          then ansiGreenS
          else ansiYellowS
      frame =
        "\r"
          <> ansiBlueS
          <> padRight 20 label
          <> ansiResetS
          <> " ["
          <> doneSeg
          <> todoSeg
          <> "] "
          <> pctColor
          <> padLeft 3 (show pct)
          <> ansiResetS
          <> "% ("
          <> show current
          <> "/"
          <> show total
          <> ")"
          <> "\x1b[K"
  hPutStr stderr frame
  hFlush stderr

-- | Prints the final frame and terminates the current progress line.
finishProgressBar :: String -> Int -> Int -> IO ()
finishProgressBar label current total = do
  updateProgressBar label current total
  TIO.hPutStrLn stderr ""

-- | Pads a string to the right using spaces.
padRight :: Int -> String -> String
padRight n s = take n (s <> replicate n ' ')

-- | Pads a string to the left using spaces.
padLeft :: Int -> String -> String
padLeft n s =
  let deficit = n - length s
   in replicate (max 0 deficit) ' ' <> s

-- | Writes an informational stage message with terminal highlight.
logInfo :: T.Text -> IO ()
logInfo msg = TIO.hPutStrLn stderr $ ansiBlue <> "[INFO] " <> ansiReset <> msg

-- | Writes a success stage message with terminal highlight.
logOk :: T.Text -> IO ()
logOk msg = TIO.hPutStrLn stderr $ ansiGreen <> "[DONE] " <> ansiReset <> msg

ansiBlue :: T.Text
ansiBlue = "\x1b[36m"

ansiGreen :: T.Text
ansiGreen = "\x1b[32m"

ansiReset :: T.Text
ansiReset = "\x1b[0m"

ansiBlueS :: String
ansiBlueS = "\x1b[36m"

ansiGreenS :: String
ansiGreenS = "\x1b[32m"

ansiYellowS :: String
ansiYellowS = "\x1b[33m"

ansiDimS :: String
ansiDimS = "\x1b[2m"

ansiResetS :: String
ansiResetS = "\x1b[0m"

-- | Formats nanosecond durations into milliseconds with one decimal place.
formatDurationMs :: Word64 -> T.Text
formatDurationMs ns =
  let tenthsMs :: Integer
      tenthsMs = toInteger ns `div` 100000
      wholeMs = tenthsMs `div` 10
      frac = tenthsMs `mod` 10
   in T.pack (show wholeMs <> "." <> show frac <> " ms")
