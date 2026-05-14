{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : GlassJar.Decompile
-- Description : Class decompilation enrichment for JAR diffs.
-- Copyright   : (c) Flechazo, 2026
-- License     : MIT
-- Maintainer  : 2558755403@qq.com
--
-- Rewrites class-entry payloads into decompiled source text using
-- configurable backend tools (CFR, Vineflower).
module GlassJar.Decompile
  ( DecompilerBackend (..),
    DecompileBatchMode (..),
    DecompileSettings (..),
    defaultDecompileSettings,
    prepareDiffsWithDecompilers,
    prepareDiffsWithDecompilersProgress,
  )
where

import qualified Codec.Compression.Zlib as Z
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar (modifyMVar_, newMVar)
import Control.Monad (filterM)
import Crypto.Hash (Digest, MD5, hash)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char (toLower)
import Data.List (isInfixOf, isSuffixOf, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import GlassJar.Internal
  ( decodeLenient,
    digestToHex,
    isClassEntry,
    stripClassExt,
    toEntryPath,
  )
import GlassJar.Types (JarDiff (..))
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.Exit (ExitCode (..))
import System.FilePath
  ( makeRelative,
    replaceExtension,
    takeDirectory,
    (</>),
  )
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

-------------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------------

-- | Selects the decompiler backend to use for class decompilation.
data DecompilerBackend
  = -- | Tries Vineflower first, then falls back to CFR.
    BackendAuto
  | -- | Uses CFR exclusively.
    BackendCfr
  | -- | Uses Vineflower exclusively.
    BackendVineflower
  deriving (Show, Eq)

-- | Controls how class decompilation tasks are grouped for tool execution.
data DecompileBatchMode
  = -- | Chooses per-class or batched mode automatically based on input count.
    BatchAuto
  | -- | Forces batched execution.
    BatchOn
  | -- | Forces per-class execution.
    BatchOff
  deriving (Show, Eq)

-- | Configures class decompilation behavior.
--
-- @dcJobs@ controls parallel worker count.
--
-- @dcCacheDir@ specifies the cache directory; @dcUseCache@ enables caching.
--
-- @dcPreferOuterClassView@ replaces inner class content with the outer
-- class decompilation result when the outer class is among changed entries.
data DecompileSettings = DecompileSettings
  { dcBackend :: !DecompilerBackend,
    dcToolsDir :: !FilePath,
    dcShowLambda :: !Bool,
    dcJobs :: !Int,
    dcBatchMode :: !DecompileBatchMode,
    dcUseCache :: !Bool,
    dcCacheDir :: !FilePath,
    dcPreferOuterClassView :: !Bool
  }
  deriving (Show, Eq)

-- | Provides default decompilation settings.
defaultDecompileSettings :: DecompileSettings
defaultDecompileSettings =
  DecompileSettings
    { dcBackend = BackendAuto,
      dcToolsDir = "data",
      dcShowLambda = True,
      dcJobs = 4,
      dcBatchMode = BatchAuto,
      dcUseCache = True,
      dcCacheDir = ".glassjar-cache/decompile",
      dcPreferOuterClassView = True
    }

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Rewrites class-entry payloads into decompiled source text using
-- configured decompiler tools.
prepareDiffsWithDecompilers :: DecompileSettings -> [JarDiff] -> IO [JarDiff]
prepareDiffsWithDecompilers settings =
  prepareDiffsWithDecompilersProgress settings (\_ _ -> pure ())

-- | Rewrites class-entry payloads and invokes @onProgress@ after each
-- class entry is decompiled.
--
-- The callback receives:
--
--   * The number of class entries processed so far
--   * The total class entries that require decompilation
prepareDiffsWithDecompilersProgress ::
  DecompileSettings ->
  (Int -> Int -> IO ()) ->
  [JarDiff] ->
  IO [JarDiff]
prepareDiffsWithDecompilersProgress settings onProgress diffs = do
  let totalClassCount = length [() | d <- diffs, isClassEntry (diffEntry d)]
      payloads = buildPayloads settings diffs
      payloadLookup = Map.fromList [((diSideNew p, diEntry p), diKey p) | p <- payloads]
  onProgress 0 totalClassCount
  batchOut <- decompileClassBatch settings payloads

  doneVar <- newMVar 0
  mapM (rewriteOne doneVar totalClassCount batchOut payloadLookup) (zip [0 :: Int ..] diffs)
  where
    rewriteOne doneVar total batchOut payloadLookup (ix, d)
      | not (isClassEntry (diffEntry d)) = pure d
      | otherwise = do
          oldC <- traverse (rewriteSide batchOut payloadLookup False (payloadKey ix False) (diffEntry d)) (diffOldContent d)
          newC <- traverse (rewriteSide batchOut payloadLookup True (payloadKey ix True) (diffEntry d)) (diffNewContent d)
          modifyMVar_ doneVar $ \done -> do
            let done' = done + 1
            onProgress done' total
            pure done'
          pure
            d
              { diffOldContent = oldC,
                diffNewContent = newC
              }

    rewriteSide batchOut payloadLookup isNew k entry raw =
      case Map.lookup k batchOut of
        Just (Right txt) ->
          pure (BSL.fromStrict (TE.encodeUtf8 (applyLambdaFilter (dcShowLambda settings) txt)))
        _ | Just outer <- outerClassPath entry ->
          case Map.lookup (isNew, outer) payloadLookup >>= (`Map.lookup` batchOut) of
            Just (Right txt) ->
              pure (BSL.fromStrict (TE.encodeUtf8 (applyLambdaFilter (dcShowLambda settings) txt)))
            _ ->
              decompileClassPayload settings entry raw
        _ ->
          decompileClassPayload settings entry raw

-------------------------------------------------------------------------------
-- Payload construction
-------------------------------------------------------------------------------

data DecompileInput = DecompileInput
  { diKey :: !Int,
    diEntry :: !T.Text,
    diBytes :: !BSL.ByteString,
    diSideNew :: !Bool,
    diDigest :: !T.Text
  }

buildPayloads :: DecompileSettings -> [JarDiff] -> [DecompileInput]
buildPayloads settings diffs = concatMap mkOne (zip [0 :: Int ..] diffs)
  where
    changedClassEntries =
      Map.fromList
        [ (diffEntry d, ())
          | d <- diffs,
            isClassEntry (diffEntry d)
        ]

    mkOne (ix, d)
      | not (isClassEntry (diffEntry d)) = []
      | otherwise =
          oldPayload ++ newPayload
      where
        shouldSkip =
          dcPreferOuterClassView settings
            && isInnerClassEntry (diffEntry d)
            && maybe False (`Map.member` changedClassEntries) (outerClassPath (diffEntry d))
        oldPayload =
          [ mkPayload ix False (diffEntry d) oldBs
            | not shouldSkip,
              Just oldBs <- [diffOldContent d]
          ]
        newPayload =
          [ mkPayload ix True (diffEntry d) newBs
            | not shouldSkip,
              Just newBs <- [diffNewContent d]
          ]

    mkPayload ix isNew entry bs =
      DecompileInput
        { diKey = payloadKey ix isNew,
          diEntry = entry,
          diBytes = bs,
          diSideNew = isNew,
          diDigest = digestToHex (hash (BSL.toStrict bs))
        }

payloadKey :: Int -> Bool -> Int
payloadKey ix isNew = ix * 2 + if isNew then 1 else 0

isInnerClassEntry :: T.Text -> Bool
isInnerClassEntry entry =
  isClassEntry entry && "$" `T.isInfixOf` stripClassExt entry

-------------------------------------------------------------------------------
-- Single-class decompilation
-------------------------------------------------------------------------------

decompileClassPayload :: DecompileSettings -> T.Text -> BSL.ByteString -> IO BSL.ByteString
decompileClassPayload settings clsEntryName classBytes = do
  result <- decompileWithBackends settings clsEntryName classBytes
  pure $
    case result of
      Right t -> BSL.fromStrict (TE.encodeUtf8 (applyLambdaFilter (dcShowLambda settings) t))
      Left err -> renderDecompileFailure clsEntryName err

renderDecompileFailure :: T.Text -> String -> BSL.ByteString
renderDecompileFailure clsEntryName err =
  BSL.fromStrict . TE.encodeUtf8 $
    T.unlines
      [ "/* decompile failed */",
        "/* entry: " <> clsEntryName <> " */",
        "/* reason: " <> T.pack err <> " */"
      ]

-------------------------------------------------------------------------------
-- Batch orchestration
-------------------------------------------------------------------------------

decompileClassBatch ::
  DecompileSettings ->
  [DecompileInput] ->
  IO (Map.Map Int (Either String T.Text))
decompileClassBatch _ [] = pure Map.empty
decompileClassBatch settings inputs =
  do
    cacheHits <- loadBatchCache settings inputs
    let missInputs = [di | di <- inputs, Map.notMember (diKey di) cacheHits]
    computed <- runMissInputs settings missInputs
    let merged = Map.union cacheHits computed
    saveBatchCache settings merged inputs
    pure merged
  where
    runMissInputs _ [] = pure Map.empty
    runMissInputs cfg miss = do
      let (oldMiss, newMiss) = partition (not . diSideNew) miss
      oldOut <- runOneSide cfg oldMiss
      newOut <- runOneSide cfg newMiss
      pure (Map.union oldOut newOut)

    runOneSide _ [] = pure Map.empty
    runOneSide cfg sideInputs =
      case dcBatchMode cfg of
        BatchOff -> runPerClass cfg sideInputs
        BatchOn -> runBatched cfg sideInputs
        BatchAuto ->
          if length sideInputs <= 6
            then runPerClass cfg sideInputs
            else runBatched cfg sideInputs

runPerClass ::
  DecompileSettings ->
  [DecompileInput] ->
  IO (Map.Map Int (Either String T.Text))
runPerClass settings inputs = do
  let jobs = max 1 (dcJobs settings)
      buckets = splitInto jobs inputs
  parts <- forConcurrently buckets $ \bucket ->
    mapM runOne bucket
  pure . Map.fromList . concat $ parts
  where
    runOne di = do
      r <- decompileWithBackends settings (diEntry di) (diBytes di)
      pure (diKey di, r)

runBatched ::
  DecompileSettings ->
  [DecompileInput] ->
  IO (Map.Map Int (Either String T.Text))
runBatched settings inputs = do
  let jobs = max 1 (dcJobs settings)
      workers = chooseBatchWorkers jobs (length inputs)
      buckets = splitInto workers inputs
  parts <- forConcurrently buckets $ \bucket -> do
    out <- runBatchEntries settings [(diEntry di, diBytes di) | di <- bucket]
    pure (map (\di -> (diKey di, resolveBatchEntry out (diEntry di))) bucket)
  pure . Map.fromList . concat $ parts

chooseBatchWorkers :: Int -> Int -> Int
chooseBatchWorkers jobs totalInputs
  | totalInputs <= 0 = 1
  | otherwise =
      let targetPerBatch = 40 :: Int
          bySize = max 1 ((totalInputs + targetPerBatch - 1) `div` targetPerBatch)
       in min jobs bySize

runBatchEntries ::
  DecompileSettings ->
  [(T.Text, BSL.ByteString)] ->
  IO (Map.Map T.Text (Either String T.Text))
runBatchEntries settings entries =
  case dcBackend settings of
    BackendCfr -> runBatchCfrFilled settings entries
    BackendVineflower -> runBatchVineflowerFilled settings entries
    BackendAuto -> do
      vf <- runBatchVineflowerFilled settings entries
      let unresolved = [(e, bs) | (e, bs) <- entries, unresolvedEntry e vf]
      if null unresolved
        then pure vf
        else do
          cfr <- runBatchCfrFilled settings unresolved
          pure $ Map.unionWith preferRight vf cfr
  where
    unresolvedEntry entry out =
      case Map.lookup entry out of
        Just (Right _) -> False
        _ -> True

    preferRight a b =
      case a of
        Right _ -> a
        Left _ -> b

resolveBatchEntry :: Map.Map T.Text (Either String T.Text) -> T.Text -> Either String T.Text
resolveBatchEntry out entry =
  case Map.lookup entry out of
    Just (Right txt) -> Right txt
    _ ->
      case outerClassPath entry of
        Just outer ->
          case Map.lookup outer out of
            Just (Right txt) -> Right txt
            _ -> Left "batch output missing"
        Nothing -> Left "batch output missing"

outerClassPath :: T.Text -> Maybe T.Text
outerClassPath entry
  | not (isClassEntry entry) = Nothing
  | otherwise =
      let stem = stripClassExt entry
          (outerStem, rest) = T.breakOn "$" stem
       in if T.null rest
            then Nothing
            else Just (outerStem <> ".class")

-------------------------------------------------------------------------------
-- Cache
-------------------------------------------------------------------------------

loadBatchCache ::
  DecompileSettings ->
  [DecompileInput] ->
  IO (Map.Map Int (Either String T.Text))
loadBatchCache settings =
  fmap (Map.fromList . catMaybes) . mapM readOne
  where
    readOne di = do
      m <- readCacheText settings di
      pure $ case m of
        Just txt -> Just (diKey di, Right txt)
        Nothing -> Nothing

saveBatchCache ::
  DecompileSettings ->
  Map.Map Int (Either String T.Text) ->
  [DecompileInput] ->
  IO ()
saveBatchCache settings out =
  mapM_ saveOne
  where
    saveOne di =
      case Map.lookup (diKey di) out of
        Just (Right txt) -> writeCacheText settings di txt
        _ -> pure ()

readCacheText :: DecompileSettings -> DecompileInput -> IO (Maybe T.Text)
readCacheText settings di
  | not (dcUseCache settings) = pure Nothing
  | otherwise = do
      let fp = cacheFilePath settings di
      ok <- doesFileExist fp
      if not ok
        then pure Nothing
        else do
          raw <- BS.readFile fp
          if cacheMagic `BS.isPrefixOf` raw
            then
              let payload = BS.drop (BS.length cacheMagic) raw
                  decompressed = Z.decompress (BSL.fromStrict payload)
               in pure (Just (decodeLenient decompressed))
            else pure (Just (TE.decodeUtf8With lenientDecode raw))

writeCacheText :: DecompileSettings -> DecompileInput -> T.Text -> IO ()
writeCacheText settings di txt
  | not (dcUseCache settings) = pure ()
  | otherwise = do
      let fp = cacheFilePath settings di
          encoded = TE.encodeUtf8 txt
          compressed =
            BSL.toStrict $
              Z.compressWith
                Z.defaultCompressParams
                  { Z.compressLevel = Z.bestCompression
                  }
                (BSL.fromStrict encoded)
          packed = cacheMagic <> compressed
      createParentDirectories fp
      BS.writeFile fp packed

cacheFilePath :: DecompileSettings -> DecompileInput -> FilePath
cacheFilePath settings di =
  let backendTag =
        case dcBackend settings of
          BackendAuto -> "auto"
          BackendCfr -> "cfr"
          BackendVineflower -> "vineflower"
      lambdaTag = if dcShowLambda settings then "lambda1" else "lambda0"
      configTag = T.unpack (digestToHex (hash (TE.encodeUtf8 (T.pack (backendTag <> "|" <> lambdaTag))) :: Digest MD5))
      digestHex = T.unpack (diDigest di)
      fileStem =
        case splitAt 16 digestHex of
          (prefix, _) | not (null prefix) -> prefix
          _ -> "0000000000000000"
      lvl1 = take 2 configTag
      lvl2 = take 2 (drop 2 configTag)
      name = fileStem <> ".gjcbin"
   in dcCacheDir settings </> lvl1 </> lvl2 </> name

cacheMagic :: BS.ByteString
cacheMagic = BS.pack [71, 74, 67, 49]

splitInto :: Int -> [a] -> [[a]]
splitInto n xs
  | n <= 1 = [xs]
  | otherwise = go k xs
  where
    k = max 1 n
    go _ [] = []
    go buckets ys =
      let len = length ys
          sz = max 1 ((len + buckets - 1) `div` buckets)
          (h, t) = splitAt sz ys
       in h : go (buckets - 1) t

-------------------------------------------------------------------------------
-- Batch backend runners
-------------------------------------------------------------------------------

runBatchVineflowerFilled ::
  DecompileSettings ->
  [(T.Text, BSL.ByteString)] ->
  IO (Map.Map T.Text (Either String T.Text))
runBatchVineflowerFilled settings inputs = do
  r <- runBatchVineflower settings inputs
  pure (fillBatchResult inputs r)

runBatchCfrFilled ::
  DecompileSettings ->
  [(T.Text, BSL.ByteString)] ->
  IO (Map.Map T.Text (Either String T.Text))
runBatchCfrFilled settings inputs = do
  r <- runBatchCfr settings inputs
  pure (fillBatchResult inputs r)

fillBatchResult ::
  [(T.Text, BSL.ByteString)] ->
  Either String (Map.Map T.Text T.Text) ->
  Map.Map T.Text (Either String T.Text)
fillBatchResult inputs result =
  case result of
    Left err ->
      Map.fromList [(entry, Left err) | (entry, _) <- inputs]
    Right out ->
      Map.fromList
        [ (entry, maybe (Left "batch output missing") Right (Map.lookup entry out))
          | (entry, _) <- inputs
        ]

runBatchVineflower ::
  DecompileSettings ->
  [(T.Text, BSL.ByteString)] ->
  IO (Either String (Map.Map T.Text T.Text))
runBatchVineflower settings inputs =
  withSystemTempDirectory "glassjar-vf-batch" $ \tmp -> do
    jarPath <- findToolJar (dcToolsDir settings) ["vineflower", "fernflower", "quiltflower"]
    case jarPath of
      Nothing -> pure (Left "vineflower jar not found")
      Just vfJar -> do
        let inDir = tmp </> "in"
            outDir = tmp </> "out"
            lambdaArg = if dcShowLambda settings then "--lambda-to-anonymous-class=0" else "--lambda-to-anonymous-class=1"
            args = ["-jar", vfJar, "--folder", lambdaArg, inDir, outDir]
        writeBatchInputs inDir inputs
        createDirectoryIfMissing True outDir
        (ec, _out, err) <- readProcessWithExitCode "java" args ""
        if ec /= ExitSuccess
          then pure (Left err)
          else Right <$> readBatchOutputs outDir

runBatchCfr ::
  DecompileSettings ->
  [(T.Text, BSL.ByteString)] ->
  IO (Either String (Map.Map T.Text T.Text))
runBatchCfr settings inputs =
  withSystemTempDirectory "glassjar-cfr-batch" $ \tmp -> do
    jarPath <- findToolJar (dcToolsDir settings) ["cfr", "cfr-"]
    case jarPath of
      Nothing -> pure (Left "cfr jar not found")
      Just cfrJar -> do
        let inDir = tmp </> "in"
            outDir = tmp </> "out"
            args =
              [ "-jar",
                cfrJar,
                inDir,
                "--outputdir",
                outDir,
                "--silent",
                "true",
                "--decodelambdas",
                if dcShowLambda settings then "true" else "false"
              ]
        writeBatchInputs inDir inputs
        createDirectoryIfMissing True outDir
        (ec, _out, err) <- readProcessWithExitCode "java" args ""
        if ec /= ExitSuccess
          then pure (Left err)
          else Right <$> readBatchOutputs outDir

writeBatchInputs :: FilePath -> [(T.Text, BSL.ByteString)] -> IO ()
writeBatchInputs inDir =
  mapM_ (\(entry, bs) -> writeParentFile (inDir </> toToolClassPath entry) bs)

readBatchOutputs :: FilePath -> IO (Map.Map T.Text T.Text)
readBatchOutputs outDir = do
  files <- collectJavaFiles outDir
  pairs <- mapM toPair files
  pure (Map.fromList pairs)
  where
    toPair fp = do
      bytes <- BS.readFile fp
      let rel = makeRelative outDir fp
          relNorm = toEntryPath rel
          clsName = replaceExtension relNorm ".class"
      pure (T.pack clsName, TE.decodeUtf8With lenientDecode bytes)

collectJavaFiles :: FilePath -> IO [FilePath]
collectJavaFiles root = do
  exists <- doesDirectoryExist root
  if not exists then pure [] else go root
  where
    go dir = do
      names <- listDirectory dir
      let files = [dir </> n | n <- names]
      subdirs <- filterM doesDirectoryExist files
      let javaFiles = [f | f <- files, ".java" `isSuffixOf` map toLower f]
      nested <- fmap concat (mapM go subdirs)
      pure (javaFiles ++ nested)

-------------------------------------------------------------------------------
-- Single-class backend dispatch
-------------------------------------------------------------------------------

decompileWithBackends :: DecompileSettings -> T.Text -> BSL.ByteString -> IO (Either String T.Text)
decompileWithBackends settings clsEntryName classBytes =
  case dcBackend settings of
    BackendCfr -> runCfr settings clsEntryName classBytes
    BackendVineflower -> runVineflower settings clsEntryName classBytes
    BackendAuto -> do
      vf <- runVineflower settings clsEntryName classBytes
      case vf of
        Right t -> pure (Right t)
        Left _ -> runCfr settings clsEntryName classBytes

runCfr :: DecompileSettings -> T.Text -> BSL.ByteString -> IO (Either String T.Text)
runCfr settings clsEntryName classBytes =
  withSystemTempDirectory "glassjar-cfr" $ \tmp -> do
    jarPath <- findToolJar (dcToolsDir settings) ["cfr", "cfr-"]
    case jarPath of
      Nothing -> pure (Left "cfr jar not found")
      Just cfrJar -> do
        let inClass = tmp </> toToolClassPath clsEntryName
            outDir = tmp </> "out"
        writeParentFile inClass classBytes
        createDirectoryIfMissing True outDir
        let args = ["-jar", cfrJar, inClass, "--outputdir", outDir, "--silent", "true", "--decodelambdas", if dcShowLambda settings then "true" else "false"]
        (ec, _out, err) <- readProcessWithExitCode "java" args ""
        if ec /= ExitSuccess
          then pure (Left err)
          else do
            mJava <- firstJavaFile outDir
            case mJava of
              Nothing -> pure (Left "cfr output missing")
              Just fp -> Right . TE.decodeUtf8With lenientDecode <$> BS.readFile fp

runVineflower :: DecompileSettings -> T.Text -> BSL.ByteString -> IO (Either String T.Text)
runVineflower settings clsEntryName classBytes =
  withSystemTempDirectory "glassjar-vf" $ \tmp -> do
    jarPath <- findToolJar (dcToolsDir settings) ["vineflower", "fernflower", "quiltflower"]
    case jarPath of
      Nothing -> pure (Left "vineflower jar not found")
      Just vfJar -> do
        let inClass = tmp </> toToolClassPath clsEntryName
            outDir = tmp </> "out"
            lambdaArg = if dcShowLambda settings then "--lambda-to-anonymous-class=0" else "--lambda-to-anonymous-class=1"
            args = ["-jar", vfJar, "--folder", lambdaArg, inClass, outDir]
        writeParentFile inClass classBytes
        createDirectoryIfMissing True outDir
        (ec, _out, err) <- readProcessWithExitCode "java" args ""
        if ec /= ExitSuccess
          then pure (Left err)
          else do
            mJava <- firstJavaFile outDir
            case mJava of
              Nothing -> pure (Left "vineflower output missing")
              Just fp -> Right . TE.decodeUtf8With lenientDecode <$> BS.readFile fp

applyLambdaFilter :: Bool -> T.Text -> T.Text
applyLambdaFilter True t = t
applyLambdaFilter False t =
  T.unlines
    [ line
      | line <- T.lines t,
        not ("lambda$" `T.isInfixOf` line)
    ]

findToolJar :: FilePath -> [String] -> IO (Maybe FilePath)
findToolJar dir prefixes = do
  ok <- doesDirectoryExist dir
  if not ok
    then pure Nothing
    else do
      names <- listDirectory dir
      let lowered = map (\n -> (n, map toLower n)) names
          pick =
            [ dir </> n
              | (n, low) <- lowered,
                ".jar" `isSuffixOf` low,
                any (`isInfixOf` low) prefixes
            ]
      pure $ case pick of
        [] -> Nothing
        (x : _) -> Just x

writeParentFile :: FilePath -> BSL.ByteString -> IO ()
writeParentFile fp content = do
  createParentDirectories fp
  BSL.writeFile fp content

createParentDirectories :: FilePath -> IO ()
createParentDirectories fp =
  createDirectoryIfMissing True (takeDirectory fp)

firstJavaFile :: FilePath -> IO (Maybe FilePath)
firstJavaFile root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure Nothing
    else go root
  where
    go dir = do
      names <- listDirectory dir
      let files = [dir </> n | n <- names]
      subdirs <- filterM doesDirectoryExist files
      let javaFiles = [f | f <- files, ".java" `isSuffixOf` map toLower f]
      case javaFiles of
        (f : _) -> pure (Just f)
        [] -> search subdirs

    search [] = pure Nothing
    search (d : ds) = do
      r <- go d
      case r of
        Just _ -> pure r
        Nothing -> search ds

toToolClassPath :: T.Text -> FilePath
toToolClassPath = map slash . T.unpack
  where
    slash '/' = '\\'
    slash c = c
