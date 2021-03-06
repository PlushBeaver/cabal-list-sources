{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad

import Data.List
import Data.Maybe
import Data.Monoid

import System.FilePath as System
import System.IO as System

import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal

import qualified System.Directory as System
import qualified System.Environment as System
import qualified System.Exit as System


data Input = Input
    { inputPackage :: !Cabal.PackageDescription
    , inputLibraries :: ![Cabal.Library]
    , inputExecutables :: ![Cabal.Executable]
    , inputTestSuites :: ![Cabal.TestSuite]
    , inputBenchmarks :: ![Cabal.Benchmark]
    }
    deriving (Eq, Show)


class Artifact a where
    artifactBuildInfo :: a -> Cabal.BuildInfo

    artifactDirs :: a -> [FilePath]
    artifactDirs = Cabal.hsSourceDirs . artifactBuildInfo

    artifactModules :: a -> [Cabal.ModuleName]
    artifactModules = Cabal.otherModules . artifactBuildInfo


main :: IO ()
main = do
    System.getArgs >>= \args -> case args of
        []     -> run Nothing
        ["-"]  -> run Nothing
        [path] -> run (Just path)
        _ ->
            printHelp >> System.exitFailure
    where
        run maybePath = mapM_ putStrLn =<< listCabalSources maybePath


printHelp :: IO ()
printHelp = do
    self <- System.getProgName
    display $ "Print source and data file paths referenced from a Cabal file (or from"
    display $ "STDIN, in which case the file is assumed to be in current directory)."
    display $ "Usage: " <> self <> " [CABAL_FILE/-]"
    where
        display = hPutStrLn System.stderr


listCabalSources :: Maybe FilePath -> IO [FilePath]
listCabalSources maybePath = do
    text <- maybe System.getContents System.readFile maybePath
    case parseCabalFile text of
        Left error ->
            fail error
        Right input ->
            let path = maybe "." id maybePath
                in listCabalSources' path input


listCabalSources' :: FilePath -> Input -> IO [FilePath]
listCabalSources' path Input{..} = do
    libraryPaths <- fmap concat <$> forM inputLibraries locateModules
    testPaths <- fmap concat <$> forM inputTestSuites locateModules
    benchmarkPaths <- fmap concat <$> forM inputBenchmarks locateModules
    exePaths <- fmap concat <$> forM inputExecutables $ \exe -> do
        let dirs = (Cabal.hsSourceDirs . Cabal.buildInfo) exe
        modulePaths <- locateModules exe
        mainPath <- locateFile (dirs <> ["."]) (Cabal.modulePath exe)
        return $ modulePaths <> maybeToList mainPath

    let dataPaths = (baseDir </>) . (Cabal.dataDir inputPackage </>) <$>
            Cabal.dataFiles inputPackage
        extraPaths = (baseDir </> ) <$>
               Cabal.licenseFiles inputPackage
            <> Cabal.extraSrcFiles inputPackage
            <> Cabal.extraDocFiles inputPackage

    let allPaths = concat
            [ exePaths
            , libraryPaths
            , dataPaths
            , extraPaths
            , testPaths
            , benchmarkPaths
            ]
        in return $ uniq $ sort $ allPaths

    where
        locateModules :: Artifact artifact => artifact -> IO [FilePath]
        locateModules artifact =
            let dirs = artifactDirs artifact
                modules = artifactModules artifact
                toFilePath moduleName = Cabal.toFilePath moduleName <> ".hs"
                in catMaybes <$> mapM (locateFile dirs) (toFilePath <$> modules)

        locateFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
        locateFile possibleDirs relativePath = do
            let possiblePath = (baseDir </>) . (</> relativePath)
                firstFile = findFirstM (System.doesFileExist . possiblePath) possibleDirs
                in fmap possiblePath <$> firstFile

        baseDir = System.takeDirectory path

        findFirstM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
        findFirstM f (x:xs) =
            f x >>= \stop -> if stop then return (Just x) else findFirstM f xs
        findFirstM _ [] =
            return Nothing

        uniq :: Eq a => [a] -> [a]
        uniq (x:y:xs)
            | x == y    = uniq (x:xs)
            | otherwise = x : uniq (y:xs)
        uniq [x] = [x]
        uniq [] = []


instance Artifact Cabal.Executable where
    artifactBuildInfo = Cabal.buildInfo

instance Artifact Cabal.Library where
    artifactBuildInfo = Cabal.libBuildInfo
    artifactModules library =
        Cabal.exposedModules library <>
        (Cabal.otherModules . artifactBuildInfo) library

instance Artifact Cabal.TestSuite where
    artifactBuildInfo = Cabal.testBuildInfo

instance Artifact Cabal.Benchmark where
    artifactBuildInfo = Cabal.benchmarkBuildInfo


parseCabalFile :: String -> Either String Input
parseCabalFile text =
    case Cabal.parsePackageDescription text of
        Cabal.ParseOk _ Cabal.GenericPackageDescription{..} ->
            Right $ Input
                { inputPackage = packageDescription
                , inputLibraries = maybe [] flatten condLibrary
                , inputExecutables = concatMap (flatten . snd) condExecutables
                , inputTestSuites = concatMap (flatten . snd) condTestSuites
                , inputBenchmarks = concatMap (flatten . snd) condBenchmarks
                }
        Cabal.ParseFailed error ->
            Left $ show error
    where
        flatten :: Cabal.CondTree v c a -> [a]
        flatten Cabal.CondNode{..} =
            let recurse (_, tree, maybeTree) = flatten tree <> maybe [] flatten maybeTree
                in condTreeData : concatMap recurse condTreeComponents
