{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad

import Data.List
import Data.Monoid

import System.FilePath as System
import System.IO as System

import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Verbosity as Cabal

import qualified System.Directory as System
import qualified System.Environment as System
import qualified System.Exit as System


data Input = Input
    { inputPackage :: Cabal.PackageDescription
    , inputLibraries :: [Cabal.Library]
    , inputExecutables :: [Cabal.Executable]
    }
    deriving (Eq, Show)


main :: IO ()
main = do
    System.getArgs >>= \args -> case args of
        [path] ->
            mapM_ putStrLn =<< listCabalSources path
        _ ->
            printHelp >> System.exitFailure


printHelp :: IO ()
printHelp = do
    self <- System.getProgName
    display $ "Print source and data file paths referenced from a Cabal file."
    display $ "Usage: " <> self <> " CABAL_FILE"
    where
        display = hPutStrLn System.stderr


listCabalSources :: FilePath -> IO [FilePath]
listCabalSources path = do
    Input{..} <- readCabalFile path

    let escapeName = map (\c -> if c == '-' then '_' else c) . Cabal.unPackageName
        ignoredModules = Cabal.fromString <$>
            [ "Paths_" <> (escapeName . Cabal.pkgName . Cabal.package) inputPackage
            ]

    libraryPaths <- fmap concat <$> forM inputLibraries $ \library -> do
        let info = Cabal.libBuildInfo library
            dirs = Cabal.hsSourceDirs info
            allModules = Cabal.exposedModules library <> Cabal.otherModules info
            modules = filter ((flip notElem) ignoredModules) allModules
        forM modules $ \moduleName ->
            locateFile dirs (toFilePath moduleName)

    exePaths <- fmap concat <$> forM inputExecutables $ \exe -> do
        let info = Cabal.buildInfo exe
            dirs = Cabal.hsSourceDirs info
            modules = Cabal.otherModules info
        modulePaths <- forM modules $ \moduleName -> locateFile dirs (toFilePath moduleName)
        mainPath <- locateFile (dirs <> ["."]) (Cabal.modulePath exe)
        return $ mainPath : modulePaths

    let dataPaths = (baseDir </>) . (Cabal.dataDir inputPackage </>) <$> Cabal.dataFiles inputPackage

    return $ uniq $ sort $ exePaths <> libraryPaths <> dataPaths

    where
        locateFile :: [FilePath] -> FilePath -> IO FilePath
        locateFile possibleDirs relativePath = do
            let possiblePath = (baseDir </>) . (</> relativePath)
            found <- findFirstM (System.doesFileExist . possiblePath) possibleDirs
            case found of
                Just dir -> return $ possiblePath dir
                Nothing -> fail $ "unable to locate: " <> relativePath

        baseDir = System.takeDirectory path

        toFilePath moduleName =
            Cabal.toFilePath moduleName <> ".hs"

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


readCabalFile :: FilePath -> IO Input
readCabalFile path = do
    Cabal.GenericPackageDescription{..} <- Cabal.readPackageDescription Cabal.silent path
    return $ Input
        { inputPackage = packageDescription
        , inputLibraries = maybe [] flatten condLibrary
        , inputExecutables = concatMap (flatten . snd) condExecutables
        }
    where
        flatten :: Cabal.CondTree v c a -> [a]
        flatten Cabal.CondNode{..} =
            let recurse (_, tree, maybeTree) = flatten tree <> maybe [] flatten maybeTree
                in condTreeData : concatMap recurse condTreeComponents
