{-# LANGUAGE RecordWildCards #-}

module Build(main) where

import Control.Monad
import Data.List
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import System.Environment
import System.Exit
import System.Process
import Args


main :: IO ()
main = do
    Args{..} <- args

    when modeGHC $
        exitWith =<< rawSystem "ghc" argsGHC

    let prefix = makeDir </> ".ghc-make"
    withArgs argsShake $ shakeArgs shakeOptions{shakeFiles=prefix, shakeVerbosity=Quiet} $ do
        want [prefix <.> "makefile"]

        prefix <.> "args" *> \out -> do
            alwaysRerun
            writeFileChanged out $ unlines argsGHC

        prefix <.> "makefile" *> \out -> do
            need [prefix <.> "args"]
            () <- cmd "ghc -M -dep-makefile" [out] argsGHC
            opts <- liftIO $ fmap parseMakefile $ readFile out
            () <- cmd "ghc --make" argsGHC
            need $ nub $ concatMap (uncurry (:)) opts


parseMakefile :: String -> [(FilePath, [FilePath])]
parseMakefile = concatMap f . join . lines
    where
        join (x1:x2:xs) | "\\" `isSuffixOf` x1 = join $ (init x1 ++ x2) : xs
        join (x:xs) = x : join xs
        join [] = []

        f x = [(a, words $ drop 1 b) | a <- words a]
            where (a,b) = break (== ':') $ takeWhile (/= '#') x
