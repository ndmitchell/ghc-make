{-# LANGUAGE PatternGuards #-}

module Main(main) where

import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import System.Environment
import System.Exit
import System.Process


main :: IO ()
main = do
    args <- getArgs
    let (argsShake, argsGHC) = splitFlags $ delete "--make" args
    let prefix = maybe ".ghc-make" (</> ".ghc-make") $ dumpDir argsGHC

    let badFlags = "--version" : "--numeric-version" : flagsConflictingWithM
    when (any (`elem` badFlags) argsGHC) $
        exitWith =<< rawSystem "ghc" argsGHC

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


-- | All flags conflicting with `ghc -M`.
-- Obtained from ghc/Main.hs, `data PostLoadMode`, must be kept up to date:
-- All modes that are not `DoMkDependHS` (`-M`) are conflicting
-- (apart from `--make`).
flagsConflictingWithM :: [String]
flagsConflictingWithM = [ "--show-iface", "-E", "-C", "-S", "-c"
                        , "--interactive", "-e", "--abi-hash"
                        , "--info"
                        ]


-- | Split flags into (Shake flags, GHC flags)
splitFlags :: [String] -> ([String], [String])
splitFlags = partitionEithers . map f
    where
        f x | Just x <- stripPrefix "--shake-" x = Left $ '-':x
            | Just x <- stripPrefix "-shake-" x = Left $ '-':x
            | otherwise = Right x


-- | Where does the user want to dump temp files (-odir, -hidir)
--   GHC accepts -odir foo, -odirfoo, -odir=foo
dumpDir :: [String] -> Maybe FilePath
dumpDir (flag:x:xs) | flag `elem` dirFlags = Just $ fromMaybe x $ dumpDir xs
dumpDir (x:xs) | x:_ <- mapMaybe (`stripPrefix` x) dirFlags = Just $ fromMaybe (if "=" `isPrefixOf` x then drop 1 x else x) $ dumpDir xs
dumpDir (x:xs) = dumpDir xs
dumpDir [] = Nothing


dirFlags = ["-odir","-hidir"]


parseMakefile :: String -> [(FilePath, [FilePath])]
parseMakefile = concatMap f . join . lines
    where
        join (x1:x2:xs) | "\\" `isSuffixOf` x1 = join $ (init x1 ++ x2) : xs
        join (x:xs) = x : join xs
        join [] = []

        f x = [(a, words $ drop 1 b) | a <- words a]
            where (a,b) = break (== ':') $ takeWhile (/= '#') x
