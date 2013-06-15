{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Args(Args(..), args) where

import Data.Either
import Data.List
import Data.Maybe
import System.Environment


data Args = Args
    {argsGHC :: [String] -- ^ Arguments to pass to ghc, does not include --make
    ,argsShake :: [String] -- ^ Arguments to pass to shake
    ,modeGHC :: Bool -- ^ Are these flags which should go direct to GHC, not a --make/-M mode
    ,makeDir :: FilePath -- ^ Directory to place any ghc-make files
    }


args :: IO Args
args = do
    args <- getArgs
    let (argsShake, argsGHC) = splitFlags $ delete "--make" args
    let badFlags = "--version" : "--numeric-version" : flagsConflictingWithM
    let modeGHC = any (`elem` badFlags) argsGHC
    let makeDir = fromMaybe "" $ dumpDir argsGHC
    return Args{..}



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
