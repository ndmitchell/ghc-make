{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Args(Args(..), args) where

import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import System.Environment
import Development.Shake.FilePath
import Makefile


data Args = Args
    {argsGHC :: [String] -- ^ Arguments to pass to ghc, does not include --make
    ,argsShake :: [String] -- ^ Arguments to pass to shake
    ,threads :: Int -- ^ Number of threads to use
    -- Interpretation of the flags
    ,modeGHC :: Bool -- ^ Are these flags which should go direct to GHC, not a --make/-M mode
    ,prefix :: FilePath -- ^ Where make should put its files, e.g. .ghc-make
    -- Where should things live
    ,outputFile :: Module -> FilePath -- ^ Final output, .exe file
    ,hiFile :: Module -> FilePath -- ^ .hi files
    ,oFile :: Module -> FilePath  -- ^ .o files
    ,hiModule :: FilePath -> Maybe Module
    ,oModule :: FilePath -> Maybe Module
    }


args :: IO Args
args = do
    args <- getArgs
    let (argsThreads, argsRest) = partition (isJust . parseThreads) args
    let threads = max 1 $ fromMaybe 1 $ msum $ map parseThreads argsThreads
    let (argsShake, argsGHC) = splitFlags $ delete "--make" argsRest
    let hasArg x = x `elem` argsGHC
    let getArg b x = findArg b x argsGHC

    let modeGHC = any hasArg $ "--version" : "--numeric-version" : flagsConflictingWithM
    let prefix = fromMaybe "" (getArg True "-odir" `mplus` getArg True "-hidir") </> ".ghc-make"
    let outputFile (Module name _) = fromMaybe "" (getArg True "-outputdir") </> fromMaybe (joinPath name) (getArg False "-o") <.> exe
    
    let (hiFile, hiModule) = extFileModule getArg "hi"
    let ( oFile,  oModule) = extFileModule getArg "o"
    return Args{..}


extFileModule :: (Bool -> String -> Maybe String) -> String -> (Module -> FilePath, FilePath -> Maybe Module)
extFileModule getArg ext = (extFile, extModule)
    where
        extDir = fromMaybe "" $ getArg True $ "-" ++ ext ++ "dir"
        extSuf = fromMaybe ext $ getArg True $ "-" ++ ext ++ "suf"
        extFile (Module name boot) = extDir </> joinPath name <.> extSuf ++ (if boot then "-boot" else "")
        extModule s
            | "-boot" `isSuffixOf` s, Just (Module name _) <- extModule $ take (length s - 5) s = Just $ Module name True
            | extDir `isPrefixOf` s && extSuf `isSuffixOf` s
                = Just $ Module (splitDirectories $ dropWhile isPathSeparator $ dropExtensions $ drop (length extDir) s) False
            | otherwise = Nothing


parseThreads :: String -> Maybe Int
parseThreads x = do
    x <- msum $ map (`stripPrefix` x) ["-threads","--threads","-j"]
    [(i,"")] <- return $ reads x
    return i


-- | -odir is implicit since -odirfoo works, but -o is explicit
findArg :: Bool -> String -> [String] -> Maybe String
findArg implicit flag xs
    | x1:x2:xs <- xs, flag == x1 = add x2 $ rec xs
    | implicit, x:xs <- xs, Just x <- stripPrefix flag x = add (if "=" `isPrefixOf` x then drop 1 x else x) $ rec xs
    | x:xs <- xs = rec xs
    | otherwise = Nothing
    where add a b = Just $ fromMaybe a b
          rec = findArg implicit flag


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

