{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Arguments(Arguments(..), getArguments) where

import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import System.Environment
import Development.Shake.FilePath
import Makefile


data Arguments = Arguments
    {argsGHC :: [String] -- ^ Arguments to pass to ghc, does not include --make
    ,argsShake :: [String] -- ^ Arguments to pass to shake
    ,threads :: Int -- ^ Number of threads to use
    -- Interpretation of the flags
    ,modeGHC :: Bool -- ^ Are these flags which should go direct to GHC, not a --make/-M mode
    ,prefix :: FilePath -- ^ Where make should put its files, e.g. .ghc-make
    -- Where should things live
    ,outputFile :: FilePath -> FilePath -- ^ Root source file, .exe file
    ,hiFile :: Module -> FilePath -- ^ .hi files
    ,oFile :: Module -> FilePath  -- ^ .o files
    ,hiModule :: FilePath -> Maybe Module
    ,oModule :: FilePath -> Maybe Module
    }


getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let (argsThreads, argsRest) = partition (isJust . parseThreads) args
    let threads = max 1 $ fromMaybe 1 $ msum $ map parseThreads argsThreads
    let (argsShake, argsGHC) = splitFlags $ delete "--make" argsRest
    let hasArg x = x `elem` argsGHC
    let getArg b x = findArg b x argsGHC

    let modeGHC = any hasArg $ "--version" : "--numeric-version" : flagsConflictingWithM
    let prefix = fromMaybe "" (getArg True ["-outputdir","-odir"] `mplus` getArg True ["-outputdir","-hidir"]) </> ".ghc-make"
    let outputFile file = let s = fromMaybe (dropExtension file) (getArg False ["-o"])
                          in if null $ takeExtension s then s <.> exe else s
    
    let (hiFile, hiModule) = extFileModule getArg "hi"
    let ( oFile,  oModule) = extFileModule getArg "o"
    return Arguments{..}


extFileModule :: (Bool -> [String] -> Maybe String) -> String -> (Module -> FilePath, FilePath -> Maybe Module)
extFileModule getArg ext = (extFile, extModule)
    where
        extDir = fromMaybe "" $ getArg True ["-outputdir","-" ++ ext ++ "dir"]
        extSuf = fromMaybe ext $ getArg True ["-outputdir","-" ++ ext ++ "suf"]
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
findArg :: Bool -> [String] -> [String] -> Maybe String
findArg implicit flags xs
    | x1:x2:xs <- xs, x1 `elem` flags = add x2 $ rec xs
    | implicit, x:xs <- xs, Just x <- msum $ map (`stripPrefix` x) flags
        = add (if "=" `isPrefixOf` x then drop 1 x else x) $ rec xs
    | x:xs <- xs = rec xs
    | otherwise = Nothing
    where add a b = Just $ fromMaybe a b
          rec = findArg implicit flags


-- Obtained from the man page (listed in the same order as they appear there)
-- and ghc/Main.hs, `data PostLoadMode`:
flagsConflictingWithM = words $
    -- "Help and verbosity options"
    "-? --help -V " ++
    "--supported-extensions --supported-languages " ++
    "--info --version --numeric-version --print-libdir " ++
    -- "Which phases to run"
    "-E -C -S -c " ++
    -- "Alternative modes of operation"
    "--interactive -e " ++
    -- "Interface file options"
    "--show-iface " ++
    -- Undocumented?
    "--abi-hash"



-- | Split flags into (Shake flags, GHC flags)
splitFlags :: [String] -> ([String], [String])
splitFlags = partitionEithers . map f
    where
        f x | Just x <- stripPrefix "--shake-" x = Left $ '-':x
            | Just x <- stripPrefix "-shake-" x = Left $ '-':x
            | otherwise = Right x

