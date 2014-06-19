{-# LANGUAGE PatternGuards, RecordWildCards #-}

module Arguments(Arguments(..), getArguments) where

import Control.Monad
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
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
    ,hiDir :: FilePath -- ^ -hidir
    ,oDir :: FilePath -- ^ -odir
    ,hiFile :: Module -> FilePath -- ^ .hi files
    ,oFile :: Module -> FilePath  -- ^ .o files
    ,hiModule :: FilePath -> Maybe Module
    ,oModule :: FilePath -> Maybe Module
    }

helpMessage =
    ["ghc-make, (C) Neil Mitchell"
    ,""
    ,"  ghc-make [ghc-options] --shake[shake-options] [-jN]"
    ,""
    ,"ghc-make is a drop-in replacement for 'ghc', and accepts GHC arugments."
    ,"For GHC arguments, see 'ghc --help' or <http://haskell.org/haskellwiki/GHC>."
    ,""
    ,"ghc-make uses 'shake', and accepts Shake arguments prefixed by '--shake'."
    ,"For Shake arguments, see 'ghc-make --shake--help'."
    ,"As an example, to write a profile report to stdout pass '--shake--report=-'."
    ,""
    ,"In addition, 'ghc-make' accepts the following option:"
    ,""
    ,"  -jN --threads=N   Allow N modules to compile in parallel (defaults to 1)."
    ]


getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    when (any (`elem` helpFlags) args) $ do
        putStrLn $ unlines helpMessage
        exitSuccess

    let (argsThreads, argsRest) = partition (isJust . parseThreads) args
    let threads = max 1 $ fromMaybe 1 $ msum $ map parseThreads argsThreads
    let (argsShake, argsGHC) = splitFlags $ delete "--make" argsRest
    let hasArg x = x `elem` argsGHC
    let getArg b x = findArg b x argsGHC

    let modeGHC = any hasArg $ "--version" : "--numeric-version" : flagsConflictingWithM
    let prefix = fromMaybe "" (getArg True ["-outputdir","-odir"] `mplus` getArg True ["-outputdir","-hidir"]) </> ".ghc-make"
    let outputFile file = let s = fromMaybe (dropExtension file) (getArg False ["-o"])
                          in if null $ takeExtension s then s <.> exe else s
    
    let (hiDir, hiFile, hiModule) = extFileModule getArg "hi"
    let ( oDir,  oFile,  oModule) = extFileModule getArg "o"
    return Arguments{..}


extFileModule :: (Bool -> [String] -> Maybe String) -> String -> (FilePath, Module -> FilePath, FilePath -> Maybe Module)
extFileModule getArg ext = (extDir, extFile, extModule)
    where
        extDir = fromMaybe "" $ getArg True ["-outputdir","-" ++ ext ++ "dir"]
        extSuf = fromMaybe ext $ getArg True ["-outputdir","-" ++ ext ++ "suf"]
        extFile (Module name boot) = extDir </> joinPath name <.> extSuf ++ (if boot then "-boot" else "")
        extModule s
            | "-boot" `isSuffixOf` s, Just (Module name _) <- extModule $ take (length s - 5) s = newModule name True
            | extDir `isPrefixOf` s && extSuf `isSuffixOf` s
                = newModule (splitDirectories $ dropWhile isPathSeparator $ dropExtensions $ drop (length extDir) s) False
            | otherwise = Nothing

newModule :: [String] -> Bool -> Maybe Module
newModule xs y = if all isValid xs then Just $ Module xs y else Nothing
    where isValid (x:xs) = isUpper x && all (\x -> isAlphaNum x || x `elem` "\'_") xs
          isValid [] = False


parseThreads :: String -> Maybe Int
parseThreads x = do
    x <- msum $ map (`stripPrefix` x) ["-threads","--threads","-j"]
    x <- return $ fromMaybe x $ stripPrefix "=" x
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


helpFlags = words "-? --help"

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

