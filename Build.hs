{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Build(main) where

import Control.Monad
import Development.Shake
import Development.Shake.Command
import Development.Shake.Classes
import Development.Shake.FilePath
import System.Environment
import System.Exit
import System.Process
import qualified Data.HashMap.Strict as Map
import Args
import Makefile


-- | Increment every time I change the rules in an incompatible way
ghcMakeVer :: Int
ghcMakeVer = 2


newtype AskImports = AskImports Module deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

newtype AskSource = AskSource Module deriving (Show,Typeable,Eq,Hashable,Binary,NFData)


main :: IO ()
main = do
    Args{..} <- args

    when modeGHC $
        exitWith =<< rawSystem "ghc" argsGHC

    let opts = shakeOptions
            {shakeThreads=threads
            ,shakeFiles=prefix
            ,shakeVerbosity=if threads == 1 then Quiet else Normal
            ,shakeVersion=show ghcMakeVer}
    withArgs argsShake $ shakeArgs opts $ do
        want [prefix <.> "result"]

        -- A file containing the GHC arguments
        prefix <.> "args" *> \out -> do
            alwaysRerun
            writeFileChanged out $ unlines argsGHC
        args <- return $ do need [prefix <.> "args"]; return argsGHC

        -- A file containing the output of -M
        prefix <.> "makefile" *> \out -> do
            args <- args
            -- Use the default o/hi settings so we can parse the makefile properly
            () <- cmd "ghc -M -dep-makefile" [out] args "-odir. -hidir. -hisuf=hi -osuf=o"
            mk <- liftIO $ makefile out
            need $ Map.elems $ source mk
        mk <- do cache <- newCache makefile; return $ cache $ prefix <.> "makefile"
        askImports <- addOracle $ \(AskImports x) -> do mk <- mk; return $ Map.lookupDefault [] x $ imports mk
        askSource <- addOracle $ \(AskSource x) -> do mk <- mk; return $ source mk Map.! x


        -- The result, we can't want the object directly since it is painful to
        -- define a build rule for it because its name depends on both args and makefile
        prefix <.> "result" *> \out -> do
            args <- args
            mk <- mk
            -- if you don't specify an odir/hidir then impossible to reverse from the file name to the module
            let exec = cmd "ghc --make -odir. -hidir." args :: Action ()
                grab = need $ map oFile $ Map.keys $ source mk
            if threads == 1 then exec >> grab else grab >> exec

            let output = outputFile $ source mk Map.! root mk
            -- ensure that if the file gets deleted we rerun this rule without first trying to
            -- need the output, since we don't have a rule to build the output
            b <- doesFileExist output
            unless b $
                error $ "Failed to build output file: " ++ output ++ "\n" ++
                        "Most likely ghc-make has guessed the output location wrongly."
            need [output]
            writeFile' out ""

        let match x = do m <- oModule x `mplus` hiModule x; return [oFile m, hiFile m]
        match ?>> \[o,hi] -> do
            let Just m = oModule o
            source <- askSource (AskSource m)
            need . (source:) . map hiFile =<< askImports (AskImports m)
            when (threads /= 1) $ do
                args <- args
                let isRoot x = x == "Main" || takeExtension x `elem` [".hs",".lhs"]
                cmd "ghc -odir. -hidir." (filter (not . isRoot) args) "-c" [source]
