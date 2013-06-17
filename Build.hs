{-# LANGUAGE RecordWildCards #-}

module Build(main) where

import Control.Monad
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import System.Environment
import Data.List
import System.Exit
import System.Process
import qualified Data.HashMap.Strict as Map
import Args
import Makefile


-- | Increment every time I change the rules in an incompatible way
ghcMakeVer :: Int
ghcMakeVer = 2


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
            () <- cmd "ghc -M -dep-makefile" [out] args -- FIXME: Put back "-odir. -hidir. -hisuf=hi -osuf=o"
            mk <- liftIO $ makefile out
            need $ Map.elems $ source mk
        mk <- do cache <- newCache makefile; return $ cache $ prefix <.> "makefile"

        -- The result, we can't want the object directly since it is painful to
        -- define a build rule for it because its name depends on both args and makefile
        prefix <.> "result" *> \out -> do
            args <- args
            mk <- mk
            let exec = cmd "ghc --make" args :: Action ()
                grab = need $ map oFile $ Map.keys $ source mk
            if threads == 1 then exec >> grab else grab >> exec

            let output = outputFile $ root mk
            -- ensure that if the file gets deleted we rerun this rule without first trying to
            -- need the output, since we don't have a rule to build the output
            doesFileExist output
            need [output]
            writeFile' out ""

        let match x = do m <- oModule x `mplus` hiModule x; return [oFile m, hiFile m]
        match ?>> \[o,hi] -> do
            let Just m = oModule o
            mk <- mk
            need $ map hiFile $ Map.lookupDefault [] m $ imports mk
            when (threads /= 1) $ do
                args <- args
                cmd "ghc" (delete "Main.hs" args) "-c" [source mk Map.! m]
