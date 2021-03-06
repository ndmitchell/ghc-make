{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main(main) where

import Control.Monad
import Data.Either
import Data.Maybe
import Data.Functor
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import System.Environment
import System.Exit
import System.Process
import qualified Data.HashMap.Strict as Map
import Arguments
import Makefile
import Prelude


-- | Increment every time I change the rules in an incompatible way
ghcMakeVer :: Int
ghcMakeVer = 3


newtype AskImports = AskImports Module deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult AskImports = [Either FilePath Module]

newtype AskSource = AskSource Module deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult AskSource = String


main :: IO ()
main = do
    Arguments{..} <- getArguments

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
        prefix <.> "args" %> \out -> do
            alwaysRerun
            writeFileChanged out $ unlines argsGHC
        let needArgs = do need [prefix <.> "args"]; return argsGHC

        -- A file containing the ghc-pkg list output
        prefix <.> "pkgs" %> \out -> do
            alwaysRerun
            (Stdout s, Stderr (_ :: String)) <- cmd "ghc-pkg list --verbose"
            writeFileChanged out s
        let needPkgs = need [prefix <.> "pkgs"]

        -- A file containing the output of -M
        prefix <.> "makefile" %> \out -> do
            args <- needArgs
            needPkgs
            -- Use the default o/hi settings so we can parse the makefile properly
            () <- cmd "ghc -M -include-pkg-deps -dep-suffix=" [""] "-dep-makefile" [out] args "-odir. -hidir. -hisuf=hi -osuf=o"
            mk <- liftIO $ makefile out
            need $ Map.elems $ source mk
        needMk <- do cache <- newCache (\x -> do need [x]; liftIO $ makefile x); return $ cache $ prefix <.> "makefile"
        askImports <- addOracle $ \(AskImports x) -> do mk <- needMk; return $ Map.lookupDefault [] x $ imports mk
        askSource <- addOracle $ \(AskSource x) -> do mk <- needMk; return $ source mk Map.! x


        -- The result, we can't want the object directly since it is painful to
        -- define a build rule for it because its name depends on both args and makefile
        prefix <.> "result" %> \out -> do
            args <- needArgs
            mk <- needMk
            let output = if "-no-link" `elem` argsGHC then Nothing
                         else fmap outputFile $ Map.lookup (Module ["Main"] False) $ source mk

            -- if you don't specify an odir/hidir then impossible to reverse from the file name to the module
            let exec = when (isJust output || threads == 1) $
                            cmd "ghc --make -odir. -hidir." args
                grab = need $ map oFile $ Map.keys $ source mk
            if threads == 1 then exec >> grab else grab >> exec

            case output of
                Nothing -> return ()
                Just output -> do
                    -- ensure that if the file gets deleted we rerun this rule without first trying to
                    -- need the output, since we don't have a rule to build the output
                    b <- doesFileExist output
                    unless b $
                        error $ "Failed to build output file: " ++ output ++ "\n" ++
                                "Most likely ghc-make has guessed the output location wrongly."
                    need [output]
            writeFile' out ""

        let match x = do m <- oModule x `mplus` hiModule x; return [oFile m, hiFile m]
        match &?> \[o,hi] -> do
            let Just m = oModule o
            source <- askSource (AskSource m)
            (files,mods) <- partitionEithers <$> askImports (AskImports m)
            need $ source : map hiFile mods ++ files
            when (threads /= 1) $ do
                args <- needArgs
                let isRoot x = x == "Main" || takeExtension x `elem` [".hs",".lhs"]
                cmd "ghc -odir. -hidir." (filter (not . isRoot) args) (if hiDir == "" then [] else ["-i" ++ hiDir]) "-o" [o] "-c" [source]
