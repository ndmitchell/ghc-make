{-# LANGUAGE ScopedTypeVariables #-}

module Test(main) where

import qualified Main
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import Development.Shake(removeFiles)
import Development.Shake.FilePath


data Expect = Exit
            | Change FilePath
            | Remain FilePath
              deriving Eq


mtime file = do
    b <- doesFileExist file
    if b then fmap Just $ getModificationTime file else return Nothing

expect :: Expect -> IO (IO ())
expect Exit = return $ return ()
expect (Change x) = do
    old <- mtime x
    return $ do
        new <- mtime x
        when (old == new) $ error $ "File did not change, but should have: " ++ x
expect (Remain x) = do
    old <- mtime x
    return $ do
        new <- mtime x
        when (old /= new) $ error $ "File changed, but should not have: " ++ x


run :: FilePath -> [String] -> [Expect] -> IO ()
run dir args es =
    handle (\(e :: ExitCode) -> if Exit `elem` es then return () else error "Unexpected exit") $ do
        withCurrentDirectory dir $ withArgs args $ do
            acts <- mapM expect es
            threadDelay 1000000
            Main.main
            sequence_ acts

clean :: FilePath -> IO ()
clean dir = removeFiles dir ["//*.hi","//*.hi-boot","//*.o","//*.o-boot","//.ghc-make.*","//Main" <.> exe, "//Root" <.> exe]

withCurrentDirectory :: FilePath -> IO () -> IO ()
withCurrentDirectory dir act = do
    curdir <- getCurrentDirectory
    bracket_ (setCurrentDirectory dir) (setCurrentDirectory curdir) act

touch :: FilePath -> IO ()
touch file = do
    src <- readFile file
    evaluate $ length src
    writeFile file src

main :: IO ()
main = do
    clean "tests/simple"
    run "tests/simple" ["Main.hs"] [Change "Main.o"]
    run "tests/simple" ["Main.hs"] [Remain "Main.o"]
    touch "tests/simple/Main.hs"
    run "tests/simple" ["Main.hs"] [Change "Main.o"]
    clean "tests/simple"

    run "." ["--version"] [Exit]

    clean "tests/complex"
    run "tests/complex" ["Root.hs","-ichildren"] [Change "Main.o"]
    clean "tests/complex"
    putStrLn "Success"
