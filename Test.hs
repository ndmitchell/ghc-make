{-# LANGUAGE ScopedTypeVariables #-}

module Test(main) where

import qualified Main
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.IO
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
run dir args es = do
    putStrLn $ "Running " ++ unwords (dir:args)
    handle (\(e :: ExitCode) -> if Exit `elem` es then return () else error "Unexpected exit") $ do
        withCurrentDirectory dir $ withArgs args $ do
            acts <- mapM expect es
            threadDelay 1000000
            Main.main
            sequence_ acts

clean :: FilePath -> IO ()
clean dir = do
    putStrLn $ "Cleaning " ++ dir
    removeFiles dir ["//*.hi","//*.hi-boot","//*.o","//*.o-boot","//.ghc-make.*","//Main" <.> exe, "//Root" <.> exe]

withCurrentDirectory :: FilePath -> IO () -> IO ()
withCurrentDirectory dir act = do
    curdir <- getCurrentDirectory
    bracket_ (setCurrentDirectory dir) (setCurrentDirectory curdir) act

touch :: FilePath -> IO ()
touch file = do
    putStrLn $ "Touching " ++ file
    src <- readFile file
    evaluate $ length src
    writeFile file src

copyFileUnix :: FilePath -> FilePath -> IO ()
copyFileUnix from to = do
    src <- readFile from
    withBinaryFile to WriteMode $ \h -> hPutStr h $ filter (/= '\r') src

main :: IO ()
main = do
    args <- getArgs
    if args /= [] then
        withArgs args Main.main
     else do
        clean "tests/simple"
        run "tests/simple" ["Main.hs"] [Change "Main.o"]
        copyFileUnix "tests/simple/.ghc-make.makefile" "tests/Simple.makefile"
        run "tests/simple" ["Main.hs"] [Remain "Main.o"]
        touch "tests/simple/Main.hs"
        run "tests/simple" ["Main.hs"] [Change "Main.o", Remain "A.o"]
        clean "tests/simple"
        run "tests/simple" ["Main.hs","-j3","-o","simple" <.> exe] [Change $ "simple" <.> exe]
        clean "tests/simple"
        run "tests/simple" ["Main.hs","-j3","-hidir","his","-o","simple" <.> exe] [Change $ "simple" <.> exe]
        clean "tests/simple"

        run "." ["--version"] [Exit]
        run "." ["--help"] [Exit]

        clean "tests/complex"
        run "tests/complex" ["Root.hs","-ichildren"] [Change "Main.o"]
        copyFileUnix "tests/complex/.ghc-make.makefile" "tests/Complex.makefile"
        run "tests/complex" ["Root.hs","-ichildren","-j3"] [Remain "Main.o"]
        clean "tests/complex"
        run "tests/complex" ["Root.hs","-ichildren","-j3","--shake--report=-"] [Change "Main.o"]
        run "tests/complex" ["Root.hs","-ichildren"] [Remain "Main.o"]
        clean "tests/complex"
        putStrLn "Success"
