{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Test(main) where

import qualified Main
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Directory.Extra
import System.Time.Extra
import System.Environment
import System.Exit
import System.Mem
import System.Random
import Development.Shake(removeFiles)
import Development.Shake.FilePath


---------------------------------------------------------------------
-- TEST EXPECTATIONS

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

touch :: FilePath -> IO ()
touch file = do
    putStrLn $ "Touching " ++ file
    sleep 1 -- to give the file system time to register it
    src <- readFile file
    evaluate $ length src
    writeFile file src

retry :: Int -> IO a -> IO a
retry i x | i <= 0 = error "retry ran out of times"
retry 1 x = x
retry i x = do
    res <- try x
    case res of
        Left (_ :: SomeException) -> retry (i-1) x
        Right v -> return v


---------------------------------------------------------------------
-- RANDOM TESTS

clean :: FilePath -> IO ()
clean dir = do
    putStrLn $ "Cleaning " ++ dir
    -- Retry a lot, sometimes Windows gets caught up
    retry 10 $ do
        performGC
        sleep 1
        removeFiles dir $
            ["//*.hi","//*.hi-boot","//*.o","//*.o-boot"
            ,"//*.hix","//*.hix-boot","//*.ox","//*.ox-boot"
            ,"//.ghc-make.*"
            ,"//Result" <.> exe, "//Main" <.> exe, "//Root" <.> exe]

data Test = Test
    {hisuf :: String
    ,osuf :: String
    ,hidir :: String
    ,odir :: String
    ,outputdir :: String
    ,nolink :: Bool
    ,output :: String
    ,threads :: Int
    } deriving Show

newTest :: FilePath -> IO Test
newTest prefix = do
    hisuf <- pick ["","hi","hix"]
    osuf <- pick ["","o","ox"]
    hidir <- pick $ prefixed ["","hidir","bothdir"]
    odir <- pick $ prefixed ["","odir","bothdir"]
    outputdir <- pick $ prefixed ["","","bothdir","oodir"]
    output <- pick $ prefixed ["","Result","Result" <.> exe]
    nolink <- pick [False,False,True]
    threads <- pick [1,2,3,4]
    let res = Test{..}
    putStrLn $ "Testing with " ++ show res
    return res
    where
        prefixed xs = xs ++ [if x == "" then "" else prefix </> x | x <- xs]
        pick xs = do i <- randomRIO (0, length xs - 1); return $ xs !! i

testFlags :: Test -> [String]
testFlags Test{..} =
    flag "hisuf" hisuf ++ flag "osuf" osuf ++ flag "hidir" hidir ++ flag "odir" odir ++
    flag "outputdir" outputdir ++
    (if nolink then ["-no-link"] else flag "o" output) ++
    ["-j" ++ show threads | threads > 1]
    where flag a b = if b == "" then [] else ['-':a, b]

objName :: Test -> String -> FilePath
objName Test{..} x =
    (if outputdir == "" then odir else outputdir) </> x <.> (if osuf == "" then "o" else osuf)


---------------------------------------------------------------------
-- MAIN DRIVER

main :: IO ()
main = do
    args <- getArgs
    if args /= [] then
        withArgs args Main.main
     else do
        run "." ["--version"] [Exit]
        run "." ["--help"] [Exit]

        let count = 10
        cdir <- getCurrentDirectory
        forM_ [1..count] $ \i -> do
            putStrLn $ "RUNNING TEST " ++ show i ++ " of " ++ show count

            -- Use the simple test to track things rebuild when necessary
            do
                t <- newTest $ cdir </> "tests/simple"
                let t0 = t{threads=1}
                let main_o = objName t "Main"
                let a_o = objName t "A"
                clean "tests/simple"
                run "tests/simple" ("Main.hs":testFlags t0) [Change main_o]
                run "tests/simple" ("Main.hs":testFlags t0) [Remain main_o]
                touch "tests/simple/Main.hs"
                run "tests/simple" ("Main.hs":testFlags t) [Change main_o, Remain a_o]
                touch "tests/simple/Main.hs"
                run "tests/simple" ("Main.hs":testFlags t) [Change main_o, Remain a_o]
                run "tests/simple" ("Main.hs":testFlags t) [Remain main_o]
                clean "tests/simple"

            -- Use the complex test to track that we support all the weird features
            do
                t <- newTest $ cdir </> "tests/complex"
                let main_o = objName t "Main"
                clean "tests/complex"
                run "tests/complex" ("Root.hs":"-ichildren":"--shake--report=-":testFlags t) [Change main_o]
                run "tests/complex" ("Root.hs":"-ichildren":testFlags t) [Remain main_o]
                clean "tests/complex"

        putStrLn "Success"
