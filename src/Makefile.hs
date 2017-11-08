{-# LANGUAGE PatternGuards, DeriveDataTypeable #-}

module Makefile(Makefile(..), Module(..), makefile) where

import Data.List
import Development.Shake.FilePath
import Development.Shake.Classes
import Development.Shake.Util
import Data.Bits
import Data.Functor
import qualified Data.HashMap.Strict as Map
import Prelude


data Module = Module {moduleName :: [String], moduleBoot :: Bool}
    deriving (Typeable, Eq)

instance Show Module where
    show (Module name boot) = intercalate "." name ++ (if boot then "[boot]" else "")

instance Hashable Module where
    hashWithSalt salt (Module a b) = hashWithSalt salt a `xor` hashWithSalt salt b

instance NFData Module where
    rnf (Module a b) = rnf (a,b)

instance Binary Module where
    put (Module a b) = put a >> put b
    get = do a <- get; b <- get; return $ Module a b

data Makefile = Makefile
    {imports :: !(Map.HashMap Module [Either FilePath Module]) -- What does a module import
    ,source :: !(Map.HashMap Module FilePath) -- Where is that modules source located
    }
    deriving Show


makefile :: FilePath -> IO Makefile
makefile file = foldl' f z . parseMakefile <$> readFile file
    where
        z = Makefile Map.empty Map.empty

        -- We rely on the order of the generated makefile, in particular
        -- * The Foo.o : Foo.hs line is always the first with Foo.o on the LHS
        -- * The root module (often Main.o) is always last
        f m (a,[b])
            | Just o <- fromExt "o" a, not $ Map.member o $ source m = m{source=Map.insert o b $ source m}
            | Just o <- fromExt "o" a, Just hi <- fromExt "hi" b = m{imports = Map.insertWith (++) o [Right hi] $ imports m}
            | Just o <- fromExt "o" a = m{imports = Map.insertWith (++) o [Left b] $ imports m}
            | otherwise = m
        f m (a,bs) = foldl' f m [(a,[b]) | b <- bs]


fromExt ext x
    | "-boot" `isSuffixOf` x, Just (Module m _) <- fromExt ext $ take (length x - 5) x = Just $ Module m True
    | takeExtension x == "." ++ ext, isRelative x = Just $ Module (splitDirectories $ dropExtension x) False
    | otherwise = Nothing
