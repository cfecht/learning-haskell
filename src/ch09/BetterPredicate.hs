module BetterPredicate where

import System.IO
import Data.Time.Clock
import Control.Monad (filterM)
import Control.Exception (handle, bracket)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)

import RecursiveContents (getRecursiveContents)

type Predicate =  FilePath 
               -> Permissions
               -> Maybe Integer
               -> UTCTime
               -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle doNothing $ 
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)

doNothing :: IOError -> IO(Maybe a)
doNothing e = return Nothing
               
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
     where check name = do 
             perms <- getPermissions name
             size <- getFileSize name
             modified <- getModificationTime name
             return (p name perms size modified)
             
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072

type InfoP a =  FilePath
             -> Permissions
             -> Maybe Integer
             -> UTCTime
             -> a
             
pathP :: InfoP FilePath             
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k
