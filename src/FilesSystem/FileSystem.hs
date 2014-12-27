module FileSystem where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO
import Control.Exception (IOException, handle, bracket)
import qualified Data.Map as Map 
import Data.List (sortBy)

data FileInfo = FileInfo { paths :: [FilePath] }

getRecursiveContents :: FilePath -> IO [FilePath] -> IO [FilePath]

getRecursiveContents topdir paths = do
  names  <- handle returnEmptyList $ getDirectoryContents topdir
  f names paths
  where 
      f [] paths_ = paths_
      f (p:ps) paths_ = do 
                           f ps (addDirectoryEntry p paths_)
      addDirectoryEntry "."   ps = ps
      addDirectoryEntry ".."  ps = ps 
      addDirectoryEntry entry ps = do
           let currentPath = topdir </> entry
           isDirectory <- doesDirectoryExist currentPath
           if isDirectory
               then getRecursiveContents currentPath ps
               else do 
                      ps_ <- ps
                      return (currentPath : ps_)
  
returnEmptyList :: IOException -> IO [FilePath]
returnEmptyList _ = return []
 
getFileSize path = handle doNothing $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)  

doNothing :: IOException -> IO (Maybe Integer)
doNothing _ = return Nothing    

getFileSizes :: FilePath -> IO [(FilePath, Integer)]
getFileSizes path = do
    paths <- getRecursiveContents path (return [])
    f paths
    where f [] = return []
          f (p:ps) = do 
                       size <- getFileSize p
                       sizes <- f ps
                       case size of
                         Nothing -> return sizes
                         Just s  -> return ((p, s) : sizes)
    

testPath :: FilePath
testPath = "C:\\Users\\D025630\\Documents"
-- testPath = "C:\\HANA_Studio"

createFileSizeHistogram :: [(FilePath, Integer)] -> Map.Map Integer Integer
createFileSizeHistogram [] = Map.empty
createFileSizeHistogram ((p,s) : r) = 
    case Map.lookup s m of
      Nothing -> Map.insert s 1 m
      Just v' -> Map.insert s (v'+1) m
    where m = createFileSizeHistogram r
    
orderPairBySecondDesc :: (Integer, Integer) -> (Integer, Integer) -> Ordering
orderPairBySecondDesc (_, b1) (_, b2) 
   | b1 < b2  = GT
   | b1 == b2 = EQ
   | b1 > b2  = LT
         
main :: IO ()
main = do 
        sizes <- getFileSizes testPath
        let histo = createFileSizeHistogram sizes
            al    = Map.toList histo
        putStrLn (show (sortBy orderPairBySecondDesc al))