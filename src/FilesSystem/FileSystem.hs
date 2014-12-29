module FileSystem where

import Data.Maybe (isJust, isNothing)
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO
import Control.Exception (IOException, handle, bracket)
import qualified Data.Map as Map 
import Data.List (sortBy)
import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Text.Printf (printf)

type FileHash = Strict.ByteString
type FileSize = Integer

data IdenticalFiles = IdenticalFiles FileSize FileHash [FilePath]

insert :: FilePath -> FileSize -> FileHash -> [IdenticalFiles] -> [IdenticalFiles]
insert path size hash [] = [IdenticalFiles size hash [path]]
insert path size hash ((IdenticalFiles size' hash' paths) : ids) = 
    if size == size' && hash == hash'
        then (IdenticalFiles size hash (path : paths)) : ids
        else (IdenticalFiles size' hash' paths) : insert path size hash ids

data State = State {
               paths :: [FilePath],
               numberOfFiles :: Integer,
               equalSizedFiles :: Map.Map Integer [FilePath]
             } deriving (Show)
             
initialState :: State
initialState = State {
                 paths = [],
                 numberOfFiles = 0,
                 equalSizedFiles = Map.empty
               }
             
addPath :: FilePath -> FileSize -> FileHash-> State -> State
addPath path size hash state =
    State {
       paths = path : (paths state)
     , numberOfFiles = (numberOfFiles state) + 1
     , equalSizedFiles = Map.insertWith (++) size [path] (equalSizedFiles state)
    }

foldM' :: Monad m => [a] -> (a -> m b -> m b) -> m b -> m b
foldM' [] f = id
foldM' (x : xs) f = \s -> foldM' xs f (f x s)

getJust :: Maybe a -> a
getJust (Just v) = v

addDirectoryEntry :: FilePath -> IO State -> IO State
addDirectoryEntry path state = do
    isDirectory <- doesDirectoryExist path
    if isDirectory
        then getRecursiveContents path state
        else do 
              fileSize <- getFileSize path
              hash <- hashFile path  
              if isJust fileSize && isJust hash
                  then fmap (addPath path (getJust fileSize) (getJust hash)) state
                  else state
         
                  
getRecursiveContents :: FilePath -> IO State -> IO State
getRecursiveContents path state = do
  names  <- handle returnEmptyList $ getDirectoryContents path
  let paths = map ((</>) path) (filter (`notElem` [".", ".."]) names)
  foldM' paths addDirectoryEntry state
   
returnEmptyList :: IOException -> IO [FilePath]
returnEmptyList _ = return []
 
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle doNothing $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)  

   
hashFile :: FilePath -> IO (Maybe Strict.ByteString)
hashFile path = handle doNothing $ (fmap (Just . hashlazy) . Lazy.readFile ) path

doNothing :: IOException -> IO (Maybe a)
doNothing e = do
    putStrLn $ show e
    return Nothing    


testPath :: FilePath
testPath = "C:\\Users\\D025630\\Documents"
--testPath = "C:\\HANA_Studio"

       
main :: IO ()
main = do 
         state <- getRecursiveContents testPath (return initialState)
         putStrLn $ show (numberOfFiles state)
         

toHex :: Strict.ByteString -> String
toHex bytes = Strict.unpack bytes >>= printf "%02x"


