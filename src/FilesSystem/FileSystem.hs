module FileSystem where

import Data.Maybe (isJust, isNothing)
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.IO
import Control.Exception (IOException, handle, bracket)
import qualified Data.Map.Strict as Map 
import Data.List (sortBy)
import Crypto.Hash.SHA1 (hashlazy)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Text.Printf (printf)
import qualified Data.Foldable as F

type FileHash = Strict.ByteString
type FileSize = Integer

data IdentitySet = SingleFileIdentitySet FilePath
                 | MultipleFilesIdentitySet FileHash [FilePath]
                   deriving (Show)
                    

data State = State {
               paths :: [FilePath],
               identityMap :: Map.Map FileSize [IdentitySet],
               counter :: Integer
             } deriving (Show)
             
initialState :: State
initialState = State {
                 paths = [],
                 identityMap = Map.empty,
                 counter = 0
               }
             
addPath :: FilePath -> FileSize-> [IdentitySet] -> State -> State
addPath path size idSets state =
    state {
       paths         = path : (paths state),
       identityMap   = Map.insert size idSets (identityMap state),
       counter       = (counter state) + 1
     }
  
    
addPathIO :: FilePath -> FileSize -> IO State -> IO State
addPathIO path size state = do
    state' <- state
    case Map.lookup size (identityMap state') of
      Nothing -> return $ addPath path size [SingleFileIdentitySet path] state'
      Just idSets -> do 
                      newIdSets <- addPathToIdSets idSets path
                      return $ addPath path size newIdSets state'

addPathToIdSets ::[IdentitySet] -> FilePath -> IO [IdentitySet] 
addPathToIdSets [] path  =  return [SingleFileIdentitySet path]
addPathToIdSets ((SingleFileIdentitySet path') : idsets) path = do 
    hash <-hashFile path
    hash' <- hashFile path'
    let hash1 = getJust hash
    let hash2 = getJust hash'
    if hash1 == hash2 
        then return $ (MultipleFilesIdentitySet hash1 [path', path]) : idsets
        else do idset' <- addPathToIdSets idsets path
                return $ SingleFileIdentitySet path' : idset'
     
addPathToIdSets ((MultipleFilesIdentitySet hash paths) : idsets) path = do
    hash' <- hashFile path
    let hash1 = getJust hash'
    if hash == hash1
        then return $ (MultipleFilesIdentitySet hash1 (path : paths)) : idsets
        else do idset' <- addPathToIdSets idsets path
                return $ (MultipleFilesIdentitySet hash paths) : idset'       
        


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
              if isJust fileSize
                  then addPathIO path (getJust fileSize) state
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

data FileDirectory a = FileDirectory [FilePath] (FilePath -> IO [FilePath]) (FilePath -> IO Bool)

instance Foldable FileDirectory where
  foldMap f (FileDirectory [] _ _) = mempty
  foldMap f (FileDirectory (path :: paths) getDirectoryContents isDirectory) = 
      f path `mappend` (foldMap f (FileDirectory path' getDirectoryContents isDirectory)
      where path' = do
                      isDir <- isDirectory path
                      if isDir 
                         then 
                         else return 
                         
                 

testPath :: FilePath
--testPath = "C:\\Users\\D025630\\Documents"
--testPath = "C:\\HANA_Studio"
--testPath = "D:\\mp3"
testPath = "D:\\Benutzer\\Familie Fecht\\Eigene Dokumente"

       
main :: IO ()
main = do 
         state <- getRecursiveContents testPath (return initialState)
         putStrLn $ show (length (paths state))
         

toHex :: Strict.ByteString -> String
toHex bytes = Strict.unpack bytes >>= printf "%02x"


