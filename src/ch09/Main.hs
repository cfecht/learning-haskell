import RecursiveContents( getRecursiveContents )
import System.IO
import Control.Exception( handle, bracket )

tmpPath :: FilePath
tmpPath = "D:\\Benutzer\\Familie Fecht\\Eigene Bilder"


main :: IO ()
main = do
         files <- (getRecursiveContents tmpPath)
         fileSizes <- mapM zipFileSize files
         mapM_ putFileSize fileSizes
         return ()
         where 
             zipFileSize path  = do
                 size <- getFileSize path
                 return (path, size)
             putFileSize (path, size) = do
                 putStrLn $ path ++ " : " ++ show size
         
         