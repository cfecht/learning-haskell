import System.IO
import Data.Char(toUpper)

main = do
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       hPutStrLn outh (map toUpper inpStr)
       hClose inh
       hClose outh