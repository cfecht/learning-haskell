import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
          inh <-openFile "input.txt" ReadMode
          outh <- openFile "output.txt" WriteMode
          inpStr <- hGetContents inh
          let result = processData inpStr
          hPutStr outh result
          close inh
          close outh
          
processData :: String -> String
