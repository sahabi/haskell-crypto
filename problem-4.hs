import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString.Lazy as B

import qualified Crypto as C

getStrings :: FilePath -> IO [String]
getStrings fname = do
  body <- readFile fname
  return (lines body)

curFindKey = C.findKey2 '_'

main :: IO ()
main = do
    input <- getStrings "4.txt"
    let result = map C.decode input
    putStrLn $ show $ result 
