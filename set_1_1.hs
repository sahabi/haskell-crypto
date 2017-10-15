{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Base16.Lazy as B16
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Internal as I
import Data.Bits
import Data.List
import Data.Ord
import Data.Tuple.Select
import Data.Char
import Data.Word
import qualified Codec.Binary.UTF8.String as U

boolToString :: Bool -> String
boolToString True = "TRUE"
boolToString False = "FALSE"

hexToBytes :: String -> B.ByteString
hexToBytes = (fst . B16.decode . C.pack) 
 
bytesToHex :: B.ByteString -> String
bytesToHex = (C.unpack . B16.encode)

bytesToBase64 :: B.ByteString -> String
bytesToBase64 = (C.unpack . B64.encode)

xorEncrypt :: B.ByteString -> B.ByteString -> B.ByteString 
xorEncrypt msg key = B.pack $ zipWith xor (B.unpack msg) (B.unpack key)  


mostCommon :: (Eq a , Ord a) => [a] -> a 
mostCommon st = fst ((sortBy (comparing sel2) $ charFreq st)!!0)

charFreq :: (Eq a , Ord a) => [a] -> [(a, Int)]
charFreq st = map (\a -> (head a, -1*length a)) $ group $ sort st


encoded_msg :: String
encoded_msg = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

makeKey :: Char -> String -> B.ByteString

makeKey key msg = B.replicate inputLength $ fromIntegral $ ord key where
             inputLength = B.length $ hexToBytes msg

findKey :: String -> B.ByteString -> Char 
findKey msg gs = chr $ fromEnum $ mostCommon $ B.unpack $ xorEncrypt (hexToBytes msg) gs

key = makeKey ' ' encoded_msg 
decoded_msg = decode encoded_msg
decode :: String -> B.ByteString
decode st =  xorEncrypt (hexToBytes st)  (makeKey (findKey st $ makeKey ' ' st) st)

main :: IO ()
main =
  putStrLn $ show $ decoded_msg   
