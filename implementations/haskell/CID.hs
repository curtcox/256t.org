module CID
  ( cidsDir
  , examplesDir
  , computeCID
  ) where

import Crypto.Hash (Digest, SHA512, hash)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8)
import System.FilePath ((</>))

baseDir :: FilePath
baseDir = ".." </> ".."

cidsDir :: FilePath
cidsDir = baseDir </> "cids"

examplesDir :: FilePath
examplesDir = baseDir </> "examples"

toBase64Url :: BS.ByteString -> String
toBase64Url bytes = BC.unpack (BAE.convertToBase BAE.Base64URLUnpadded bytes)

encodeLength :: Int -> String
encodeLength len =
  let byteAt shift = fromIntegral ((len `shiftR` shift) .&. 0xFF) :: Word8
      bytes = BS.pack $ map byteAt [40, 32, 24, 16, 8, 0]
   in toBase64Url bytes

computeCID :: BS.ByteString -> String
computeCID content =
  let prefix = encodeLength (BS.length content)
      suffix
        | BS.length content <= 64 = toBase64Url content
        | otherwise =
            let digest :: Digest SHA512
                digest = hash content
             in toBase64Url (BA.convert digest :: BS.ByteString)
   in prefix ++ suffix
