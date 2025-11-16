module Main where

import CID (cidsDir, computeCID, examplesDir)
import Control.Monad (forM_, when)
import Data.List (sort)
import qualified Data.ByteString as BS
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  createDirectoryIfMissing True cidsDir
  entries <- sort <$> listDirectory examplesDir
  forM_ entries $ \name -> do
    let path = examplesDir </> name
    isFile <- doesFileExist path
    when isFile $ do
      content <- BS.readFile path
      let cid = computeCID content
          destination = cidsDir </> cid
      BS.writeFile destination content
      putStrLn $ "Wrote " ++ cid ++ " from " ++ name
