module Main where

import CID (cidsDir, computeCID)
import Control.Monad (filterM, forM, unless)
import Data.List (sort)
import qualified Data.ByteString as BS
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))

checkFile :: FilePath -> IO (Maybe String)
checkFile name = do
  let path = cidsDir </> name
  content <- BS.readFile path
  let expected = computeCID content
  if expected == name
    then pure Nothing
    else pure $ Just (name ++ " should be " ++ expected)

main :: IO ()
main = do
  exists <- doesDirectoryExist cidsDir
  unless exists $ do
    putStrLn "cids directory not found."
    exitFailure

  entries <- sort <$> listDirectory cidsDir
  files <- filterM (doesFileExist . (cidsDir </>)) entries
  results <- forM files checkFile
  let mismatches = [msg | Just msg <- results]
      count = length files

  if null mismatches
    then putStrLn $ "All " ++ show count ++ " CID files match their contents."
    else do
      putStrLn "Found CID mismatches:"
      mapM_ putStrLn $ map ("- " ++) mismatches
      exitFailure
