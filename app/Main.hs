module Main where

import System.Environment
import qualified Data.ByteString as BS
import qualified Lib as SSBMBO

getFlag :: String -> [String] -> IO String
getFlag f [] = fail ("Could not find flag: " ++ f)
getFlag f (a1:a2:as) | a1 == f = return a2
getFlag f (a1:a2:as) | a1 /= f = getFlag f (a2:as)
getFlag f (a1:[]) = fail ("No flag data for flag: " ++ f)

main :: IO ()
main = do
    args <- getArgs
    inputAsmFile <- getFlag "-iasm" args
    inputGciFile <- getFlag "-igci" args
    outputGciFile <- getFlag "-ogci" args
    putStrLn $ "Input asm file: " ++ inputAsmFile
    putStrLn $ "Output gci file: " ++ outputGciFile
    fContents <- BS.readFile (inputGciFile)
    --BS.putStr (BS.pack . SSBMBO.pack . BS.unpack $ fContents)
    BS.writeFile outputGciFile (BS.pack . SSBMBO.pack . SSBMBO.recomputeChecksums . SSBMBO.setupACE . SSBMBO.unpack . BS.unpack $ fContents)
