module Main where

import System.Environment
import qualified Data.ByteString as BS
import qualified Lib as SSBMBO

main :: IO ()
main = do
    args <- getArgs
    fContents <- BS.readFile (head args)
    --BS.putStr (BS.pack . SSBMBO.pack . BS.unpack $ fContents)
    BS.writeFile (head . tail $ args) (BS.pack . SSBMBO.pack . BS.unpack $ fContents)
