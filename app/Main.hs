module Main where

import System.Environment
import System.Directory
import System.Process
import System.IO
import qualified Data.ByteString as BS
import qualified Lib as SSBMBO
import Data.Word (Word8)

getFlag :: String -> [String] -> IO String
getFlag f [] = fail ("Could not find flag: " ++ f)
getFlag f (a1:a2:as) | a1 == f = return a2
getFlag f (a1:a2:as) | a1 /= f = getFlag f (a2:as)
getFlag f (a1:[]) = fail ("No flag data for flag: " ++ f)


compile :: IO [Word8]
compile = do
  (_, Just hout, _, _) <- createProcess (shell "./bin/linux_x86_64/powerpc-eabi-as -W -mregnames -mgekko -o ./src1.o ./test.asm") {std_out = CreatePipe}
  out <- hGetContents hout
  putStrLn out
  (_, Just hout, _, _) <- createProcess (shell "./bin/linux_x86_64/powerpc-eabi-ld -Ttext 0x80000000 -o ./src2.o ./src1.o") {std_out = CreatePipe}
  out <- hGetContents hout
  putStrLn out
  (_, Just hout, _, _) <- createProcess (shell "./bin/linux_x86_64/powerpc-eabi-objcopy -O binary ./src2.o code.bin") {std_out = CreatePipe}
  out <- hGetContents hout
  putStrLn out
  removeFile "src1.o"
  removeFile "src2.o"
  BS.unpack <$> BS.readFile "code.bin"

main :: IO ()
main = do
    args <- getArgs
    inputAsmFile <- getFlag "-iasm" args
    inputGciFile <- getFlag "-igci" args
    outputGciFile <- getFlag "-ogci" args
    putStrLn $ "Input asm file: " ++ inputAsmFile
    putStrLn $ "Output gci file: " ++ outputGciFile
    bin <- compile
    removeFile "code.bin"
    BS.putStrLn (BS.pack bin)
    fContents <- BS.readFile (inputGciFile)
    --BS.putStr (BS.pack . SSBMBO.pack . BS.unpack $ fContents)
    BS.writeFile outputGciFile (BS.pack . SSBMBO.pack . SSBMBO.recomputeChecksums . SSBMBO.setupACE . SSBMBO.unpack . BS.unpack $ fContents)
