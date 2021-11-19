module Lib
    ( 
        someFunc
    )
    where

import Data.Word
import Data.Bits

blockList = [           --GCI block offset list
        0x02060,        --Block 0
        0x04060,        --Block 1
        0x06060,        --Block 2
        0x08060,        --Block 3
        0x0a060,        --Block 4
        0x0c060,        --Block 5
        0x0e060,        --Block 6
        0x10060,        --Block 7
        0x12060,        --Block 8
        0x14060         --Block 9
        ]

memList = [             --Melee start address for each GCI block
        0x00000000,     --Block 0 (not in memory)
        0x8045d6b8,     --Block 1
        0x8045f5e4,     --Block 2
        0x80461510,     --Block 3
        0x8046343c,     --Block 4
        0x80465368,     --Block 5
        0x80467294,     --Block 6
        0x804691c0,     --Block 7
        0x00000000,     --Block 8 (not in memory)
        0x8045bf28      --Block 9
        ]

blockStart = blockList !! 0
blockEnd = blockList !! 9 + blockSize !! 9
memStart = memList !! 9 --Memory starts with block 9 for some reason
memEnd = memList !! 7 + blockSize !! 7

blockSize = [0, 0x1f2c, 0x1f2c, 0x1f2c, 0x1f2c, 0x1f2c, 0x1f2c, 0x1f2c, 0, 0x1790]

type CardLocation = (Integer, Integer)

getCardLocationBlock :: CardLocation -> Integer
getCardLocationBlock (b, _) = b

getCardLocationOffset :: CardLocation -> Integer
getCardLocationOffset (_, o) = o

-- |Takes a memory location and returns the block number and offset
memAddressToCardLocationPair :: Integer -> CardLocation
memAddressToCardLocationPair memAddress = 
    let
        filterFunc (_, bs, ba) = (memAddress - ba) < bs && (memAddress - ba >= 0 )
        (ix, matchedBlockSize, matchedBlockAddress) = head $ filter filterFunc $ zip3 [0..] blockSize memList
    in
        (ix, memAddress - matchedBlockAddress)

-- |Takes a memory location and returns the corresponding card location.
memAddressToCardLocation :: Integer -> Integer
memAddressToCardLocation memAddress =
    let 
        (blockNum, offset) = memAddressToCardLocationPair memAddress
    in
        (blockList !! fromInteger blockNum) + offset

-- |Takes a card location and returns a memory location.
cardLocationToMemAddress :: Integer -> Integer
cardLocationToMemAddress cardAddress =
    let
        filterFunc (bStart, bSize, _) = (cardAddress - bStart < bSize) && (cardAddress - bStart >= 0)
        (bStart, bSize, mAddress) = head $ filter filterFunc $ zip3 blockList blockSize memList
    in
        mAddress + (cardAddress - bStart)

-- |Takes a memory location and data and returns where to write the data.
dataToCardLocations :: Integer -> [Word8] -> [(Integer, Word8)]
dataToCardLocations memAddress [] = []
dataToCardLocations memAddress bytes = 
    let
        offsetsBytes = zip [0..] bytes
        memAddressBytes = map (\(ix, b) -> (memAddress + ix, b)) offsetsBytes
        cardAddressBytes = map (\(addr, b) -> (memAddressToCardLocation addr, b)) memAddressBytes
    in
        cardAddressBytes

-- |Unpacks the card to raw.
unpack :: [Word8] -> [Word8]
unpack packed = undefined

encode :: Word8 -> Word8 -> Word8
encode prev cur = undefined

-- |Packs a raw card
pack :: [Word8] -> [Word8]
pack unpacked =
    let
        prevByteOffset = 0x204f
        baseOffset = 0x2050
        dataSize = 0x1ff0
        blockSizeByte1 = unpacked !! 0x38
        blockSizeByte2 = unpacked !! 0x39
        blockSize = (shiftL (toInteger blockSizeByte1) 8) + (toInteger blockSizeByte2) -- big endian
        prevByteOffsets = [(prevByteOffset + 0x2000 * x, baseOffset + 0x2000 * x) | x <- [0..blockSize-2]] --Next is [(Integer, Word8)] to map to where the modified bits will go.
        foldFunc convertedByteBlocks (prevOff, baseOff) = 
            let
                byteIxToConvert = [baseOff..baseOff+dataSize-1]
                bytesToConvert = map ((unpacked !!) . fromInteger) byteIxToConvert
                initialPrevRes = unpacked !! (fromInteger prevOff)
                innerFoldFunc (prevRes, packedBytes) curByte = (encode prevRes curByte, encode prevRes curByte : packedBytes)
                (_, convertedBytes) = foldl innerFoldFunc (initialPrevRes, []) bytesToConvert
            in
                (reverse convertedBytes:convertedByteBlocks)
        allConvertedByteBlocks = foldl foldFunc [] prevByteOffsets
    in
        undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
