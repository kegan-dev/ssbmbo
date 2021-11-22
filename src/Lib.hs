module Lib
    ( 
        pack,
        recomputeChecksums,
        unpack
    )
    where

import Data.Word
import Data.Bits
import Debug.Trace

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

unkArr = [
    0x00000026,
    0x000000FF,
    0x000000E8,
    0x000000EF,
    0x00000042,
    0x000000D6,
    0x00000001,
    0x00000054,
    0x00000014,
    0x000000A3,
    0x00000080,
    0x000000FD,
    0x0000006E
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

rotl :: Integer -> Integer -> Integer
rotl rx sh = if sh >= 32 then error "Shift must be between 0 and 31." else ((shiftL rx (fromInteger sh)) .&. 0xffffffff) .|. (shiftR rx (fromInteger ((32 - sh) .&. 31)))
        --rotl rx sh = if sh >= 32 then error "Shift must be between 0 and 31."
        --else rotateL rx (fromIntegral sh)

mask :: Integer -> Integer -> Integer
mask mb me =
    let
        x = shiftR 0xffffffff (fromInteger mb)
        y = (shiftL 0xffffffff (31 - fromInteger me))
    in
        if mb >= 32 || me >= 32 then error "Arguments must be between 0 and 31."
        else if mb <= me then x .&. y else x .|. y

rlwinm :: Integer -> Integer -> Integer -> Integer -> Integer
rlwinm rs sh mb me = (rotl rs sh) .&. (mask mb me)
rlwimi :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
rlwimi ra rs sh mb me =
    let
        m = mask mb me
        r = rotl rs sh
    in
        (r .&. m) .|. (ra .&. (complement m))

decode :: Word32 -> Word32 -> Word32
--decode prev cur | trace ("decode " ++ show prev ++ " " ++ show cur) False = undefined
decode prev cur =
    let
        prevInt = toInteger prev
        curInt = toInteger cur
        r5_1 = if prevInt == 0 then 0x92492493 else (complement 0x92492493) .&. 0xffffffff
        r5_2 = if prevInt == 0 then (shiftR (r5_1 * prevInt) 32) .&. 0xffffffff else (shiftR (complement (r5_1 * prevInt)) 32) .&. 0xffffffff
        r5_3 = (r5_2 + prevInt) .&. 0xff
        r5_4 = shiftR r5_3 2
        r6_1 = rlwinm r5_4 1 31 31
        r5_5 = r5_4 + r6_1
        r5_6 = (r5_5 * 7) .&. 0xffffffff
        r7_1 = (prevInt - r5_6) .&. 0xffffffff
        r4_1 = if r7_1 == 0 then (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm curInt 1 29 29) curInt 0 31 31) curInt 2 27 27) curInt 3 25 25) curInt 29 30 30) curInt 30 28 28) curInt 31 26 26) curInt 0 24 24) 0 24 31)
            else if r7_1 == 1 then (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm curInt 6 24 24) curInt 1 30 30) curInt 0 29 29) curInt 29 31 31) curInt 1 26 26) curInt 31 27 27) curInt 29 28 28) curInt 31 25 25) 0 24 31)
            else if r7_1 == 2 then (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm curInt 2 28 28) curInt 2 29 29) curInt 4 25 25) curInt 1 27 27) curInt 3 24 24) curInt 28 30 30) curInt 26 31 31) curInt 30 26 26) 0 24 31)
            else if r7_1 == 3 then (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm curInt 31 31 31) curInt 4 27 27) curInt 3 26 26) curInt 30 30 30) curInt 31 28 28) curInt 1 25 25) curInt 1 24 24) curInt 27 29 29) 0 24 31)
            else if r7_1 == 4 then (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm curInt 4 26 26) curInt 3 28 28) curInt 31 30 30) curInt 4 24 24) curInt 2 25 25) curInt 29 29 29) curInt 30 27 27) curInt 25 31 31) 0 24 31)
            else if r7_1 == 5 then (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm curInt 5 25 25) curInt 5 26 26) curInt 5 24 24) curInt 0 28 28) curInt 30 29 29) curInt 27 31 31) curInt 27 30 30) curInt 29 27 27) 0 24 31)
            else if r7_1 == 6 then (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm curInt 0 30 30) curInt 6 25 25) curInt 30 31 31) curInt 2 26 26) curInt 0 27 27) curInt 2 24 24) curInt 28 29 29) curInt 28 28 28) 0 24 31)
            else error "r7 is an invalid value!"
        r5_7 = 0x4ec4ec4f
        r5_8 = (shiftR (r5_7 * prevInt) 32) .&. 0xffffffff
        r5_9 = shiftR r5_8 2
        r6_2 = rlwinm r5_9 1 31 31
        r5_10 = r5_9 + r6_2
        r5_11 = r5_10 * 13
        r0_1 = (prevInt - r5_11) .&. 0xff
        r6_3 = rlwinm r0_1 2 0 29
        r0_2 = unkArr !! (fromInteger $ div r6_3 4)

        r4_2 = (xor r4_1 r0_2)
        r4_3 = (xor r4_2 prevInt) .&. 0xff
        r3_1 = r4_3 + 0
    in
        fromInteger $ r3_1 .&. 0xff

-- |Unpacks the card to raw.
unpack :: [Word8] -> [Word8]
unpack packed =
    let
        blockSize = getBlockSize packed
        blockRanges = map (\i -> (0x2050 + (0x2000 * i), 0x2050 + 0x1ff0 + (0x2000 * i))) [0..blockSize-2]
        decodeLoop :: Integer -> Word8 -> [Word8] -> [Word8] -> [Word8]
        decodeLoop i prev ps [] = ps
        decodeLoop i prev [] (u:us) = decodeLoop (i+1) u [u] us
        decodeLoop i prev (p:ps) (u:us) =
            if any (\(f,l) -> i >= f && i < l) blockRanges
            then decodeLoop (i+1) u ((fromIntegral $ decode (fromIntegral prev) (fromIntegral u)):p:ps) us
            else decodeLoop (i+1) u (u:p:ps) us
    in
        reverse $ decodeLoop 0 0 [] packed

-- |Encodes a bit correctly. See
-- https://github.com/dansalvato/melee-gci-compiler for the algorithm source.
encode :: Word32 -> Word32 -> Word32
encode prev cur =
    let
        prevInt = toInteger prev
        curInt = toInteger cur
        r5_1 = (shiftR (toInteger $ prevInt * 0x4ec4ec4f) 32) .&. 0xffffffff
        r0_1 = if prevInt == 0 then 0x92492493 else (complement 0x92492493) .&. (0xffffffff)
        r0_2 = if prevInt == 0 then (shiftR (r0_1 * prevInt) 32) .&. 0xffffffff else (shiftR (complement (r0_1 * prevInt)) 32) .&. 0xffffffff
        r3_1 = shiftR r5_1 2
        r5_2 = rlwinm r3_1 1 31 31
        r0_3 = (r0_2 + prevInt) .&. 0xff
        r3_2 = r3_1 + r5_2
        r0_4 = shiftR r0_3 2
        r5_3 = r3_2 * 13
        r3_3 = rlwinm r0_4 1 31 31
        r0_5 = r0_4 + r3_3
        r0_6 = r0_5 * 7
        r5_4 = (prevInt - r5_3) .&. 0xff
        r0_7 = (prevInt - r0_6) .&. 0xff
        r5_5 = rlwinm r5_4 2 0 29
        r5_6 = unkArr !! (fromInteger (div r5_5 4))
        r3_4 = xor prevInt curInt
        r3_5 = xor r3_4 r5_6
        r0_8 = if r0_7 > 6 then error "r0 should be no greater than 6" else rlwinm r0_7 2 0 29
    in
        if r0_8 == 0x0 then fromInteger $ (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm r3_5 3 27 27) r3_5 0 31 31) r3_5 31 30 30) r3_5 2 26 26) r3_5 30 29 29) r3_5 1 25 25) r3_5 29 28 28) r3_5 0 24 24) 0 24 31) .&. 0xffffffff
        else if r0_8 == 0x4 then fromInteger $ (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm r3_5 31 31 31) r3_5 3 28 28) r3_5 0 29 29) r3_5 3 25 25) r3_5 1 26 26) r3_5 31 27 27) r3_5 1 24 24) r3_5 26 30 30) 0 24 31) .&. 0xffffffff
        else if r0_8 == 0x8 then fromInteger $ (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm r3_5 4 26 26) r3_5 6 25 25) r3_5 30 31 31) r3_5 30 30 30) r3_5 31 28 28) r3_5 2 24 24) r3_5 28 29 29) r3_5 29 27 27) 0 24 31) .&. 0xffffffff
        else if r0_8 == 0xc then fromInteger $ (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm r3_5 2 28 28) r3_5 1 30 30) r3_5 5 24 24) r3_5 1 27 27) r3_5 28 31 31) r3_5 29 29 29) r3_5 31 26 26) r3_5 31 25 25) 0 24 31) .&. 0xffffffff
        else if r0_8 == 0x10 then fromInteger $ (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm r3_5 1 29 29) r3_5 7 24 24) r3_5 3 26 26) r3_5 29 31 31) r3_5 2 25 25) r3_5 28 30 30) r3_5 30 27 27) r3_5 28 28 28) 0 24 31) .&. 0xffffffff
        else if r0_8 == 0x14 then fromInteger $ (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm r3_5 5 25 25) r3_5 5 26 26) r3_5 2 27 27) r3_5 0 28 28) r3_5 3 24 24) r3_5 27 31 31) r3_5 27 30 30) r3_5 27 29 29) 0 24 31) .&. 0xffffffff
        else if r0_8 == 0x18 then fromInteger $ (rlwinm (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwimi (rlwinm r3_5 0 30 30) r3_5 2 29 29) r3_5 4 25 25) r3_5 4 24 24) r3_5 0 27 27) r3_5 30 28 28) r3_5 26 31 31) r3_5 30 26 26) 0 24 31) .&. 0xffffffff
        else fromInteger $ r3_5 .&. 0xffffffff

getBlockSize :: [Word8] -> Integer
getBlockSize unpacked = 
    let
        blockSizeByte1 = unpacked !! 0x38
        blockSizeByte2 = unpacked !! 0x39
        blockSize = (shiftL (toInteger blockSizeByte1) 8) + (toInteger blockSizeByte2)
    in
        blockSize

-- |Packs a raw card to gci format.
pack :: [Word8] -> [Word8]
pack unpacked =
    let
        blockSize = getBlockSize unpacked
        blockRanges = map (\i -> (0x2050 + (0x2000 * i), 0x2050 + 0x1ff0 + (0x2000 * i))) [0..blockSize-2]
        encodeLoop :: Integer -> [Word8] -> [Word8] -> [Word8]
        encodeLoop i ps [] = ps
        encodeLoop i [] (u:us) = encodeLoop (i+1) [u] us
        encodeLoop i (p:ps) (u:us) =
            if any (\(f,l) -> i >= f && i < l) blockRanges
            then encodeLoop (i+1) ((fromIntegral $ encode (fromIntegral p) (fromIntegral u)):p:ps) us
            else encodeLoop (i+1) (u:p:ps) us
    in
        reverse $ encodeLoop 0 [] unpacked

-- |Sets the correct checksums in the raw memory card.
recomputeChecksums :: [Word8] -> [Word8]
recomputeChecksums badBytes =
    let
        blockSize = getBlockSize badBytes
        blockData = map (\i -> take 0x1ff0 . drop (0x2050 + 0x2000 * i) $ badBytes) [0..(fromInteger blockSize)-2]
        blockChecksums = map (\b -> getChecksum 0 [0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF, 0xFE, 0xDC, 0xBA, 0x98, 0x76, 0x54, 0x32, 0x10] b) blockData
        
        finalizeChecksum :: Int -> [Word8] -> [Word8]
        finalizeChecksum i checksum = 
            let
                x = checksum !! (i - 1)
                y = checksum !! i
                (newChecksum1, _:newChecksum2) = splitAt i checksum
                newChecksum = if x == y then newChecksum1 ++ [xor y 0x00ff] ++ newChecksum2 else checksum
            in
                if i >= 0xf
                then checksum
                else finalizeChecksum (i + 1) newChecksum

        getChecksum :: Int -> [Word8] -> [Word8] -> [Word8]
        getChecksum i checksum [] = finalizeChecksum 1 checksum
        getChecksum i checksum (b:bs) =
            let
                arrPos = i
                curArr = checksum !! (arrPos .&. 0xf)
                (newChecksum1, _:newChecksum2) = splitAt (arrPos .&. 0xf) checksum
                newChecksum = newChecksum1 ++ [(b + curArr) .&. 0xff] ++ newChecksum2
            in
                if length newChecksum > 16 then error "Too long checksum" else getChecksum (i + 1) newChecksum bs

        blocksAndChecksums = zip blockChecksums blockData
    in
        foldl (\acc (c, b) -> acc ++ c ++ b) (take 0x2040 badBytes) blocksAndChecksums