# ssbmbo

Tool to inject code into memory card. This will run the input assembly once the
exploit is used.

Assemblers 

Helpful assembler information http://wiibrew.org/wiki/Assembler_Tutorial

The idea:
```
replace memory location 802662D0 with branch instruction to the block starting at 0x80002348 and ending at 0x80002408 (our code)
move our code somewhere we want to branch to.

The indicator code to branch to. Need to add branch to 0x802662D4 at end instead of 0s (maybe 48263ED0)
7C0802A6 90010004
9421FF50 BE810008
48000089 7FC802A6
38600000 38800000
3D80803A 618C6754
7D8903A6 4E800421
7C7F1B78 38800001
989F0049 38800001
989F004A C03E000C
D03F0024 D03F0028
7FE3FB78 48000059
7C8802A6 C03E0000
C05E0004 3D80803A
618C6B98 7D8903A6
4E800421 7C641B78
7FE3FB78 C03E0008
C05E0008 3D80803A
618C7548 7D8903A6
4E800421 48000024
4E800021 42180000
C3898000 3EE66666
3DCCCCCD 4E800021
55434620 302E3800 # text string that loads
BA810008 800100B4
382100B0 7C0803A6
38980000 00000000
```

Compiling code in devcontainer. Gives raw machine code in a binary file
corresponding to our assembly.

```
./bin/linux_x86_64/powerpc-eabi-as -W -mregnames -mgekko -o ./src2.o ./test.asm
./bin/linux_x86_64/powerpc-eabi-ld -Ttext 0x80000000 -o ./src2.o ./src1.o
./bin/linux_x86_64/powerpc-eabi-objcopy -O binary ./src2.o code.bin
```
