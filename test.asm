.macro branchl reg, address
lis \reg, \address @h
ori \reg,\reg,\address @l
mtctr \reg
bctrl
.endm

.set BKP_FREE_SPACE_OFFSET, 0x38 # This is where the free space in our stack starts

.macro backup space=0x78
mflr r0
stw r0, 0x4(r1)
stwu r1,-(BKP_FREE_SPACE_OFFSET + \space)(r1)	# make space for 12 registers
stmw r20,0x8(r1)
.endm

.macro restore space=0x78
lmw r20,0x8(r1)
lwz r0, (BKP_FREE_SPACE_OFFSET + 0x4 + \space)(r1)
addi r1,r1,BKP_FREE_SPACE_OFFSET + \space	# release the space
mtlr r0
.endm

lis r11,0x8046
ori r11,r11,0xaf00
lis r12,0x8023
ori r12,r12,0x9e9c
lis r3,0x8043
lwz r4,0x2f74(r3)
cmpwi r4,0			#is a memcard save in progress?
beq NOTMEMCARD
stw r11,0x2f74(r3)	#if so, run loader2 when it's done and return
b LOADER2
NOTMEMCARD:
mtctr r11			#if not, run loader2 now and set LR to return
mtlr r12

LOADER2:
mflr r0
stw r0,0x4(sp)
stwu sp,-0x20(sp)
stw r31,0x1c(sp)
stw r30,0x18(sp)
stw r29,0x14(sp)
stw r28,0x10(sp)

lis r11,0x8026 # r11 contains address to replace
lis r4,0x4bd9
ori r4,r4,0xc078 # r4 contains branch instruction
stw r4,0x62D0(r11) # replace instruction at 0x802662D0 with branch

bl INDICATORCODE
mflr r4 # Start of copy
bl ENDINDICATORCODE
mflr r5
subi r5,r5,4 # End of copy
sub r5,r5,r4	 #size
lis r3,0x8000
ori r3,r3,0x2348 #destination

#copy data
subi r4,r4,4
subi r6,r3,4
addi r5,r5,4
b 0xc
lwzu r0,0x4(r4)
stwu r0,0x4(r6)
subic. r5,r5,4
bne+ -0xc

# DONE with our code ============================================= 

#Reload game data
lis r12,0x8001
ori r12,r12,0xcbbc
mtctr r12
bctrl

#Clear cache
lis r3,0x8000
lis r4,0x3c
lis r12,0x8032
ori r12,r12,0x8f50
mtctr r12
bctrl

li r3,6
lis r12,0x801a
ori r12,r12,0x428c
mtctr r12
bctrl

li r0,5
sth r0,-0x4ad8(r13)
lwz r3,-0x4f80(r13)
lwz r4,0x8(r3)
li r30,0
stb r30,0(r4)

END:
lis r12,0x801a
ori r12,r12,0x4b60
mtctr r12
bctrl

lwz r0,0x24(sp)
lwz r31,0x1c(sp)
lwz r30,0x18(sp)
lwz r29,0x14(sp)
lwz r28,0x10(sp)
addi sp,sp,0x20
mtlr r0
blr

INDICATORCODE:
blrl
.set REG_TextGObj,31
.set REG_TextProperties,30


NTSC102:
	.set	Injection,0x802662D0
	.set	Text_CreateTextGObj,0x803a6754
	.set	Text_InitializeSubtext,0x803a6b98
	.set	Text_UpdateSubtextSize,0x803a7548
backup

#GET PROPERTIES TABLE
	bl TEXTPROPERTIES
	mflr REG_TextProperties

########################
## Create Text Object ##
########################

#CREATE TEXT OBJECT, RETURN POINTER TO STRUCT IN r3
	li r3,0
	li r4,0
	branchl r14,Text_CreateTextGObj

#BACKUP STRUCT POINTER
	mr REG_TextGObj,r3

#SET TEXT SPACING TO TIGHT
	li r4,0x1
	stb r4,0x49(REG_TextGObj)

#SET TEXT TO CENTER AROUND X LOCATION
	li r4,0x1
	stb r4,0x4A(REG_TextGObj)

#Scale Canvas Down
	lfs f1,0xC(REG_TextProperties)
	stfs f1,0x24(REG_TextGObj)
	stfs f1,0x28(REG_TextGObj)

####################################
## INITIALIZE PROPERTIES AND TEXT ##
####################################

#Initialize Line of Text
	mr r3,REG_TextGObj       #struct pointer
	bl 	TEXT
	mflr 	r4		#pointer to ASCII
	lfs f1,0x0(REG_TextProperties) #X offset of REG_TextGObj
	lfs f2,0x4(REG_TextProperties) #Y offset of REG_TextGObj
	branchl r14,Text_InitializeSubtext

#Set Size/Scaling
  mr  r4,r3
  mr	r3,REG_TextGObj
	lfs   f1,0x8(REG_TextProperties) #get REG_TextGObj scaling value from table
	lfs   f2,0x8(REG_TextProperties) #get REG_TextGObj scaling value from table
  branchl	r12,Text_UpdateSubtextSize

b end


#**************************************************#
TEXTPROPERTIES:
blrl
.float 38			#x offset
.float -275		#y offset
.float 0.45		#REG_TextGObj scaling
.float 0.1		#canvas scaling


TEXT:
blrl
.string "UR HACKED"
.align 2

#**************************************************#
end:
restore

addi	r4, r24, 0
b 0x263EC0
ENDINDICATORCODE:
blrl