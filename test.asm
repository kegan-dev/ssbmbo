lis r11,0x8046
ori r11,r11,0xaf00
lis r12,0x8023
ori r12,r12,0x9e9c
lis r3,0x8043
lwz r4,0x2f74(r3)
cmpwi r4,0
beq 0x10
stw r11,0x2f74(r3)
mtctr r12
bctr
mtctr r11
mtlr r12
bctr
