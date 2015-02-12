#### GDB #########################
# ft32-elf-objdump -D lcvm.bin > disassembly.txt
# FT900Prog.exe -f lcvm.bin -o 
# python "C:\FT90x Toolchain\programmer\gdb_bridge.py" live
# 
# open another shell
# ft32-elf-gdb lcvm.bin
# target remote localhost:9998
#
# set disassemble-next-line on
# break *0x## addr
# info registers (all registers)
# info registers $r0





        .equ    PM_UNLOCK,      0x1fc80
        .equ    PM_ADDR,        0x1fc84
        .equ    PM_DATA,        0x1fc88

        .section        .text
        .equ    DSTACK_TOP,     0xfcfc
.global _main
_main:

        jmp     0x3fffc
        jmp     0 # ft900_watchdog
        jmp     interrupt_0
        jmp     interrupt_1
        jmp     interrupt_2
        jmp     interrupt_3
        jmp     interrupt_4
        jmp     interrupt_5
        jmp     interrupt_6
        jmp     interrupt_7
        jmp     interrupt_8
        jmp     interrupt_9
        jmp     interrupt_10
        jmp     interrupt_11
        jmp     interrupt_12
        jmp     interrupt_13
        jmp     interrupt_14
        jmp     interrupt_15
        jmp     interrupt_16
        jmp     interrupt_17
        jmp     interrupt_18
        jmp     interrupt_19
        jmp     interrupt_20
        jmp     interrupt_21
        jmp     interrupt_22
        jmp     interrupt_23
        jmp     interrupt_24
        jmp     interrupt_25
        jmp     interrupt_26
        jmp     interrupt_27
        jmp     interrupt_28
        jmp     interrupt_29
        jmp     interrupt_30
        jmp     interrupt_31
        jmp     0x3fff8

        jmp     codestart

        # Macro to construct the interrupt stub code.
        # it just saves r0, loads r0 with the int vector
        # and branches to interrupt_common.

        .macro  inth i=0
interrupt_\i:
        push    $r0     
        ldk     $r0, nullvector
        jmp     interrupt_common
        .endm

        inth    0
        inth    1
        inth    2
        inth    3
        inth    4
        inth    5
        inth    6
        inth    7
        inth    8
        inth    9
        inth    10
        inth    11
        inth    12
        inth    13
        inth    14
        inth    15
        inth    16
        inth    17
        inth    18
        inth    19
        inth    20
        inth    21
        inth    22
        inth    23
        inth    24
        inth    25
        inth    26
        inth    27
        inth    28
        inth    29
        inth    30
        inth    31
        inth    32

        # On entry: r0, already saved, holds the handler function
interrupt_common:
        push    $r1     # {
        push    $r2     # {
        push    $r3     # {
        push    $r4     # {
        push    $r5     # {
        push    $r6     # {
        push    $r7     # {
        push    $r8     # {
        push    $r9     # {
        push    $r10    # {
        push    $r11    # {
        push    $r12    # {
        push    $r13    # {
        push    $r14    # {
        push    $r15    # {

#        push    dsp     # {
        push    $cc     # {

        calli   $r0

        pop     $cc     # }
#        pop     dsp     # }

        pop     $r15    # }
        pop     $r14    # }
        pop     $r13    # }
        pop     $r12    # }
        pop     $r11    # }
        pop     $r10    # }
        pop     $r9     # }
        pop     $r8     # }
        pop     $r7     # }
        pop     $r6     # }
        pop     $r5     # }
        pop     $r4     # }
        pop     $r3     # }
        pop     $r2     # }
        pop     $r1     # }
        pop     $r0     # } matching push in interrupt_0-31 above
        reti

        # Null function for unassigned interrupt to point at
nullvector:
        return

        .equ    sys_regclkcfg    , 0x10008 
        .equ    sys_regmsc0cfg_b2, 0x1001a 
        .equ    sys_regmsc0cfg_b3, 0x1001b 

        .equ    sys_regpad48     , 0x1004c 
        .equ    sys_regpad49     , 0x1004d 
        .equ    sys_regpad50     , 0x1004e 
        .equ    sys_regpad51     , 0x1004f 
        .equ    sys_regpad52     , 0x10050 
        .equ    sys_regpad53     , 0x10051 
        .equ    sys_regpad54     , 0x10052 
        .equ    sys_regpad55     , 0x10053 

        .equ    uart1_rhr        , 0x10320 
        .equ    uart1_thr        , 0x10320 
        .equ    uart1_ier        , 0x10321 
        .equ    uart1_isr_reg    , 0x10322 
        .equ    uart1_fcr        , 0x10322 
        .equ    uart1_lcr        , 0x10323 
        .equ    uart1_mcr        , 0x10324 
        .equ    uart1_lsr        , 0x10325 
        .equ    uart1_icr        , 0x10325 
        .equ    uart1_msr        , 0x10326 
        .equ    uart1_spr        , 0x10327 
        .equ    uart1_dll        , 0x10320 
        .equ    uart1_dlm        , 0x10321 

        .macro  snap    reg
        move    $r0,\reg
        call    dot
stophere:
        jmp     stophere
        .endm


# not
.macro      not rd,r1
    xor     \rd,\r1,-1
.endm

# negate
.macro      neg x
    not     \x, \x
    add     \x, \x, 1
.endm   

# swap registers
.macro SWAP a b
    xor     \a,\a,\b
    xor     \b,\a,\b
    xor     \a,\a,\b 
.endm         

###### Memory Map ####################################################
## 0x00010000: IO
## 0x0000FFFF: Ret Stack (64 words)
## 0x0000FEFF: Arg Stack (32 words)
## 0x00008000: Unmanaged Heap 
## 0x00000000: Managed Heap (1024 frames)

###### Closure Encoding ############################################## 
## | 14b env ptr (/4) | 18b code ptr |
        
###### Env ############_#############################################                                                    
#           |0|###    |N|       | |       | |       | |-------]0x8000                                                          
#                E     N                                           
#--------------------------------------------------------------------
# GetFrame  |0|###<---|-|       | |       | |       | |-------]0x8000     
#                      E         N
####### Registers ###################################################
#define     A   $r0
#---------------------Frame 0 
#define     F00 $r1
#define     F01 $r2
#define     F02 $r3
#define     F03 $r4
#define     F04 $r5
#define     F05 $r6
#define     F06 $r7
#define     F07 $r8
#---------------------Frame 1
#define     F10 $r9
#define     F11 $r10
#define     F12 $r11
#define     F13 $r12
#define     F14 $r13
#define     F15 $r14
#define     F16 $r15
#define     F17 $r16
#---------------------State
#define     FM  $r17   
#define     D   $r18        
#define     N   $r19    
#define     E   $r20
#---------------------GPIO Shadow
#define     IO0 $r21 
#define     IO1 $r22 
#define     IO2 $r23

#define S0      $r24
#define S1      $r25
#define MK      $r26        
#define C0      $r27           
# r28           
# r29    
# r30       
#define     R   $r31           



mark_val:   .long 0x80000000
frame_mask:  .long 0xFFFFFFE0
#define CELL_MASK    0x1F
    
.macro LDIS x   #20b constant
    ldk     A, \x
.endm    

.macro LDIL x  #(could switch to lpm? this is only 30b) 
   ldk      A, (\x >> 10)
   ldl      A, A,(\x & 0x3ff)
.endm     

.macro CLOS a 
   ashl    A, E, 16 
   ldk     S0, \a
   or      A, A, S0
.endm     
   
IF:

.macro ACC0
    ldi     A, E, 0
.endm    

.macro ACC n
    ldk   $r28, (\n<<2)
    call  _ACC
.endm

_ACC:
    and     S0, E, FM       # E frame idx
    sub     S1, E, $r28
findfrm:    
    sub     $r28, S0, S1
    jmpx    31, $r28, 1, thisframe
    ldi     S1, S0, 0
    and     S0, S1, FM
    sub     S1, S1, $r28
    jmp     findfrm
thisframe:
    ldi     A, S1, 0   
    return                               

.macro GET_FRAME
    exi     E, N, 0             # swap E and [N] in next frames header
    SWAP    E, N                # swap N & E
.endm

.macro DROP_FRAME
    exi     N, E, 0             # swap N and [E] in next frames header
    SWAP    E, N                # swap N & E
.endm

.macro _APPT
    bextu   E, A, (14<<5)|18    # get E' from clos in A            
    GET_FRAME
    add     E, E, 4             # inc E 
    ldi     S0, D, 0            # grab first arg
    sti     E, 0, S0
    add     D, D, 4             # inc D    
    jmpi    A   
.endm

APP:    # call to
    push    E                   # push E on R
APPT:   # jump to 
    _APPT

.macro PUSH_D
    sub     D, D, 4
    sti     D, 0, A
.endm 

.macro POP_D    
    ldi     A, D, 0
    add     D, D, 4
.endm

.macro MARK
    sub     D, D, 4
    sti     D, 0, MK
.endm

.macro PUSH_E r
    and     S1, E, CELL_MASK
    cmp     S1, 0x1C
    jmpc    nz, 1f
    GET_FRAME
1:
    add     E, E, 4
    sti     E, 0, \r
.endm       

GRAB:
    ldi     S0, D, 0
    add     D, D, 4     
    cmp     S0, MK
    jmpc    nz, notgrabmk
    # build clos
    ashl    A, E, 16 
    pop     S0
    or      A, A, S0
    pop     E
    return
notgrabmk:    
    PUSH_E S0
    return

RET:    # jump to
    ldi     S0, D, 0   
    add     D, D, 4  
    cmp     S0, MK
    jmpc    nz, notretmk
    pop     E
    return
notretmk:    
    _APPT

LET:
    PUSH_E A
    return

ELET:
    and     S1, E, CELL_MASK
    cmp     S1, 0x4
    jmpc    nz, 1f
    DROP_FRAME
    return
1:    
    sub     E, E, 4
    return

.macro TEMP
    PUSH_E MK
.endm    
    
.macro UPDT
    sti     E, 0, A
.endm


END:
    jmp END

#### Arith #####
ADD:
    ldi S0, D, 0
    add D, D, 4
    add A, A, S0    
    return

SUB:
    ldi S0, D, 0
    add D, D, 4
    sub A, A, S0    
    return    
   

#######   Test   #####################################################

test:
#     MARK
#     LDIS 5
#     PUSH_D
#     CLOS c0
#     call APP
#     jmp END
# c0:    
#     ACC0
#     PUSH_D
#     ACC0
#     ldi S0, D, 0
#     add D, D, 4
#     add A, A, S0
#     jmp RET

    MARK 
    MARK 
    LDIS 10
    PUSH_D
    CLOS c0
    call APP 
    PUSH_D 
    CLOS c1
    call APP 
    jmp END 
c0:    
    LDIS 0
    call LET 
    LDIS 1
    call LET 
    LDIS 2
    call LET 
    LDIS 3
    call LET 
    LDIS 4
    call LET 
    LDIS 5
    call LET 
    LDIS 6
    call LET 
    LDIS 7
    call LET 
    LDIS 8
    call LET 
    ACC 9
    PUSH_D
    ACC0
    call SUB 
    PUSH_D 
    ACC 1
    call ADD 
    PUSH_D 
    ACC 2
    call ADD 
    PUSH_D 
    ACC 3
    call ADD 
    PUSH_D 
    ACC 4
    call ADD 
    PUSH_D 
    ACC 5
    call ADD 
    PUSH_D 
    ACC 6
    call ADD 
    PUSH_D 
    ACC 7
    call ADD 
    PUSH_D 
    ACC 8
    call ADD 
    jmp RET
c1:    
    ACC0
    PUSH_D
    ACC0
    call ADD 
    jmp RET     

#######   BOOT   #####################################################


codestart:

    ldk     $r1,0x80
    sta.b   sys_regmsc0cfg_b3,$r1

    # Enable the RTC as soon as possible
    # Write 1 to RTC_EN in RTC_CCR
    ldk     $r1,(1 << 2)
    sta     0x1028c,$r1

    #init VM
    lpm     MK,     mark_val
    lpm     FM,     frame_mask
    ldk     C0,     0
    ldk     R,      0xFFFF
    ldk     D,      0xFEFF

    #setup frame chain
    ldk     N, 0x20
    ldk     S1, 0x8000
1:        
    add     S0, N, 32
    sti     N, 0, S0
    move    N, S0
    cmp     N, S1
    jmpc    lt, 1b  

    ldk     N, 0x20
    ldk     S0, 0
    ldk     S1, 0

    #run 
    jmp test






