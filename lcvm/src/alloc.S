
###########################################################
## Allocator ##############################################

#r27 is the allocator register. It contains the addr of the active region in the vector

# |      Region Stack Entry       |
# |31-----|24-----|16-----|8------|
# |   region cap  |   next slot   |
# | 



.equ ALLOC_VEC_ADDR,         0x00000000   # the vector of region entries starts here
.equ ALLOC_VEC_SIZE,         0xF   # the size of the region vector

init_alloc_stack: 
   # set vector to 0 
   ldk      $r0, 0
   memset   $r0, $r0, ALLOC_VEC_SIZE  

# request a new region - the size is in r0, r27 is the addr of the current region entry
new_region:
   ldk      $r27, -4
find_open_slot_loop:
   add      $r27, $r27, 4   
   cmp      $r27, (ALLOC_VEC_ADDR + ALLOC_VEC_SIZE)
   jmpc     ns, no_open_slots_err
   ldi      $r26, $r27, 0           # test the next slot, if 0 we know its open                   
   btst     $r26, (0<<5)|0 
   jmpc     nz, find_open_slot_loop
   # found an open slot
   lshr     $r25, $r26, 16          # test if there is a cap set
   cmp      $r25, 0
   move     $r26, $r27
   sub      $r26, $r26, 4           # load the prior slot
   ldi      $r26, $r26, 0
   lshr     $r26, $r26, 16
   jmpc     z, not_slot_cap_limit   # no cap above
   ldk      $r24, 0xFFFF            # flag for adjacent empty slots
   cmp      $r25,$r24
   jmpc     nz, single_open_slot    
   # there is more than 1 open slot, so we search ahead for the cap
   move     $r23, $r27           
look_ahead_for_cap:   
   add      $r23, 4
   ldi      $r25, $r23, 0
   lshr     $r25, $r25, 16
   cmp      $r25,$r24
   jmpc     z, look_ahead_for_cap   # if still open, keep looking
   
single_open_slot:   
   sub      $r25, $r25, $r26        # find open size
   cmp      $r25, $r0
   jmpc     s, find_open_slot_loop  # if not big enough, try again
   ashr     $r25, $r25, 1           # cut avail size in half
   cmp      $r25, $r0
   jmpc     ns, find_open_slot_loop # if still fits might be a waste, try again
not_slot_cap_limit:   
   add      $r0, $r0, $r26          # calc the new cap
   add      $r26, $r26, 4           # inc to set the next avail addr
   ashl     $r0, $r0, 16            # prep to combine cap & next
   or       $r0, $r0, $r26
   sti      $r27, 0, $r0            # store entry in vec
   return
no_open_slots_err:   
   ldk      $r0, 0                  # return 0 to indicate err
   return

# Allocate r0 bytes (aligned to word bound), addr returned in r0
# A return of 0 is a failure 
alloc: 
   ldi      $r26, $r27, 0
   lshr     $r25, $r26, 16
   bextu    $r26, $r26, (0<<5)|0      
   sub      $r24, $r25, $r26
   cmp      $r0, $r24
   jmpc     s, alloc_fail
   ashl     $r25, $r25, 16
   #--need to align addr
alloc_fail:   
   ldk      $r0, 0                  # return 0 to indicate fail
   return


###########################################################
## Hash Table #############################################


hash_C1:    .long 0x85EBCA6B
hash_C2:    .long 0xC2B2AE35

.macro swapr a,b
   xor     \a,\a,\b
   xor     \b,\a,\b
   xor     \a,\a,\b
.endm   


.macro   hash_int 
   lshr     $r1,$r0,16
   xor      $r0,$r0,$r1
   lpm      $r1,hash_C1
   mul      $r0,$r0,$r1
   lshr     $r1,$r0,13
   xor      $r0,$r0,$r1
   lpm      $r1,hash_C2
   mul      $r0,$r0,$r1
   lshr     $r1,$r0,16
   xor      $r0,$r0,$r1   
.endm

.macro hash_str             #address of str in r2
   swapr    $r3,$r31
   ldk      $r0,5381
hash_loop:
   pop.b    $r2
   cmp      $r2,0
   jmpc     z,end_of_str
   ashl     $r1,$r0,5
   add      $r0,$r0,$r1
   xor      $r0,$r0,$r3
   jmp      hash_loop
end_of_str:
   swapr    $r3,$r31     
.endm   


# Create a new table of r0 size in slots (eventually we will have an allocator,
# for now target address passed in r1)
new_table:   
   sub      $r2,$r0,1
   and      $r2,$r2,$r0
   cmp      $r2,0
   jmpc     z,size_is_ok
   ldk      $r0,0
   return                  # requested table size is not a power of 2
size_is_ok:
   add      $r0,$r0,8      # add room for header and zero cell
   memset   $r1,$r2,$r0
   sti.s    $r1,0,$r0
   add      $r0,$r1,8      # return the beginning of the regular table
   return


table_get:
   cmp      $r0,0
   jmpc     z,check_zero_cell
   
check_zero_cell:





table_set:
