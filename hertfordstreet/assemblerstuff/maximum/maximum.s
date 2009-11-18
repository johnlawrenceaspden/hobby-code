#find the maximum of a set

#using %ebx to store the current max, %edi as index

.section .data

data_items:

.long 3,67,34,222,45,75,54,34,44,33,22,11,66,0 #null terminated list!

.section .text
.globl _start

_start:

movl $0,   %edi                        #index into data  
movl data_items(,%edi,4), %eax         
movl %eax, %ebx                        #first item is maximum so far

start_loop:
  cmpl $0, %eax
  je loop_exit
  incl %edi
  movl data_items(,%edi,4), %eax
  cmpl %ebx, %eax
  jle start_loop

  movl %eax, %ebx
  jmp start_loop

loop_exit:
                      # %ebx already contains the number we wish to return
  movl $1, %eax       # exit call
  int $0x80           # call kernel

