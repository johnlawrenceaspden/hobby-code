#assembler program to return a value to the system under linux
#run it and then use echo $? to get the value


.section .data
.section .text
.globl _start

_start:

movl $1,   %eax #exit function
movl $100, %ebx #return value

int $0x80     #call kernel

