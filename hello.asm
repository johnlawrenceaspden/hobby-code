;;; hello world for assembler

;;; On 64 bit linux compile, link, run with these commands
;;; nasm -f elf64 hello.asm
;;; ld -s -o hello hello.o
;;; ./hello
        
        section .data
        msg db "Hello World!",0x0a

        len equ $-msg

        section .text
        global _start

_start:
        mov ebx,0x01            ; file descriptor (stdout)
        mov ecx,msg             ; pointer to buffer
        mov edx,len             ; length of buffer
        mov eax,0x04            ; system call number 4 (write)
        int 0x80                ; make system call

        mov ebx,0x00            ; return code 0
        mov eax,0x01            ; system call number 1 (exit)
        int 0x80                ; make system call
        
        
