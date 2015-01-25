;;; hello world for assembler

;;; On 64 bit linux compile, link, run with these commands
;;; nasm -f elf64 hello.asm
;;; ld -s -o hello hello.o
;;; ./hello

        sys_exit equ 0x01
        write    equ 0x04
        stdout   equ 0x01

        section .bss
        outputBuffer resb 4
        
        section .data
        msg db "GCD Machine",0x0a

        len equ $-msg

        section .text
        global _start

_start:


        ;; write message to stdout      
        mov ebx,stdout            ; file descriptor (stdout)
        mov ecx,msg             ; pointer to buffer
        mov edx,len             ; length of buffer
        mov eax,write            ; system call number 4 (write)
        int 0x80                ; make system call

        ;; put value in outputBuffer
        mov ecx,1
        add ecx,0x30            ;ascii conversion
        mov [outputBuffer],ecx

        
        ;; write digit to stdout
        mov ebx, stdout
        mov ecx, outputBuffer
        mov edx, 1
        mov eax, write            ; system call number 4 (write)         
        int 0x80

        ;; exit with code 0
        mov ebx,0x00            ; return code 0
        mov eax,sys_exit            ; system call number 1 (exit)
        int 0x80                ; make system call
        
        
