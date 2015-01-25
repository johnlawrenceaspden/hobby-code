;;; assembly language version of:
;; (define-machine gcd
;;   (registers a b t)
;;   (controller
;;    loop
;;    (branch (zero? (fetch b)) done)
;;    (assign t (remainder (fetch a) (fetch b)))
;;    (assign a (fetch b))
;;    (assign b (fetch t))
;;    (goto loop)
;;    done))


;;; On 64 bit linux compile, link, run with these commands
;;; nasm -f elf64 hello.asm
;;; ld -o hello hello.o
;;; ./hello
;;; or gdb hello


        
        
        sys_exit equ 0x01
        write    equ 0x04
        stdout   equ 0x01

        section .bss
        outputBuffer resb 4
        a resb 4
        b resb 4
        t resb 4
        
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

        mov [a], dword 42      ; (2*3*7)
        mov [b], dword 32      ; (2*2*2*2*2)


loop:   
        ;; if b is zero branch to done
        cmp [b],dword 0
        jz done

        ;; divide a by b and put remainder in t
        mov eax,[a]
        mov edx,0
        mov ebx,[b]
        div ebx
        mov [t],edx

        ;; put b into a
        mov eax,[b]
        mov [a],eax

        ;; put t into b
        mov eax,[t]
        mov [b],eax

        ;; loop
        jmp loop                
        
done:   
printa:       
        ;; put value in outputBuffer
        mov eax,[a]
        add eax,0x30            ;ascii conversion
        mov [outputBuffer],eax

        
        ;; write digit to stdout
        mov ebx, stdout
        mov ecx, outputBuffer
        mov edx, 1
        mov eax, write            ; system call number 4 (write)         
        int 0x80

exit:   
        ;; exit with code 0
        mov ebx,0x00            ; return code 0
        mov eax,sys_exit            ; system call number 1 (exit)
        int 0x80                ; make system call
        
        
