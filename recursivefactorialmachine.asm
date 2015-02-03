;;; assembly language version of:

;; (def recursive-factorial-program
;;   '[:begin
;;     (assign continue :done)
;;     :loop
;;     (branch (= 0 n) :base)
;;     (save continue)
;;     (save n)
;;     (assign n (dec n))
;;     (assign continue :aft)
;;     (goto :loop)
;;     :aft
;;     (restore n)
;;     (restore continue)
;;     (assign value (* n value))
;;     (goto continue)
;;     :base
;;     (assign value 1)
;;     (goto continue)
;;     :done])

;;; On 64 bit linux assemble and run with

;;; nasm -f elf64 ${PROGRAM}.asm && ld -o ${PROGRAM} ${PROGRAM}.o && ./${PROGRAM} ; echo "--->" $?

        
        sys_exit equ 0x01
        write    equ 0x04
        stdout   equ 0x01
        
        section .bss
        
        ;; registers, all 64 bit 
        n        resb 8
        value    resb 8
        continue resb 8
        
        ;; opening message
        section .data
        msg db "Recursive Factorial Machine",0x0a
        len equ $-msg

        section .text
        global _start

_start:


        ;; write message to stdout
        mov eax,write            ; system call number 4 (write)
        mov ebx,stdout            ; file descriptor (stdout)
        mov ecx,msg             ; pointer to buffer
        mov edx,len             ; length of buffer
        int 0x80                ; make system call

        ;; set up problem
        mov rax, 20
        mov [n], rax
begin:
        ;; (assign continue :done)
        mov rax, done
        mov [continue], rax
loop:    
        ;; (branch (= 0 n) :base)  
        cmp [n], dword 0
        je base

        ;; (save continue)
        mov rax,[continue]
        push rax
        
        ;; (save n)
        mov rax,[n]
        push rax

        ;; (assign n (dec n))
        mov rax,[n]
        dec rax
        mov [n],rax

        ;; (assign continue :aft)
        mov rax,aft
        mov [continue],rax

        ;; (goto loop)
        jmp loop
aft:
        ;; (restore n)
        pop rax
        mov [n],rax

        ;; (restore continue)
        pop rax
        mov [continue],rax

        ;; (assign value (* n value)
        mov rax,[value]
        mov rbx,[n]
        mul rbx
        mov [value],rax

        ;; (goto continue)
        mov rax,[continue]
        jmp rax

base:
        ;; (assign value 1)
        mov [value],dword 1
        
        ;; (goto continue)
        mov rax,[continue]
        jmp rax 


done:   
        mov rax,[value]
        
printeax:
        mov r9,0

count:
        ;; express eax as decimal digits 
        ;; repeatedly divide eax by ten, pushing remainders onto stack, until it's zero
        ;; count digits in r9
        mov rbx,10
        mov rdx,0
        div rbx ;quotient goes into eax, remainder into rdx
        push rdx
        inc r9

        cmp rax,0
        jnz count
        

show:
        ;; take each digit off the stack, convert to ascii, move into message buffer
        pop rdx
        add rdx,0x30
        mov [msg],rdx

        ;; preserving r9, write that byte to stdout
        push r9
        mov eax,write            ; system call number 4 (write)
        mov ebx,stdout           ; file descriptor (stdout)
        mov ecx,msg              ; pointer to buffer
        mov edx,1                ; length of buffer
        int 0x80                 ; make system call
        pop r9

        ;; repeat until all digits output
        dec r9
        cmp r9,0
        jnz show

newline:
        ;; finally print a newline
        mov [msg],byte 0xa
        mov eax,write            ; system call number 4 (write)
        mov ebx,stdout           ; file descriptor (stdout)
        mov ecx,msg              ; pointer to buffer
        mov edx,1                ; length of buffer
        int 0x80                 ; make system call
  
        
exit:   
        mov ebx,0              ; return code
        mov eax,sys_exit       ; system call number 1 (exit)
        int 0x80               ; make system call
        
        
