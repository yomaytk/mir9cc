.intel_syntax noprefix
.data
.L.str1:
  .ascii "%d => %d\\n\000"
.L.str2:
  .ascii "OK\\n\000"
.text
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 0
	push r12
	push r13
	push r14
	push r15
.L2:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str1
	mov rbx, 54
	mov r12, 320
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	mov r10, 0
	cmp r10, 0
	jne .L2
	lea r10, .L.str2
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call printf
	pop r11
	pop r10
	mov r11, rax
	mov r10, 0
	mov rax, r10
	jmp .Lend0
.Lend0:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
