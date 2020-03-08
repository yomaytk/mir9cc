.intel_syntax noprefix
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 0
	mov rdi, rsp
	mov rsi, 2
	mov r10, 3
	push rbx
	push rbp
	push rsp
	push r12
	push r13
	push r14
	push r15
	mov rdi, rsi
	mov rsi, r10
	mov rax, 0
	call plus
	mov r11, rax
	pop r15
	pop r14
	pop r13
	pop r12
	pop rsp
	pop rbp
	pop rbx
	mov rax, r11
	jmp .Lend
.Lend:
	mov rsp, rbp
	mov rsp, rbp
	pop rbp
	ret
