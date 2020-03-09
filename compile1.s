.intel_syntax noprefix
.global one
one:
	push rbp
	mov rbp, rsp
	sub rsp, 8
	push r12
	push r13
	push r14
	push r15
	mov r10, 1
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
.global two
two:
	push rbp
	mov rbp, rsp
	sub rsp, 8
	push r12
	push r13
	push r14
	push r15
	mov r10, 2
	mov rax, r10
	jmp .Lend1
.Lend1:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 8
	push r12
	push r13
	push r14
	push r15
	push r10
	push r11
	mov rax, 0
	call one
	pop r11
	pop r10
	mov r10, rax
	push r10
	push r11
	mov rax, 0
	call two
	pop r11
	pop r10
	mov r11, rax
	add r10, r11
	mov rax, r10
	jmp .Lend2
.Lend2:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
