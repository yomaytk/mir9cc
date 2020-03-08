.intel_syntax noprefix
.global one
one:
	push r12
	push r13
	push r14
	push r15
	push rbp
	mov rbp, rsp
	sub rsp, 0
	mov r10, rsp
	mov r11, 1
	mov rax, r11
	jmp .Lend0
.Lend0:
	mov rsp, rbp
	pop rbp
	pop r15
	pop r14
	pop r13
	pop r12
	ret
.global two
two:
	push r12
	push r13
	push r14
	push r15
	push rbp
	mov rbp, rsp
	sub rsp, 0
	mov r10, rsp
	mov r11, 2
	mov rax, r11
	jmp .Lend1
.Lend1:
	mov rsp, rbp
	pop rbp
	pop r15
	pop r14
	pop r13
	pop r12
	ret
.global main
main:
	push r12
	push r13
	push r14
	push r15
	push rbp
	mov rbp, rsp
	sub rsp, 0
	mov r10, rsp
	push r10
	push r11
	mov rax, 0
	call one
	pop r11
	pop r10
	mov r11, rax
	push r10
	push r11
	mov rax, 0
	call two
	pop r11
	pop r10
	mov rbx, rax
	add r11, rbx
	mov rax, r11
	jmp .Lend2
.Lend2:
	mov rsp, rbp
	pop rbp
	pop r15
	pop r14
	pop r13
	pop r12
	ret
