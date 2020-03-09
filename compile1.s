.intel_syntax noprefix
.global add
add:
	push rbp
	mov rbp, rsp
	sub rsp, 48
	push r12
	push r13
	push r14
	push r15
	mov [rbp-8], rdi
	mov [rbp-16], rsi
	mov [rbp-24], rdx
	mov [rbp-32], rcx
	mov [rbp-40], r8
	mov [rbp-48], r9
	mov r10, rbp
	sub r10, 8
	mov r10, [r10]
	mov r11, rbp
	sub r11, 16
	mov r11, [r11]
	add r10, r11
	mov r11, rbp
	sub r11, 24
	mov r11, [r11]
	add r10, r11
	mov r11, rbp
	sub r11, 32
	mov r11, [r11]
	add r10, r11
	mov r11, rbp
	sub r11, 40
	mov r11, [r11]
	add r10, r11
	mov r11, rbp
	sub r11, 48
	mov r11, [r11]
	add r10, r11
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
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 0
	push r12
	push r13
	push r14
	push r15
	mov r10, 1
	mov r11, 2
	mov rbx, 3
	mov r12, 4
	mov r13, 5
	mov r14, 6
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call add
	pop r11
	pop r10
	mov r15, rax
	mov rax, r15
	jmp .Lend1
.Lend1:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
