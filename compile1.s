.intel_syntax noprefix
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 8
	push r12
	push r13
	push r14
	push r15
	mov r10, rbp
	sub r10, 8
	mov r11, 1
	mov [r10], r11d
	mov r10, rbp
	sub r10, 8
	mov r11, 1
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 2
	mov [r10], r11d
	mov r10, rbp
	sub r10, 8
	mov r10, [r10]
	mov r11, rbp
	sub r11, 8
	mov rbx, 1
	mov r12, 4
	mov rax, r12
	mul rbx
	mov rbx, rax
	add r11, rbx
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
