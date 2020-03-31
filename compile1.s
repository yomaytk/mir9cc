.intel_syntax noprefix
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 12
	push r12
	push r13
	push r14
	push r15
	mov r10, rbp
	sub r10, 4
	mov r11, 0
	mov [r10], r11d
	mov r10, rbp
	sub r10, 12
	mov r11, rbp
	sub r11, 4
	mov [r10], r11
	mov r10, rbp
	sub r10, 12
	mov r10, [r10]
	mov r11, 0
	mov rbx, 1
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 42
	mov [r10], r11b
	mov r10, rbp
	sub r10, 4
	mov r10d, [r10]
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
