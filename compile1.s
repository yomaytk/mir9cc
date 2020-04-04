.intel_syntax noprefix
.data
.text
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
	sub r10, 4
	mov r11, 0
	mov [r10], r11d
	mov r10, rbp
	sub r10, 8
	mov r11, 0
	mov [r10], r11d
.L1:
	mov r10, rbp
	sub r10, 8
	mov r11, rbp
	sub r11, 8
	mov r11d, [r11]
	mov rbx, rbp
	sub rbx, 4
	mov ebx, [rbx]
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 4
	mov r11, rbp
	sub r11, 4
	mov r11d, [r11]
	mov rbx, 1
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 4
	mov r10d, [r10]
	mov r11, 10
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	cmp r10, 0
	jne .L1
	mov r10, rbp
	sub r10, 8
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
