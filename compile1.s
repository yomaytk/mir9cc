.intel_syntax noprefix
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 16
	push r12
	push r13
	push r14
	push r15
	mov r10, rbp
	sub r10, 8
	mov r11, 0
	mov [r10], r11
	mov r11, rbp
	sub r11, 16
	mov rbx, 10
	mov [r11], rbx
.L1:
	mov r11, rbp
	sub r11, 16
	mov r11, [r11]
	mov rbx, 15
	cmp r11, rbx
	setl r11b
	movzb r11, r11b
	cmp r11, 0
	je .L2
	mov r11, rbp
	sub r11, 8
	mov rbx, rbp
	sub rbx, 8
	mov rbx, [rbx]
	mov r12, rbp
	sub r12, 16
	mov r12, [r12]
	add rbx, r12
	mov [r11], rbx
	mov rbx, rbp
	sub rbx, 16
	mov r12, rbp
	sub r12, 16
	mov r12, [r12]
	mov r13, 1
	add r12, r13
	mov [rbx], r12
	jmp .L1
.L2:
	mov r12, rbp
	sub r12, 8
	mov r12, [r12]
	mov rax, r12
	jmp .Lend0
.Lend0:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
