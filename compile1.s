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
	mov r11, 1
	mov [r10], r11d
	mov r10, rbp
	sub r10, 8
	mov r11, 2
	mov [r10], r11d
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
