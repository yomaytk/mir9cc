.intel_syntax noprefix
.data
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
	mov r10, 3
	mov r11, 5
	mov rbx, r11
	jmp .L2
.L2:
	add r10, rbx
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
