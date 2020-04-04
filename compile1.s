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
	mov r10, 5
	mov r11, 5
	cmp r10, r11
	setne r10b
	movzb r10, r10b
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
