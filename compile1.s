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
	mov r11, 2
	mov rdi, r11
	push r10
	push r11
	mov rax, 0
	call alloc_ptr_ptr
	pop r11
	pop r10
	mov rbx, rax
	mov [r10], rbx
	mov r10, rbp
	sub r10, 8
	mov r10, [r10]
	mov r10, [r10]
	mov r10, [r10]
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
