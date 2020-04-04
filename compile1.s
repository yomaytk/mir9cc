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
	lea r10, global_arr
	mov r11, 0
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
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
