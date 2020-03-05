.intel_syntax noprefix
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 16
	mov rdi, rsp
	mov rsi, rdi
	add rsi, 0
	mov r10, 2
	mov [rsi], r10
	mov r10, rdi
	add r10, 8
	mov r11, 3
	mov r12, 2
	add r11, r12
	mov [r10], r11
	mov r11, rdi
	add r11, 0
	mov r11, [r11]
	mov r12, rdi
	add r12, 8
	mov r12, [r12]
	mov rax, r12
	mul r11
	mov r11, rax
	mov rax, r11
	jmp .L1
.L1:
	mov rsp, rbp
	mov rsp, rbp
	pop rbp
	ret
