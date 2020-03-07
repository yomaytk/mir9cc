.intel_syntax noprefix
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 0
	mov rdi, rsp
	mov rsi, 0
	cmp rsi, 0
	je .L1
	mov rsi, 2
	mov rax, rsi
	jmp .Lend
	jmp .L1
.L1:
	mov rsi, 3
	mov rax, rsi
	jmp .Lend
.L2:
.Lend:
	mov rsp, rbp
	mov rsp, rbp
	pop rbp
	ret
