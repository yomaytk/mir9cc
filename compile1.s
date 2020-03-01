.intel_syntax noprefix
.global main
main:
	mov rdi, 12
	mov rsi, 34
	add rdi, rsi
	mov rsi, 5
	add rdi, rsi
	mov rax, rdi
	ret
