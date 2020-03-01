.intel_syntax noprefix
.global main
main:
	mov rdi, 1
	mov rsi, 2
	add rdi, rsi
	mov rsi, 3
	add rdi, rsi
	mov rsi, 4
	add rdi, rsi
	mov rsi, 5
	add rdi, rsi
	mov rsi, 6
	add rdi, rsi
	mov rsi, 7
	add rdi, rsi
	mov rsi, 8
	add rdi, rsi
	mov rax, rdi
	ret
