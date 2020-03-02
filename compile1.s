.intel_syntax noprefix
.global main
main:
	mov rdi, 6
	mov rsi, 3
	mov rax, rsi
	mul rdi
	mov rdi, rax
	mov rsi, 2
	mov rax, rdi
	cqo
	div rsi
	mov rdi, rax
	mov rax, rdi
	ret
