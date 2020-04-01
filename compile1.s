.intel_syntax noprefix
.data
.L.str1:
  .ascii "abc\000"
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
	sub r10, 8
	lea r11, .L.str1
	mov [r10], r11
	mov r10, rbp
	sub r10, 8
	mov r10, [r10]
	mov r11, 3
	mov rbx, 1
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10b, [r10]
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
