.intel_syntax noprefix
.data
.L.str1:
  .ascii "\000"
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
	lea r10, .L.str1
	mov r11, 0
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 5
	mov [r10], r11d
	lea r10, .L.str1
	mov r11, 4
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 10
	mov [r10], r11d
	lea r10, .L.str1
	mov r11, 0
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10d, [r10]
	lea r11, .L.str1
	mov rbx, 4
	mov r12, 4
	mov rax, r12
	mul rbx
	mov rbx, rax
	add r11, rbx
	mov r11d, [r11]
	add r10, r11
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
