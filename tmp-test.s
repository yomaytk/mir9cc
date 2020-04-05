.intel_syntax noprefix
.data
var1:
  .ascii "\000"
var2:
  .ascii "\000"
.L.str5:
  .ascii "%s => %d\\n\000"
.L.str6:
  .ascii "0\000"
.L.str7:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str8:
  .ascii "0\000"
.L.str9:
  .ascii "%s => %d\\n\000"
.L.str10:
  .ascii "1\000"
.L.str11:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str12:
  .ascii "1\000"
.L.str13:
  .ascii "%s => %d\\n\000"
.L.str14:
  .ascii "1+1\000"
.L.str15:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str16:
  .ascii "1+1\000"
.L.str17:
  .ascii "%s => %d\\n\000"
.L.str18:
  .ascii "2*3+4\000"
.L.str19:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str20:
  .ascii "2*3+4\000"
.L.str21:
  .ascii "%s => %d\\n\000"
.L.str22:
  .ascii "2*3+4*5\000"
.L.str23:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str24:
  .ascii "2*3+4*5\000"
.L.str25:
  .ascii "%s => %d\\n\000"
.L.str26:
  .ascii "50/10\000"
.L.str27:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str28:
  .ascii "50/10\000"
.L.str29:
  .ascii "%s => %d\\n\000"
.L.str30:
  .ascii "6*3/2\000"
.L.str31:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str32:
  .ascii "6*3/2\000"
.L.str33:
  .ascii "%s => %d\\n\000"
.L.str34:
  .ascii "(2+3)*(4+5)\000"
.L.str35:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str36:
  .ascii "(2+3)*(4+5)\000"
.L.str37:
  .ascii "%s => %d\\n\000"
.L.str38:
  .ascii "1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17\000"
.L.str39:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str40:
  .ascii "1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17\000"
.L.str41:
  .ascii "OK\\n\000"
.text
.global one
one:
	push rbp
	mov rbp, rsp
	sub rsp, 0
	push r12
	push r13
	push r14
	push r15
	mov r10, 1
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
.text
.global two
two:
	push rbp
	mov rbp, rsp
	sub rsp, 0
	push r12
	push r13
	push r14
	push r15
	mov r10, 2
	mov rax, r10
	jmp .Lend1
.Lend1:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
.text
.global plus
plus:
	push rbp
	mov rbp, rsp
	sub rsp, 8
	push r12
	push r13
	push r14
	push r15
	mov [rbp-4], edi
	mov [rbp-8], esi
	mov r10, rbp
	sub r10, 4
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 8
	mov r11d, [r11]
	add r10, r11
	mov rax, r10
	jmp .Lend2
.Lend2:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
.text
.global mul
mul:
	push rbp
	mov rbp, rsp
	sub rsp, 16
	push r12
	push r13
	push r14
	push r15
	mov [rbp-12], edi
	mov [rbp-16], esi
	mov r10, rbp
	sub r10, 12
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 16
	mov r11d, [r11]
	mov rax, r11
	mul r10
	mov r10, rax
	mov rax, r10
	jmp .Lend3
.Lend3:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
.text
.global add
add:
	push rbp
	mov rbp, rsp
	sub rsp, 40
	push r12
	push r13
	push r14
	push r15
	mov [rbp-20], edi
	mov [rbp-24], esi
	mov [rbp-28], edx
	mov [rbp-32], ecx
	mov [rbp-36], r8d
	mov [rbp-40], r9d
	mov r10, rbp
	sub r10, 20
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 24
	mov r11d, [r11]
	add r10, r11
	mov r11, rbp
	sub r11, 28
	mov r11d, [r11]
	add r10, r11
	mov r11, rbp
	sub r11, 32
	mov r11d, [r11]
	add r10, r11
	mov r11, rbp
	sub r11, 36
	mov r11d, [r11]
	add r10, r11
	mov r11, rbp
	sub r11, 40
	mov r11d, [r11]
	add r10, r11
	mov rax, r10
	jmp .Lend4
.Lend4:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
.text
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 112
	push r12
	push r13
	push r14
	push r15
.L2:
	mov r10, rbp
	sub r10, 44
	mov r11, 0
	mov [r10], r11d
	mov r10, rbp
	sub r10, 48
	mov r11, 0
	mov [r10], r11d
	mov r10, rbp
	sub r10, 44
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 48
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L3
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str5
	lea rbx, .L.str6
	mov r12, rbp
	sub r12, 48
	mov r12d, [r12]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	jmp .L3
.L3:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str7
	mov rbx, 27
	lea r12, .L.str8
	mov r13, rbp
	sub r13, 44
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 48
	mov r14d, [r14]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r15, rax
	mov r10, 1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call exit
	pop r11
	pop r10
	mov r11, rax
.L4:
	mov r10, 0
	cmp r10, 0
	jne .L2
.L5:
	mov r10, rbp
	sub r10, 52
	mov r11, 1
	mov [r10], r11d
	mov r10, rbp
	sub r10, 56
	mov r11, 1
	mov [r10], r11d
	mov r10, rbp
	sub r10, 52
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 56
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L6
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str9
	lea rbx, .L.str10
	mov r12, rbp
	sub r12, 56
	mov r12d, [r12]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	jmp .L6
.L6:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str11
	mov rbx, 28
	lea r12, .L.str12
	mov r13, rbp
	sub r13, 52
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 56
	mov r14d, [r14]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r15, rax
	mov r10, 1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call exit
	pop r11
	pop r10
	mov r11, rax
.L7:
	mov r10, 0
	cmp r10, 0
	jne .L5
.L8:
	mov r10, rbp
	sub r10, 60
	mov r11, 2
	mov [r10], r11d
	mov r10, rbp
	sub r10, 64
	mov r11, 1
	mov rbx, 1
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 60
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 64
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L9
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str13
	lea rbx, .L.str14
	mov r12, rbp
	sub r12, 64
	mov r12d, [r12]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	jmp .L9
.L9:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str15
	mov rbx, 29
	lea r12, .L.str16
	mov r13, rbp
	sub r13, 60
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 64
	mov r14d, [r14]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r15, rax
	mov r10, 1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call exit
	pop r11
	pop r10
	mov r11, rax
.L10:
	mov r10, 0
	cmp r10, 0
	jne .L8
.L11:
	mov r10, rbp
	sub r10, 68
	mov r11, 10
	mov [r10], r11d
	mov r10, rbp
	sub r10, 72
	mov r11, 2
	mov rbx, 3
	mov rax, rbx
	mul r11
	mov r11, rax
	mov rbx, 4
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 68
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 72
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L12
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str17
	lea rbx, .L.str18
	mov r12, rbp
	sub r12, 72
	mov r12d, [r12]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	jmp .L12
.L12:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str19
	mov rbx, 30
	lea r12, .L.str20
	mov r13, rbp
	sub r13, 68
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 72
	mov r14d, [r14]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r15, rax
	mov r10, 1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call exit
	pop r11
	pop r10
	mov r11, rax
.L13:
	mov r10, 0
	cmp r10, 0
	jne .L11
.L14:
	mov r10, rbp
	sub r10, 76
	mov r11, 26
	mov [r10], r11d
	mov r10, rbp
	sub r10, 80
	mov r11, 2
	mov rbx, 3
	mov rax, rbx
	mul r11
	mov r11, rax
	mov rbx, 4
	mov r12, 5
	mov rax, r12
	mul rbx
	mov rbx, rax
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 76
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 80
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L15
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str21
	lea rbx, .L.str22
	mov r12, rbp
	sub r12, 80
	mov r12d, [r12]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	jmp .L15
.L15:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str23
	mov rbx, 31
	lea r12, .L.str24
	mov r13, rbp
	sub r13, 76
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 80
	mov r14d, [r14]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r15, rax
	mov r10, 1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call exit
	pop r11
	pop r10
	mov r11, rax
.L16:
	mov r10, 0
	cmp r10, 0
	jne .L14
.L17:
	mov r10, rbp
	sub r10, 84
	mov r11, 5
	mov [r10], r11d
	mov r10, rbp
	sub r10, 88
	mov r11, 50
	mov rbx, 10
	mov rax, r11
	cqo
	div rbx
	mov r11, rax
	mov [r10], r11d
	mov r10, rbp
	sub r10, 84
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 88
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L18
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str25
	lea rbx, .L.str26
	mov r12, rbp
	sub r12, 88
	mov r12d, [r12]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	jmp .L18
.L18:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str27
	mov rbx, 32
	lea r12, .L.str28
	mov r13, rbp
	sub r13, 84
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 88
	mov r14d, [r14]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r15, rax
	mov r10, 1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call exit
	pop r11
	pop r10
	mov r11, rax
.L19:
	mov r10, 0
	cmp r10, 0
	jne .L17
.L20:
	mov r10, rbp
	sub r10, 92
	mov r11, 9
	mov [r10], r11d
	mov r10, rbp
	sub r10, 96
	mov r11, 6
	mov rbx, 3
	mov rax, rbx
	mul r11
	mov r11, rax
	mov rbx, 2
	mov rax, r11
	cqo
	div rbx
	mov r11, rax
	mov [r10], r11d
	mov r10, rbp
	sub r10, 92
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 96
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L21
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str29
	lea rbx, .L.str30
	mov r12, rbp
	sub r12, 96
	mov r12d, [r12]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	jmp .L21
.L21:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str31
	mov rbx, 33
	lea r12, .L.str32
	mov r13, rbp
	sub r13, 92
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 96
	mov r14d, [r14]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r15, rax
	mov r10, 1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call exit
	pop r11
	pop r10
	mov r11, rax
.L22:
	mov r10, 0
	cmp r10, 0
	jne .L20
.L23:
	mov r10, rbp
	sub r10, 100
	mov r11, 45
	mov [r10], r11d
	mov r10, rbp
	sub r10, 104
	mov r11, 2
	mov rbx, 3
	add r11, rbx
	mov rbx, 4
	mov r12, 5
	add rbx, r12
	mov rax, rbx
	mul r11
	mov r11, rax
	mov [r10], r11d
	mov r10, rbp
	sub r10, 100
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 104
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L24
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str33
	lea rbx, .L.str34
	mov r12, rbp
	sub r12, 104
	mov r12d, [r12]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	jmp .L24
.L24:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str35
	mov rbx, 34
	lea r12, .L.str36
	mov r13, rbp
	sub r13, 100
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 104
	mov r14d, [r14]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r15, rax
	mov r10, 1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call exit
	pop r11
	pop r10
	mov r11, rax
.L25:
	mov r10, 0
	cmp r10, 0
	jne .L23
.L26:
	mov r10, rbp
	sub r10, 108
	mov r11, 153
	mov [r10], r11d
	mov r10, rbp
	sub r10, 112
	mov r11, 1
	mov rbx, 2
	add r11, rbx
	mov rbx, 3
	add r11, rbx
	mov rbx, 4
	add r11, rbx
	mov rbx, 5
	add r11, rbx
	mov rbx, 6
	add r11, rbx
	mov rbx, 7
	add r11, rbx
	mov rbx, 8
	add r11, rbx
	mov rbx, 9
	add r11, rbx
	mov rbx, 10
	add r11, rbx
	mov rbx, 11
	add r11, rbx
	mov rbx, 12
	add r11, rbx
	mov rbx, 13
	add r11, rbx
	mov rbx, 14
	add r11, rbx
	mov rbx, 15
	add r11, rbx
	mov rbx, 16
	add r11, rbx
	mov rbx, 17
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 108
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 112
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L27
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str37
	lea rbx, .L.str38
	mov r12, rbp
	sub r12, 112
	mov r12d, [r12]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r13, rax
	jmp .L27
.L27:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str39
	mov rbx, 35
	lea r12, .L.str40
	mov r13, rbp
	sub r13, 108
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 112
	mov r14d, [r14]
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call fprintf
	pop r11
	pop r10
	mov r15, rax
	mov r10, 1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call exit
	pop r11
	pop r10
	mov r11, rax
.L28:
	mov r10, 0
	cmp r10, 0
	jne .L26
	lea r10, .L.str41
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call printf
	pop r11
	pop r10
	mov r11, rax
	mov r10, 0
	mov rax, r10
	jmp .Lend5
.Lend5:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
