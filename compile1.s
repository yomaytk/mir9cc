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
  .ascii "%s => %d\\n\000"
.L.str42:
  .ascii "({ int a=2; return a; })\000"
.L.str43:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str44:
  .ascii "({ int a=2; return a; })\000"
.L.str45:
  .ascii "%s => %d\\n\000"
.L.str46:
  .ascii "({ int a=2; int b; b=3+2; return a*b; })\000"
.L.str47:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str48:
  .ascii "({ int a=2; int b; b=3+2; return a*b; })\000"
.L.str49:
  .ascii "%s => %d\\n\000"
.L.str50:
  .ascii "({ if (1) return 2; return 3; })\000"
.L.str51:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str52:
  .ascii "({ if (1) return 2; return 3; })\000"
.L.str53:
  .ascii "%s => %d\\n\000"
.L.str54:
  .ascii "({ if (0) return 2; return 3; })\000"
.L.str55:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str56:
  .ascii "({ if (0) return 2; return 3; })\000"
.L.str57:
  .ascii "%s => %d\\n\000"
.L.str58:
  .ascii "({ if (1) return 2; else return 3; })\000"
.L.str59:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str60:
  .ascii "({ if (1) return 2; else return 3; })\000"
.L.str61:
  .ascii "%s => %d\\n\000"
.L.str62:
  .ascii "({ if (0) return 2; else return 3; })\000"
.L.str63:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str64:
  .ascii "({ if (0) return 2; else return 3; })\000"
.L.str65:
  .ascii "%s => %d\\n\000"
.L.str66:
  .ascii "plus(2, 3)\000"
.L.str67:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str68:
  .ascii "plus(2, 3)\000"
.L.str69:
  .ascii "%s => %d\\n\000"
.L.str70:
  .ascii "one()\000"
.L.str71:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str72:
  .ascii "one()\000"
.L.str73:
  .ascii "%s => %d\\n\000"
.L.str74:
  .ascii "one()+two()\000"
.L.str75:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str76:
  .ascii "one()+two()\000"
.L.str77:
  .ascii "%s => %d\\n\000"
.L.str78:
  .ascii "mul(2, 3)\000"
.L.str79:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str80:
  .ascii "mul(2, 3)\000"
.L.str81:
  .ascii "%s => %d\\n\000"
.L.str82:
  .ascii "add(1,2,3,4,5,6)\000"
.L.str83:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str84:
  .ascii "add(1,2,3,4,5,6)\000"
.L.str85:
  .ascii "%s => %d\\n\000"
.L.str86:
  .ascii "0 || 0\000"
.L.str87:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str88:
  .ascii "0 || 0\000"
.L.str89:
  .ascii "%s => %d\\n\000"
.L.str90:
  .ascii "1 || 0\000"
.L.str91:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str92:
  .ascii "1 || 0\000"
.L.str93:
  .ascii "%s => %d\\n\000"
.L.str94:
  .ascii "0 || 1\000"
.L.str95:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str96:
  .ascii "0 || 1\000"
.L.str97:
  .ascii "%s => %d\\n\000"
.L.str98:
  .ascii "1 || 1\000"
.L.str99:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str100:
  .ascii "1 || 1\000"
.L.str101:
  .ascii "%s => %d\\n\000"
.L.str102:
  .ascii "0 && 0\000"
.L.str103:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str104:
  .ascii "0 && 0\000"
.L.str105:
  .ascii "%s => %d\\n\000"
.L.str106:
  .ascii "1 && 0\000"
.L.str107:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str108:
  .ascii "1 && 0\000"
.L.str109:
  .ascii "%s => %d\\n\000"
.L.str110:
  .ascii "0 && 1\000"
.L.str111:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str112:
  .ascii "0 && 1\000"
.L.str113:
  .ascii "%s => %d\\n\000"
.L.str114:
  .ascii "1 && 1\000"
.L.str115:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str116:
  .ascii "1 && 1\000"
.L.str117:
  .ascii "%s => %d\\n\000"
.L.str118:
  .ascii "0 < 0\000"
.L.str119:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str120:
  .ascii "0 < 0\000"
.L.str121:
  .ascii "%s => %d\\n\000"
.L.str122:
  .ascii "1 < 0\000"
.L.str123:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str124:
  .ascii "1 < 0\000"
.L.str125:
  .ascii "%s => %d\\n\000"
.L.str126:
  .ascii "0 < 1\000"
.L.str127:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str128:
  .ascii "0 < 1\000"
.L.str129:
  .ascii "%s => %d\\n\000"
.L.str130:
  .ascii "0 > 0\000"
.L.str131:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str132:
  .ascii "0 > 0\000"
.L.str133:
  .ascii "%s => %d\\n\000"
.L.str134:
  .ascii "0 > 1\000"
.L.str135:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str136:
  .ascii "0 > 1\000"
.L.str137:
  .ascii "%s => %d\\n\000"
.L.str138:
  .ascii "1 > 0\000"
.L.str139:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str140:
  .ascii "1 > 0\000"
.L.str141:
  .ascii "%s => %d\\n\000"
.L.str142:
  .ascii "4 == 5\000"
.L.str143:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str144:
  .ascii "4 == 5\000"
.L.str145:
  .ascii "%s => %d\\n\000"
.L.str146:
  .ascii "5 == 5\000"
.L.str147:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str148:
  .ascii "5 == 5\000"
.L.str149:
  .ascii "%s => %d\\n\000"
.L.str150:
  .ascii "4 != 5\000"
.L.str151:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str152:
  .ascii "4 != 5\000"
.L.str153:
  .ascii "%s => %d\\n\000"
.L.str154:
  .ascii "5 != 5\000"
.L.str155:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str156:
  .ascii "5 != 5\000"
.L.str157:
  .ascii "%s => %d\\n\000"
.L.str158:
  .ascii "({ int x=0; int y=0; do { y=y+x; x=x+1; } while (x < 10); return y; })\000"
.L.str159:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str160:
  .ascii "({ int x=0; int y=0; do { y=y+x; x=x+1; } while (x < 10); return y; })\000"
.L.str161:
  .ascii "%s => %d\\n\000"
.L.str162:
  .ascii "({ int sum=0; int i; for (i=10; i<15; i=i+1) sum = sum + i; return sum;})\000"
.L.str163:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str164:
  .ascii "({ int sum=0; int i; for (i=10; i<15; i=i+1) sum = sum + i; return sum;})\000"
.L.str165:
  .ascii "%s => %d\\n\000"
.L.str166:
  .ascii "({ int i=1; int j=1; for (int k=0; k<10; k=k+1) { int m=i+j; i=j; j=m; } return i;})\000"
.L.str167:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str168:
  .ascii "({ int i=1; int j=1; for (int k=0; k<10; k=k+1) { int m=i+j; i=j; j=m; } return i;})\000"
.L.str169:
  .ascii "%s => %d\\n\000"
.L.str170:
  .ascii "({ int ary[2]; *ary=1; *(ary+1)=2; return *ary + *(ary+1);})\000"
.L.str171:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str172:
  .ascii "({ int ary[2]; *ary=1; *(ary+1)=2; return *ary + *(ary+1);})\000"
.L.str173:
  .ascii "%s => %d\\n\000"
.L.str174:
  .ascii "({ int x; int *p = &x; x = 5; return *p;})\000"
.L.str175:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str176:
  .ascii "({ int x; int *p = &x; x = 5; return *p;})\000"
.L.str177:
  .ascii "%s => %d\\n\000"
.L.str178:
  .ascii "({ int ary[2]; ary[0]=1; ary[1]=2; return ary[0] + ary[0+1];})\000"
.L.str179:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str180:
  .ascii "({ int ary[2]; ary[0]=1; ary[1]=2; return ary[0] + ary[0+1];})\000"
.L.str181:
  .ascii "%s => %d\\n\000"
.L.str182:
  .ascii "({ int x; int *p = &x; x = 5; return p[0];})\000"
.L.str183:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str184:
  .ascii "({ int x; int *p = &x; x = 5; return p[0];})\000"
.L.str185:
  .ascii "%s => %d\\n\000"
.L.str186:
  .ascii "({ char x; return sizeof x; })\000"
.L.str187:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str188:
  .ascii "({ char x; return sizeof x; })\000"
.L.str189:
  .ascii "%s => %d\\n\000"
.L.str190:
  .ascii "({ int x; return sizeof(x); })\000"
.L.str191:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str192:
  .ascii "({ int x; return sizeof(x); })\000"
.L.str193:
  .ascii "%s => %d\\n\000"
.L.str194:
  .ascii "({ int *x; return sizeof x; })\000"
.L.str195:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str196:
  .ascii "({ int *x; return sizeof x; })\000"
.L.str197:
  .ascii "%s => %d\\n\000"
.L.str198:
  .ascii "({ int x[4]; return sizeof x; })\000"
.L.str199:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str200:
  .ascii "({ int x[4]; return sizeof x; })\000"
.L.str201:
  .ascii "%s => %d\\n\000"
.L.str202:
  .ascii "({ char x = 5; return x; })\000"
.L.str203:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str204:
  .ascii "({ char x = 5; return x; })\000"
.L.str205:
  .ascii "%s => %d\\n\000"
.L.str206:
  .ascii "({ int x = 0; char *p = &x; p[0] = 42; return x; })\000"
.L.str207:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str208:
  .ascii "({ int x = 0; char *p = &x; p[0] = 42; return x; })\000"
.L.str209:
  .ascii "abc\000"
.L.str210:
  .ascii "%s => %d\\n\000"
.L.str211:
  .ascii "({ char *p = \\abc\\; return p[0]; })\000"
.L.str212:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str213:
  .ascii "({ char *p = \\abc\\; return p[0]; })\000"
.L.str214:
  .ascii "abc\000"
.L.str215:
  .ascii "%s => %d\\n\000"
.L.str216:
  .ascii "({ char *p = \\abc\\; return p[1]; })\000"
.L.str217:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str218:
  .ascii "({ char *p = \\abc\\; return p[1]; })\000"
.L.str219:
  .ascii "abc\000"
.L.str220:
  .ascii "%s => %d\\n\000"
.L.str221:
  .ascii "({ char *p = \\abc\\; return p[2]; })\000"
.L.str222:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str223:
  .ascii "({ char *p = \\abc\\; return p[2]; })\000"
.L.str224:
  .ascii "abc\000"
.L.str225:
  .ascii "%s => %d\\n\000"
.L.str226:
  .ascii "({ char *p = \\abc\\; return p[3]; })\000"
.L.str227:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str228:
  .ascii "({ char *p = \\abc\\; return p[3]; })\000"
.L.str229:
  .ascii "%s => %d\\n\000"
.L.str230:
  .ascii "({ int x = 1; { int x = 2; } return x; })\000"
.L.str231:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str232:
  .ascii "({ int x = 1; { int x = 2; } return x; })\000"
.L.str233:
  .ascii "%s => %d\\n\000"
.L.str234:
  .ascii "var1\000"
.L.str235:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str236:
  .ascii "var1\000"
.L.str237:
  .ascii "%s => %d\\n\000"
.L.str238:
  .ascii "({ var1 = 5; return var1; })\000"
.L.str239:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str240:
  .ascii "({ var1 = 5; return var1; })\000"
.L.str241:
  .ascii "%s => %d\\n\000"
.L.str242:
  .ascii "sizeof(var2)\000"
.L.str243:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str244:
  .ascii "sizeof(var2)\000"
.L.str245:
  .ascii "%s => %d\\n\000"
.L.str246:
  .ascii "({ var2[0] = 5; var2[4] = 10; return var2[0] + var2[4]; })\000"
.L.str247:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str248:
  .ascii "({ var2[0] = 5; var2[4] = 10; return var2[0] + var2[4]; })\000"
.L.str249:
  .ascii "%s => %d\\n\000"
.L.str250:
  .ascii "global_arr[0]\000"
.L.str251:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str252:
  .ascii "global_arr[0]\000"
.L.str253:
  .ascii "%s => %d\\n\000"
.L.str254:
  .ascii "({ return 3 + ({ return 5; }); })\000"
.L.str255:
  .ascii "%d: %s: %d expected, but got %d\\n\000"
.L.str256:
  .ascii "({ return 3 + ({ return 5; }); })\000"
.L.str257:
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
	sub rsp, 702
	push r12
	push r13
	push r14
	push r15
.L2:
	mov r10, 0
	mov r11, rbp
	sub r11, 44
	mov [r11], r10d
	mov r10, 0
	mov r11, rbp
	sub r11, 48
	mov [r11], r10d
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
	je .L4
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
.L4:
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
.L3:
	mov r10, 0
	cmp r10, 0
	jne .L2
.L6:
	mov r10, 1
	mov r11, rbp
	sub r11, 52
	mov [r11], r10d
	mov r10, 1
	mov r11, rbp
	sub r11, 56
	mov [r11], r10d
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
	je .L8
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
	jmp .L7
.L8:
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
	jne .L6
.L10:
	mov r10, 2
	mov r11, rbp
	sub r11, 60
	mov [r11], r10d
	mov r10, 1
	mov r11, 1
	add r10, r11
	mov r11, rbp
	sub r11, 64
	mov [r11], r10d
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
	je .L12
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
	jmp .L11
.L12:
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
.L11:
	mov r10, 0
	cmp r10, 0
	jne .L10
.L14:
	mov r10, 10
	mov r11, rbp
	sub r11, 68
	mov [r11], r10d
	mov r10, 2
	mov r11, 3
	mov rax, r11
	mul r10
	mov r10, rax
	mov r11, 4
	add r10, r11
	mov r11, rbp
	sub r11, 72
	mov [r11], r10d
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
	je .L16
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
	jmp .L15
.L16:
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
.L15:
	mov r10, 0
	cmp r10, 0
	jne .L14
.L18:
	mov r10, 26
	mov r11, rbp
	sub r11, 76
	mov [r11], r10d
	mov r10, 2
	mov r11, 3
	mov rax, r11
	mul r10
	mov r10, rax
	mov r11, 4
	mov rbx, 5
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, rbp
	sub r11, 80
	mov [r11], r10d
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
	je .L20
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
	jmp .L19
.L20:
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
.L19:
	mov r10, 0
	cmp r10, 0
	jne .L18
.L22:
	mov r10, 5
	mov r11, rbp
	sub r11, 84
	mov [r11], r10d
	mov r10, 50
	mov r11, 10
	mov rax, r10
	cqo
	div r11
	mov r10, rax
	mov r11, rbp
	sub r11, 88
	mov [r11], r10d
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
	je .L24
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
	jmp .L23
.L24:
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
.L23:
	mov r10, 0
	cmp r10, 0
	jne .L22
.L26:
	mov r10, 9
	mov r11, rbp
	sub r11, 92
	mov [r11], r10d
	mov r10, 6
	mov r11, 3
	mov rax, r11
	mul r10
	mov r10, rax
	mov r11, 2
	mov rax, r10
	cqo
	div r11
	mov r10, rax
	mov r11, rbp
	sub r11, 96
	mov [r11], r10d
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
	je .L28
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
	jmp .L27
.L28:
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
.L27:
	mov r10, 0
	cmp r10, 0
	jne .L26
.L30:
	mov r10, 45
	mov r11, rbp
	sub r11, 100
	mov [r11], r10d
	mov r10, 2
	mov r11, 3
	add r10, r11
	mov r11, 4
	mov rbx, 5
	add r11, rbx
	mov rax, r11
	mul r10
	mov r10, rax
	mov r11, rbp
	sub r11, 104
	mov [r11], r10d
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
	je .L32
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
	jmp .L31
.L32:
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
.L31:
	mov r10, 0
	cmp r10, 0
	jne .L30
.L34:
	mov r10, 153
	mov r11, rbp
	sub r11, 108
	mov [r11], r10d
	mov r10, 1
	mov r11, 2
	add r10, r11
	mov r11, 3
	add r10, r11
	mov r11, 4
	add r10, r11
	mov r11, 5
	add r10, r11
	mov r11, 6
	add r10, r11
	mov r11, 7
	add r10, r11
	mov r11, 8
	add r10, r11
	mov r11, 9
	add r10, r11
	mov r11, 10
	add r10, r11
	mov r11, 11
	add r10, r11
	mov r11, 12
	add r10, r11
	mov r11, 13
	add r10, r11
	mov r11, 14
	add r10, r11
	mov r11, 15
	add r10, r11
	mov r11, 16
	add r10, r11
	mov r11, 17
	add r10, r11
	mov r11, rbp
	sub r11, 112
	mov [r11], r10d
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
	je .L36
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
	jmp .L35
.L36:
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
.L35:
	mov r10, 0
	cmp r10, 0
	jne .L34
.L38:
	mov r10, 2
	mov r11, rbp
	sub r11, 116
	mov [r11], r10d
	mov r10, 2
	mov r11, rbp
	sub r11, 120
	mov [r11], r10d
	mov r10, rbp
	sub r10, 120
	mov r10d, [r10]
	mov r11, r10
	jmp .L39
.L39:
	mov r10, rbp
	sub r10, 124
	mov [r10], r11d
	mov r10, rbp
	sub r10, 116
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 124
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L41
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str41
	lea rbx, .L.str42
	mov r12, rbp
	sub r12, 124
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
	jmp .L40
.L41:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str43
	mov rbx, 37
	lea r12, .L.str44
	mov r13, rbp
	sub r13, 116
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 124
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
.L40:
	mov r10, 0
	cmp r10, 0
	jne .L38
.L43:
	mov r10, 10
	mov r11, rbp
	sub r11, 128
	mov [r11], r10d
	mov r10, 2
	mov r11, rbp
	sub r11, 132
	mov [r11], r10d
	mov r10, rbp
	sub r10, 136
	mov r11, 3
	mov rbx, 2
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 132
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 136
	mov r11d, [r11]
	mov rax, r11
	mul r10
	mov r10, rax
	mov r11, r10
	jmp .L44
.L44:
	mov r10, rbp
	sub r10, 140
	mov [r10], r11d
	mov r10, rbp
	sub r10, 128
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 140
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L46
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str45
	lea rbx, .L.str46
	mov r12, rbp
	sub r12, 140
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
	jmp .L45
.L46:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str47
	mov rbx, 38
	lea r12, .L.str48
	mov r13, rbp
	sub r13, 128
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 140
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
.L45:
	mov r10, 0
	cmp r10, 0
	jne .L43
.L48:
	mov r10, 2
	mov r11, rbp
	sub r11, 144
	mov [r11], r10d
	mov r10, 1
	cmp r10, 0
	je .L51
	mov r10, 2
	mov r11, r10
	jmp .L49
.L51:
	mov r10, 3
	mov r11, r10
	jmp .L49
.L49:
	mov r10, rbp
	sub r10, 148
	mov [r10], r11d
	mov r10, rbp
	sub r10, 144
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 148
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L53
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str49
	lea rbx, .L.str50
	mov r12, rbp
	sub r12, 148
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
	jmp .L52
.L53:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str51
	mov rbx, 39
	lea r12, .L.str52
	mov r13, rbp
	sub r13, 144
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 148
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
.L52:
	mov r10, 0
	cmp r10, 0
	jne .L48
.L55:
	mov r10, 3
	mov r11, rbp
	sub r11, 152
	mov [r11], r10d
	mov r10, 0
	cmp r10, 0
	je .L58
	mov r10, 2
	mov r11, r10
	jmp .L56
.L58:
	mov r10, 3
	mov r11, r10
	jmp .L56
.L56:
	mov r10, rbp
	sub r10, 156
	mov [r10], r11d
	mov r10, rbp
	sub r10, 152
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 156
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L60
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str53
	lea rbx, .L.str54
	mov r12, rbp
	sub r12, 156
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
	jmp .L59
.L60:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str55
	mov rbx, 40
	lea r12, .L.str56
	mov r13, rbp
	sub r13, 152
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 156
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
.L59:
	mov r10, 0
	cmp r10, 0
	jne .L55
.L62:
	mov r10, 2
	mov r11, rbp
	sub r11, 160
	mov [r11], r10d
	mov r10, 1
	cmp r10, 0
	je .L65
	mov r10, 2
	mov r11, r10
	jmp .L63
	jmp .L64
.L65:
	mov r10, 3
	mov r11, r10
	jmp .L63
.L64:
.L63:
	mov r10, rbp
	sub r10, 164
	mov [r10], r11d
	mov r10, rbp
	sub r10, 160
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 164
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L68
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str57
	lea rbx, .L.str58
	mov r12, rbp
	sub r12, 164
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
	jmp .L67
.L68:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str59
	mov rbx, 41
	lea r12, .L.str60
	mov r13, rbp
	sub r13, 160
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 164
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
.L67:
	mov r10, 0
	cmp r10, 0
	jne .L62
.L70:
	mov r10, 3
	mov r11, rbp
	sub r11, 168
	mov [r11], r10d
	mov r10, 0
	cmp r10, 0
	je .L73
	mov r10, 2
	mov r11, r10
	jmp .L71
	jmp .L72
.L73:
	mov r10, 3
	mov r11, r10
	jmp .L71
.L72:
.L71:
	mov r10, rbp
	sub r10, 172
	mov [r10], r11d
	mov r10, rbp
	sub r10, 168
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 172
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L76
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str61
	lea rbx, .L.str62
	mov r12, rbp
	sub r12, 172
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
	jmp .L75
.L76:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str63
	mov rbx, 42
	lea r12, .L.str64
	mov r13, rbp
	sub r13, 168
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 172
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
.L75:
	mov r10, 0
	cmp r10, 0
	jne .L70
.L78:
	mov r10, 5
	mov r11, rbp
	sub r11, 176
	mov [r11], r10d
	mov r10, 2
	mov r11, 3
	mov rdi, r10
	mov rsi, r11
	push r10
	push r11
	mov rax, 0
	call plus
	pop r11
	pop r10
	mov rbx, rax
	mov r10, rbp
	sub r10, 180
	mov [r10], ebx
	mov r10, rbp
	sub r10, 176
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 180
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L80
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str65
	lea rbx, .L.str66
	mov r12, rbp
	sub r12, 180
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
	jmp .L79
.L80:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str67
	mov rbx, 44
	lea r12, .L.str68
	mov r13, rbp
	sub r13, 176
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 180
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
.L79:
	mov r10, 0
	cmp r10, 0
	jne .L78
.L82:
	mov r10, 1
	mov r11, rbp
	sub r11, 184
	mov [r11], r10d
	push r10
	push r11
	mov rax, 0
	call one
	pop r11
	pop r10
	mov r10, rax
	mov r11, rbp
	sub r11, 188
	mov [r11], r10d
	mov r10, rbp
	sub r10, 184
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 188
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L84
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str69
	lea rbx, .L.str70
	mov r12, rbp
	sub r12, 188
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
	jmp .L83
.L84:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str71
	mov rbx, 45
	lea r12, .L.str72
	mov r13, rbp
	sub r13, 184
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 188
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
.L83:
	mov r10, 0
	cmp r10, 0
	jne .L82
.L86:
	mov r10, 3
	mov r11, rbp
	sub r11, 192
	mov [r11], r10d
	push r10
	push r11
	mov rax, 0
	call one
	pop r11
	pop r10
	mov r10, rax
	push r10
	push r11
	mov rax, 0
	call two
	pop r11
	pop r10
	mov r11, rax
	add r10, r11
	mov r11, rbp
	sub r11, 196
	mov [r11], r10d
	mov r10, rbp
	sub r10, 192
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 196
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L88
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str73
	lea rbx, .L.str74
	mov r12, rbp
	sub r12, 196
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
	jmp .L87
.L88:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str75
	mov rbx, 46
	lea r12, .L.str76
	mov r13, rbp
	sub r13, 192
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 196
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
.L87:
	mov r10, 0
	cmp r10, 0
	jne .L86
.L90:
	mov r10, 6
	mov r11, rbp
	sub r11, 200
	mov [r11], r10d
	mov r10, 2
	mov r11, 3
	mov rdi, r10
	mov rsi, r11
	push r10
	push r11
	mov rax, 0
	call mul
	pop r11
	pop r10
	mov rbx, rax
	mov r10, rbp
	sub r10, 204
	mov [r10], ebx
	mov r10, rbp
	sub r10, 200
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 204
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L92
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str77
	lea rbx, .L.str78
	mov r12, rbp
	sub r12, 204
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
	jmp .L91
.L92:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str79
	mov rbx, 47
	lea r12, .L.str80
	mov r13, rbp
	sub r13, 200
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 204
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
.L91:
	mov r10, 0
	cmp r10, 0
	jne .L90
.L94:
	mov r10, 21
	mov r11, rbp
	sub r11, 208
	mov [r11], r10d
	mov r10, 1
	mov r11, 2
	mov rbx, 3
	mov r12, 4
	mov r13, 5
	mov r14, 6
	mov rdi, r10
	mov rsi, r11
	mov rdx, rbx
	mov rcx, r12
	mov r8, r13
	mov r9, r14
	push r10
	push r11
	mov rax, 0
	call add
	pop r11
	pop r10
	mov r15, rax
	mov r10, rbp
	sub r10, 212
	mov [r10], r15d
	mov r10, rbp
	sub r10, 208
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 212
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L96
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str81
	lea rbx, .L.str82
	mov r12, rbp
	sub r12, 212
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
	jmp .L95
.L96:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str83
	mov rbx, 48
	lea r12, .L.str84
	mov r13, rbp
	sub r13, 208
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 212
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
.L95:
	mov r10, 0
	cmp r10, 0
	jne .L94
.L98:
	mov r10, 0
	mov r11, rbp
	sub r11, 216
	mov [r11], r10d
	mov r10, 0
	cmp r10, 0
	je .L99
	mov r10, 1
	jmp .L100
.L99:
	mov r11, 0
	mov r10, r11
	cmp r10, 0
	je .L100
	mov r10, 1
.L100:
	mov r11, rbp
	sub r11, 220
	mov [r11], r10d
	mov r10, rbp
	sub r10, 216
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 220
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L102
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str85
	lea rbx, .L.str86
	mov r12, rbp
	sub r12, 220
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
	jmp .L101
.L102:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str87
	mov rbx, 50
	lea r12, .L.str88
	mov r13, rbp
	sub r13, 216
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 220
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
.L101:
	mov r10, 0
	cmp r10, 0
	jne .L98
.L104:
	mov r10, 1
	mov r11, rbp
	sub r11, 224
	mov [r11], r10d
	mov r10, 1
	cmp r10, 0
	je .L105
	mov r10, 1
	jmp .L106
.L105:
	mov r11, 0
	mov r10, r11
	cmp r10, 0
	je .L106
	mov r10, 1
.L106:
	mov r11, rbp
	sub r11, 228
	mov [r11], r10d
	mov r10, rbp
	sub r10, 224
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 228
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L108
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str89
	lea rbx, .L.str90
	mov r12, rbp
	sub r12, 228
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
	jmp .L107
.L108:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str91
	mov rbx, 51
	lea r12, .L.str92
	mov r13, rbp
	sub r13, 224
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 228
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
.L107:
	mov r10, 0
	cmp r10, 0
	jne .L104
.L110:
	mov r10, 1
	mov r11, rbp
	sub r11, 232
	mov [r11], r10d
	mov r10, 0
	cmp r10, 0
	je .L111
	mov r10, 1
	jmp .L112
.L111:
	mov r11, 1
	mov r10, r11
	cmp r10, 0
	je .L112
	mov r10, 1
.L112:
	mov r11, rbp
	sub r11, 236
	mov [r11], r10d
	mov r10, rbp
	sub r10, 232
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 236
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L114
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str93
	lea rbx, .L.str94
	mov r12, rbp
	sub r12, 236
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
	jmp .L113
.L114:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str95
	mov rbx, 52
	lea r12, .L.str96
	mov r13, rbp
	sub r13, 232
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 236
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
.L113:
	mov r10, 0
	cmp r10, 0
	jne .L110
.L116:
	mov r10, 1
	mov r11, rbp
	sub r11, 240
	mov [r11], r10d
	mov r10, 1
	cmp r10, 0
	je .L117
	mov r10, 1
	jmp .L118
.L117:
	mov r11, 1
	mov r10, r11
	cmp r10, 0
	je .L118
	mov r10, 1
.L118:
	mov r11, rbp
	sub r11, 244
	mov [r11], r10d
	mov r10, rbp
	sub r10, 240
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 244
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L120
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str97
	lea rbx, .L.str98
	mov r12, rbp
	sub r12, 244
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
	jmp .L119
.L120:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str99
	mov rbx, 53
	lea r12, .L.str100
	mov r13, rbp
	sub r13, 240
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 244
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
.L119:
	mov r10, 0
	cmp r10, 0
	jne .L116
.L122:
	mov r10, 0
	mov r11, rbp
	sub r11, 248
	mov [r11], r10d
	mov r10, 0
	cmp r10, 0
	je .L123
	mov r11, 0
	mov r10, r11
	cmp r10, 0
	je .L123
	mov r10, 1
.L123:
	mov r11, rbp
	sub r11, 252
	mov [r11], r10d
	mov r10, rbp
	sub r10, 248
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 252
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L125
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str101
	lea rbx, .L.str102
	mov r12, rbp
	sub r12, 252
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
	jmp .L124
.L125:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str103
	mov rbx, 55
	lea r12, .L.str104
	mov r13, rbp
	sub r13, 248
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 252
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
.L124:
	mov r10, 0
	cmp r10, 0
	jne .L122
.L127:
	mov r10, 0
	mov r11, rbp
	sub r11, 256
	mov [r11], r10d
	mov r10, 1
	cmp r10, 0
	je .L128
	mov r11, 0
	mov r10, r11
	cmp r10, 0
	je .L128
	mov r10, 1
.L128:
	mov r11, rbp
	sub r11, 260
	mov [r11], r10d
	mov r10, rbp
	sub r10, 256
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 260
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L130
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str105
	lea rbx, .L.str106
	mov r12, rbp
	sub r12, 260
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
	jmp .L129
.L130:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str107
	mov rbx, 56
	lea r12, .L.str108
	mov r13, rbp
	sub r13, 256
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 260
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
.L129:
	mov r10, 0
	cmp r10, 0
	jne .L127
.L132:
	mov r10, 0
	mov r11, rbp
	sub r11, 264
	mov [r11], r10d
	mov r10, 0
	cmp r10, 0
	je .L133
	mov r11, 1
	mov r10, r11
	cmp r10, 0
	je .L133
	mov r10, 1
.L133:
	mov r11, rbp
	sub r11, 268
	mov [r11], r10d
	mov r10, rbp
	sub r10, 264
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 268
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L135
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str109
	lea rbx, .L.str110
	mov r12, rbp
	sub r12, 268
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
	jmp .L134
.L135:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str111
	mov rbx, 57
	lea r12, .L.str112
	mov r13, rbp
	sub r13, 264
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 268
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
.L134:
	mov r10, 0
	cmp r10, 0
	jne .L132
.L137:
	mov r10, 1
	mov r11, rbp
	sub r11, 272
	mov [r11], r10d
	mov r10, 1
	cmp r10, 0
	je .L138
	mov r11, 1
	mov r10, r11
	cmp r10, 0
	je .L138
	mov r10, 1
.L138:
	mov r11, rbp
	sub r11, 276
	mov [r11], r10d
	mov r10, rbp
	sub r10, 272
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 276
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L140
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str113
	lea rbx, .L.str114
	mov r12, rbp
	sub r12, 276
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
	jmp .L139
.L140:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str115
	mov rbx, 58
	lea r12, .L.str116
	mov r13, rbp
	sub r13, 272
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 276
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
.L139:
	mov r10, 0
	cmp r10, 0
	jne .L137
.L142:
	mov r10, 0
	mov r11, rbp
	sub r11, 280
	mov [r11], r10d
	mov r10, 0
	mov r11, 0
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 284
	mov [r11], r10d
	mov r10, rbp
	sub r10, 280
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 284
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L144
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str117
	lea rbx, .L.str118
	mov r12, rbp
	sub r12, 284
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
	jmp .L143
.L144:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str119
	mov rbx, 60
	lea r12, .L.str120
	mov r13, rbp
	sub r13, 280
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 284
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
.L143:
	mov r10, 0
	cmp r10, 0
	jne .L142
.L146:
	mov r10, 0
	mov r11, rbp
	sub r11, 288
	mov [r11], r10d
	mov r10, 1
	mov r11, 0
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 292
	mov [r11], r10d
	mov r10, rbp
	sub r10, 288
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 292
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L148
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str121
	lea rbx, .L.str122
	mov r12, rbp
	sub r12, 292
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
	jmp .L147
.L148:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str123
	mov rbx, 61
	lea r12, .L.str124
	mov r13, rbp
	sub r13, 288
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 292
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
.L147:
	mov r10, 0
	cmp r10, 0
	jne .L146
.L150:
	mov r10, 1
	mov r11, rbp
	sub r11, 296
	mov [r11], r10d
	mov r10, 0
	mov r11, 1
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 300
	mov [r11], r10d
	mov r10, rbp
	sub r10, 296
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 300
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L152
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str125
	lea rbx, .L.str126
	mov r12, rbp
	sub r12, 300
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
	jmp .L151
.L152:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str127
	mov rbx, 62
	lea r12, .L.str128
	mov r13, rbp
	sub r13, 296
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 300
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
.L151:
	mov r10, 0
	cmp r10, 0
	jne .L150
.L154:
	mov r10, 0
	mov r11, rbp
	sub r11, 304
	mov [r11], r10d
	mov r10, 0
	mov r11, 0
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 308
	mov [r11], r10d
	mov r10, rbp
	sub r10, 304
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 308
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L156
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str129
	lea rbx, .L.str130
	mov r12, rbp
	sub r12, 308
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
	jmp .L155
.L156:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str131
	mov rbx, 63
	lea r12, .L.str132
	mov r13, rbp
	sub r13, 304
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 308
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
.L155:
	mov r10, 0
	cmp r10, 0
	jne .L154
.L158:
	mov r10, 0
	mov r11, rbp
	sub r11, 312
	mov [r11], r10d
	mov r10, 1
	mov r11, 0
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 316
	mov [r11], r10d
	mov r10, rbp
	sub r10, 312
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 316
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L160
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str133
	lea rbx, .L.str134
	mov r12, rbp
	sub r12, 316
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
	jmp .L159
.L160:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str135
	mov rbx, 64
	lea r12, .L.str136
	mov r13, rbp
	sub r13, 312
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 316
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
.L159:
	mov r10, 0
	cmp r10, 0
	jne .L158
.L162:
	mov r10, 1
	mov r11, rbp
	sub r11, 320
	mov [r11], r10d
	mov r10, 0
	mov r11, 1
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 324
	mov [r11], r10d
	mov r10, rbp
	sub r10, 320
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 324
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L164
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str137
	lea rbx, .L.str138
	mov r12, rbp
	sub r12, 324
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
	jmp .L163
.L164:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str139
	mov rbx, 65
	lea r12, .L.str140
	mov r13, rbp
	sub r13, 320
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 324
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
.L163:
	mov r10, 0
	cmp r10, 0
	jne .L162
.L166:
	mov r10, 0
	mov r11, rbp
	sub r11, 328
	mov [r11], r10d
	mov r10, 4
	mov r11, 5
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 332
	mov [r11], r10d
	mov r10, rbp
	sub r10, 328
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 332
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L168
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str141
	lea rbx, .L.str142
	mov r12, rbp
	sub r12, 332
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
	jmp .L167
.L168:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str143
	mov rbx, 67
	lea r12, .L.str144
	mov r13, rbp
	sub r13, 328
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 332
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
.L167:
	mov r10, 0
	cmp r10, 0
	jne .L166
.L170:
	mov r10, 1
	mov r11, rbp
	sub r11, 336
	mov [r11], r10d
	mov r10, 5
	mov r11, 5
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 340
	mov [r11], r10d
	mov r10, rbp
	sub r10, 336
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 340
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L172
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str145
	lea rbx, .L.str146
	mov r12, rbp
	sub r12, 340
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
	jmp .L171
.L172:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str147
	mov rbx, 68
	lea r12, .L.str148
	mov r13, rbp
	sub r13, 336
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 340
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
.L171:
	mov r10, 0
	cmp r10, 0
	jne .L170
.L174:
	mov r10, 1
	mov r11, rbp
	sub r11, 344
	mov [r11], r10d
	mov r10, 4
	mov r11, 5
	cmp r10, r11
	setne r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 348
	mov [r11], r10d
	mov r10, rbp
	sub r10, 344
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 348
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L176
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str149
	lea rbx, .L.str150
	mov r12, rbp
	sub r12, 348
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
	jmp .L175
.L176:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str151
	mov rbx, 69
	lea r12, .L.str152
	mov r13, rbp
	sub r13, 344
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 348
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
.L175:
	mov r10, 0
	cmp r10, 0
	jne .L174
.L178:
	mov r10, 0
	mov r11, rbp
	sub r11, 352
	mov [r11], r10d
	mov r10, 5
	mov r11, 5
	cmp r10, r11
	setne r10b
	movzb r10, r10b
	mov r11, rbp
	sub r11, 356
	mov [r11], r10d
	mov r10, rbp
	sub r10, 352
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 356
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L180
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str153
	lea rbx, .L.str154
	mov r12, rbp
	sub r12, 356
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
	jmp .L179
.L180:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str155
	mov rbx, 70
	lea r12, .L.str156
	mov r13, rbp
	sub r13, 352
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 356
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
.L179:
	mov r10, 0
	cmp r10, 0
	jne .L178
.L182:
	mov r10, 45
	mov r11, rbp
	sub r11, 360
	mov [r11], r10d
	mov r10, 0
	mov r11, rbp
	sub r11, 364
	mov [r11], r10d
	mov r10, 0
	mov r11, rbp
	sub r11, 368
	mov [r11], r10d
.L184:
	mov r10, rbp
	sub r10, 368
	mov r11, rbp
	sub r11, 368
	mov r11d, [r11]
	mov rbx, rbp
	sub rbx, 364
	mov ebx, [rbx]
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 364
	mov r11, rbp
	sub r11, 364
	mov r11d, [r11]
	mov rbx, 1
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 364
	mov r10d, [r10]
	mov r11, 10
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	cmp r10, 0
	jne .L184
	mov r10, rbp
	sub r10, 368
	mov r10d, [r10]
	mov r11, r10
	jmp .L183
.L183:
	mov r10, rbp
	sub r10, 372
	mov [r10], r11d
	mov r10, rbp
	sub r10, 360
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 372
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L186
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str157
	lea rbx, .L.str158
	mov r12, rbp
	sub r12, 372
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
	jmp .L185
.L186:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str159
	mov rbx, 72
	lea r12, .L.str160
	mov r13, rbp
	sub r13, 360
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 372
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
.L185:
	mov r10, 0
	cmp r10, 0
	jne .L182
.L188:
	mov r10, 60
	mov r11, rbp
	sub r11, 376
	mov [r11], r10d
	mov r10, 0
	mov r11, rbp
	sub r11, 380
	mov [r11], r10d
	mov r10, rbp
	sub r10, 384
	mov r11, 10
	mov [r10], r11d
.L190:
	mov r10, rbp
	sub r10, 384
	mov r10d, [r10]
	mov r11, 15
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	cmp r10, 0
	je .L191
	mov r10, rbp
	sub r10, 380
	mov r11, rbp
	sub r11, 380
	mov r11d, [r11]
	mov rbx, rbp
	sub rbx, 384
	mov ebx, [rbx]
	add r11, rbx
	mov [r10], r11d
	mov r10, rbp
	sub r10, 384
	mov r11, rbp
	sub r11, 384
	mov r11d, [r11]
	mov rbx, 1
	add r11, rbx
	mov [r10], r11d
	jmp .L190
.L191:
	mov r10, rbp
	sub r10, 380
	mov r10d, [r10]
	mov r11, r10
	jmp .L189
.L189:
	mov r10, rbp
	sub r10, 388
	mov [r10], r11d
	mov r10, rbp
	sub r10, 376
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 388
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L193
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str161
	lea rbx, .L.str162
	mov r12, rbp
	sub r12, 388
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
	jmp .L192
.L193:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str163
	mov rbx, 74
	lea r12, .L.str164
	mov r13, rbp
	sub r13, 376
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 388
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
.L192:
	mov r10, 0
	cmp r10, 0
	jne .L188
.L195:
	mov r10, 89
	mov r11, rbp
	sub r11, 392
	mov [r11], r10d
	mov r10, 1
	mov r11, rbp
	sub r11, 396
	mov [r11], r10d
	mov r10, 1
	mov r11, rbp
	sub r11, 400
	mov [r11], r10d
	mov r10, 0
	mov r11, rbp
	sub r11, 404
	mov [r11], r10d
.L197:
	mov r10, rbp
	sub r10, 404
	mov r10d, [r10]
	mov r11, 10
	cmp r10, r11
	setl r10b
	movzb r10, r10b
	cmp r10, 0
	je .L198
	mov r10, rbp
	sub r10, 396
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 400
	mov r11d, [r11]
	add r10, r11
	mov r11, rbp
	sub r11, 408
	mov [r11], r10d
	mov r10, rbp
	sub r10, 396
	mov r11, rbp
	sub r11, 400
	mov r11d, [r11]
	mov [r10], r11d
	mov r10, rbp
	sub r10, 400
	mov r11, rbp
	sub r11, 408
	mov r11d, [r11]
	mov [r10], r11d
	mov r10, rbp
	sub r10, 404
	mov r11, rbp
	sub r11, 404
	mov r11d, [r11]
	mov rbx, 1
	add r11, rbx
	mov [r10], r11d
	jmp .L197
.L198:
	mov r10, rbp
	sub r10, 396
	mov r10d, [r10]
	mov r11, r10
	jmp .L196
.L196:
	mov r10, rbp
	sub r10, 412
	mov [r10], r11d
	mov r10, rbp
	sub r10, 392
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 412
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L200
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str165
	lea rbx, .L.str166
	mov r12, rbp
	sub r12, 412
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
	jmp .L199
.L200:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str167
	mov rbx, 75
	lea r12, .L.str168
	mov r13, rbp
	sub r13, 392
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 412
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
.L199:
	mov r10, 0
	cmp r10, 0
	jne .L195
.L202:
	mov r10, 3
	mov r11, rbp
	sub r11, 416
	mov [r11], r10d
	mov r10, rbp
	sub r10, 424
	mov r11, 1
	mov [r10], r11d
	mov r10, rbp
	sub r10, 424
	mov r11, 1
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 2
	mov [r10], r11d
	mov r10, rbp
	sub r10, 424
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 424
	mov rbx, 1
	mov r12, 4
	mov rax, r12
	mul rbx
	mov rbx, rax
	add r11, rbx
	mov r11d, [r11]
	add r10, r11
	mov r11, r10
	jmp .L203
.L203:
	mov r10, rbp
	sub r10, 428
	mov [r10], r11d
	mov r10, rbp
	sub r10, 416
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 428
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L205
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str169
	lea rbx, .L.str170
	mov r12, rbp
	sub r12, 428
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
	jmp .L204
.L205:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str171
	mov rbx, 77
	lea r12, .L.str172
	mov r13, rbp
	sub r13, 416
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 428
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
.L204:
	mov r10, 0
	cmp r10, 0
	jne .L202
.L207:
	mov r10, 5
	mov r11, rbp
	sub r11, 432
	mov [r11], r10d
	mov r10, rbp
	sub r10, 436
	mov r11, rbp
	sub r11, 444
	mov [r11], r10
	mov r10, rbp
	sub r10, 436
	mov r11, 5
	mov [r10], r11d
	mov r10, rbp
	sub r10, 444
	mov r10, [r10]
	mov r10d, [r10]
	mov r11, r10
	jmp .L208
.L208:
	mov r10, rbp
	sub r10, 448
	mov [r10], r11d
	mov r10, rbp
	sub r10, 432
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 448
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L210
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str173
	lea rbx, .L.str174
	mov r12, rbp
	sub r12, 448
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
	jmp .L209
.L210:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str175
	mov rbx, 78
	lea r12, .L.str176
	mov r13, rbp
	sub r13, 432
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 448
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
.L209:
	mov r10, 0
	cmp r10, 0
	jne .L207
.L212:
	mov r10, 3
	mov r11, rbp
	sub r11, 452
	mov [r11], r10d
	mov r10, rbp
	sub r10, 460
	mov r11, 0
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 1
	mov [r10], r11d
	mov r10, rbp
	sub r10, 460
	mov r11, 1
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 2
	mov [r10], r11d
	mov r10, rbp
	sub r10, 460
	mov r11, 0
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 460
	mov rbx, 0
	mov r12, 1
	add rbx, r12
	mov r12, 4
	mov rax, r12
	mul rbx
	mov rbx, rax
	add r11, rbx
	mov r11d, [r11]
	add r10, r11
	mov r11, r10
	jmp .L213
.L213:
	mov r10, rbp
	sub r10, 464
	mov [r10], r11d
	mov r10, rbp
	sub r10, 452
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 464
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L215
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str177
	lea rbx, .L.str178
	mov r12, rbp
	sub r12, 464
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
	jmp .L214
.L215:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str179
	mov rbx, 80
	lea r12, .L.str180
	mov r13, rbp
	sub r13, 452
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 464
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
.L214:
	mov r10, 0
	cmp r10, 0
	jne .L212
.L217:
	mov r10, 5
	mov r11, rbp
	sub r11, 468
	mov [r11], r10d
	mov r10, rbp
	sub r10, 472
	mov r11, rbp
	sub r11, 480
	mov [r11], r10
	mov r10, rbp
	sub r10, 472
	mov r11, 5
	mov [r10], r11d
	mov r10, rbp
	sub r10, 480
	mov r10, [r10]
	mov r11, 0
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10d, [r10]
	mov r11, r10
	jmp .L218
.L218:
	mov r10, rbp
	sub r10, 484
	mov [r10], r11d
	mov r10, rbp
	sub r10, 468
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 484
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L220
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str181
	lea rbx, .L.str182
	mov r12, rbp
	sub r12, 484
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
	jmp .L219
.L220:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str183
	mov rbx, 81
	lea r12, .L.str184
	mov r13, rbp
	sub r13, 468
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 484
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
.L219:
	mov r10, 0
	cmp r10, 0
	jne .L217
.L222:
	mov r10, 1
	mov r11, rbp
	sub r11, 488
	mov [r11], r10d
	mov r10, 1
	mov r11, r10
	jmp .L223
.L223:
	mov r10, rbp
	sub r10, 493
	mov [r10], r11d
	mov r10, rbp
	sub r10, 488
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 493
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L225
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str185
	lea rbx, .L.str186
	mov r12, rbp
	sub r12, 493
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
	jmp .L224
.L225:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str187
	mov rbx, 83
	lea r12, .L.str188
	mov r13, rbp
	sub r13, 488
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 493
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
.L224:
	mov r10, 0
	cmp r10, 0
	jne .L222
.L227:
	mov r10, 4
	mov r11, rbp
	sub r11, 497
	mov [r11], r10d
	mov r10, 4
	mov r11, r10
	jmp .L228
.L228:
	mov r10, rbp
	sub r10, 505
	mov [r10], r11d
	mov r10, rbp
	sub r10, 497
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 505
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L230
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str189
	lea rbx, .L.str190
	mov r12, rbp
	sub r12, 505
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
	jmp .L229
.L230:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str191
	mov rbx, 84
	lea r12, .L.str192
	mov r13, rbp
	sub r13, 497
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 505
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
.L229:
	mov r10, 0
	cmp r10, 0
	jne .L227
.L232:
	mov r10, 8
	mov r11, rbp
	sub r11, 509
	mov [r11], r10d
	mov r10, 8
	mov r11, r10
	jmp .L233
.L233:
	mov r10, rbp
	sub r10, 521
	mov [r10], r11d
	mov r10, rbp
	sub r10, 509
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 521
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L235
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str193
	lea rbx, .L.str194
	mov r12, rbp
	sub r12, 521
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
	jmp .L234
.L235:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str195
	mov rbx, 85
	lea r12, .L.str196
	mov r13, rbp
	sub r13, 509
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 521
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
.L234:
	mov r10, 0
	cmp r10, 0
	jne .L232
.L237:
	mov r10, 16
	mov r11, rbp
	sub r11, 525
	mov [r11], r10d
	mov r10, 16
	mov r11, r10
	jmp .L238
.L238:
	mov r10, rbp
	sub r10, 545
	mov [r10], r11d
	mov r10, rbp
	sub r10, 525
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 545
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L240
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str197
	lea rbx, .L.str198
	mov r12, rbp
	sub r12, 545
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
	jmp .L239
.L240:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str199
	mov rbx, 86
	lea r12, .L.str200
	mov r13, rbp
	sub r13, 525
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 545
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
.L239:
	mov r10, 0
	cmp r10, 0
	jne .L237
.L242:
	mov r10, 5
	mov r11, rbp
	sub r11, 549
	mov [r11], r10d
	mov r10, 5
	mov r11, rbp
	sub r11, 550
	mov [r11], r10b
	mov r10, rbp
	sub r10, 550
	mov r10b, [r10]
	movzb r10, r10b
	mov r11, r10
	jmp .L243
.L243:
	mov r10, rbp
	sub r10, 554
	mov [r10], r11d
	mov r10, rbp
	sub r10, 549
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 554
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L245
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str201
	lea rbx, .L.str202
	mov r12, rbp
	sub r12, 554
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
	jmp .L244
.L245:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str203
	mov rbx, 88
	lea r12, .L.str204
	mov r13, rbp
	sub r13, 549
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 554
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
.L244:
	mov r10, 0
	cmp r10, 0
	jne .L242
.L247:
	mov r10, 42
	mov r11, rbp
	sub r11, 558
	mov [r11], r10d
	mov r10, 0
	mov r11, rbp
	sub r11, 562
	mov [r11], r10d
	mov r10, rbp
	sub r10, 562
	mov r11, rbp
	sub r11, 570
	mov [r11], r10
	mov r10, rbp
	sub r10, 570
	mov r10, [r10]
	mov r11, 0
	mov rbx, 1
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 42
	mov [r10], r11b
	mov r10, rbp
	sub r10, 562
	mov r10d, [r10]
	mov r11, r10
	jmp .L248
.L248:
	mov r10, rbp
	sub r10, 574
	mov [r10], r11d
	mov r10, rbp
	sub r10, 558
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 574
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L250
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str205
	lea rbx, .L.str206
	mov r12, rbp
	sub r12, 574
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
	jmp .L249
.L250:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str207
	mov rbx, 89
	lea r12, .L.str208
	mov r13, rbp
	sub r13, 558
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 574
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
.L249:
	mov r10, 0
	cmp r10, 0
	jne .L247
.L252:
	mov r10, 97
	mov r11, rbp
	sub r11, 578
	mov [r11], r10d
	lea r10, .L.str209
	mov r11, rbp
	sub r11, 586
	mov [r11], r10
	mov r10, rbp
	sub r10, 586
	mov r10, [r10]
	mov r11, 0
	mov rbx, 1
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10b, [r10]
	movzb r10, r10b
	mov r11, r10
	jmp .L253
.L253:
	mov r10, rbp
	sub r10, 590
	mov [r10], r11d
	mov r10, rbp
	sub r10, 578
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 590
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L255
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str210
	lea rbx, .L.str211
	mov r12, rbp
	sub r12, 590
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
	jmp .L254
.L255:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str212
	mov rbx, 91
	lea r12, .L.str213
	mov r13, rbp
	sub r13, 578
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 590
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
.L254:
	mov r10, 0
	cmp r10, 0
	jne .L252
.L257:
	mov r10, 98
	mov r11, rbp
	sub r11, 594
	mov [r11], r10d
	lea r10, .L.str214
	mov r11, rbp
	sub r11, 602
	mov [r11], r10
	mov r10, rbp
	sub r10, 602
	mov r10, [r10]
	mov r11, 1
	mov rbx, 1
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10b, [r10]
	movzb r10, r10b
	mov r11, r10
	jmp .L258
.L258:
	mov r10, rbp
	sub r10, 606
	mov [r10], r11d
	mov r10, rbp
	sub r10, 594
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 606
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L260
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str215
	lea rbx, .L.str216
	mov r12, rbp
	sub r12, 606
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
	jmp .L259
.L260:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str217
	mov rbx, 92
	lea r12, .L.str218
	mov r13, rbp
	sub r13, 594
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 606
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
.L259:
	mov r10, 0
	cmp r10, 0
	jne .L257
.L262:
	mov r10, 99
	mov r11, rbp
	sub r11, 610
	mov [r11], r10d
	lea r10, .L.str219
	mov r11, rbp
	sub r11, 618
	mov [r11], r10
	mov r10, rbp
	sub r10, 618
	mov r10, [r10]
	mov r11, 2
	mov rbx, 1
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10b, [r10]
	movzb r10, r10b
	mov r11, r10
	jmp .L263
.L263:
	mov r10, rbp
	sub r10, 622
	mov [r10], r11d
	mov r10, rbp
	sub r10, 610
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 622
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L265
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str220
	lea rbx, .L.str221
	mov r12, rbp
	sub r12, 622
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
	jmp .L264
.L265:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str222
	mov rbx, 93
	lea r12, .L.str223
	mov r13, rbp
	sub r13, 610
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 622
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
.L264:
	mov r10, 0
	cmp r10, 0
	jne .L262
.L267:
	mov r10, 0
	mov r11, rbp
	sub r11, 626
	mov [r11], r10d
	lea r10, .L.str224
	mov r11, rbp
	sub r11, 634
	mov [r11], r10
	mov r10, rbp
	sub r10, 634
	mov r10, [r10]
	mov r11, 3
	mov rbx, 1
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10b, [r10]
	movzb r10, r10b
	mov r11, r10
	jmp .L268
.L268:
	mov r10, rbp
	sub r10, 638
	mov [r10], r11d
	mov r10, rbp
	sub r10, 626
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 638
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L270
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str225
	lea rbx, .L.str226
	mov r12, rbp
	sub r12, 638
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
	jmp .L269
.L270:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str227
	mov rbx, 94
	lea r12, .L.str228
	mov r13, rbp
	sub r13, 626
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 638
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
.L269:
	mov r10, 0
	cmp r10, 0
	jne .L267
.L272:
	mov r10, 1
	mov r11, rbp
	sub r11, 642
	mov [r11], r10d
	mov r10, 1
	mov r11, rbp
	sub r11, 646
	mov [r11], r10d
	mov r10, 2
	mov r11, rbp
	sub r11, 650
	mov [r11], r10d
	mov r10, rbp
	sub r10, 646
	mov r10d, [r10]
	mov r11, r10
	jmp .L273
.L273:
	mov r10, rbp
	sub r10, 654
	mov [r10], r11d
	mov r10, rbp
	sub r10, 642
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 654
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L275
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str229
	lea rbx, .L.str230
	mov r12, rbp
	sub r12, 654
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
	jmp .L274
.L275:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str231
	mov rbx, 96
	lea r12, .L.str232
	mov r13, rbp
	sub r13, 642
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 654
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
.L274:
	mov r10, 0
	cmp r10, 0
	jne .L272
.L277:
	mov r10, 0
	mov r11, rbp
	sub r11, 658
	mov [r11], r10d
	lea r10, var1
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 662
	mov [r11], r10d
	mov r10, rbp
	sub r10, 658
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 662
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L279
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str233
	lea rbx, .L.str234
	mov r12, rbp
	sub r12, 662
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
	jmp .L278
.L279:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str235
	mov rbx, 98
	lea r12, .L.str236
	mov r13, rbp
	sub r13, 658
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 662
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
.L278:
	mov r10, 0
	cmp r10, 0
	jne .L277
.L281:
	mov r10, 5
	mov r11, rbp
	sub r11, 666
	mov [r11], r10d
	lea r10, var1
	mov r11, 5
	mov [r10], r11d
	lea r10, var1
	mov r10d, [r10]
	mov r11, r10
	jmp .L282
.L282:
	mov r10, rbp
	sub r10, 670
	mov [r10], r11d
	mov r10, rbp
	sub r10, 666
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 670
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L284
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str237
	lea rbx, .L.str238
	mov r12, rbp
	sub r12, 670
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
	jmp .L283
.L284:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str239
	mov rbx, 99
	lea r12, .L.str240
	mov r13, rbp
	sub r13, 666
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 670
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
.L283:
	mov r10, 0
	cmp r10, 0
	jne .L281
.L286:
	mov r10, 20
	mov r11, rbp
	sub r11, 674
	mov [r11], r10d
	mov r10, 20
	mov r11, rbp
	sub r11, 678
	mov [r11], r10d
	mov r10, rbp
	sub r10, 674
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 678
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L288
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str241
	lea rbx, .L.str242
	mov r12, rbp
	sub r12, 678
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
	jmp .L287
.L288:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str243
	mov rbx, 100
	lea r12, .L.str244
	mov r13, rbp
	sub r13, 674
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 678
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
.L287:
	mov r10, 0
	cmp r10, 0
	jne .L286
.L290:
	mov r10, 15
	mov r11, rbp
	sub r11, 682
	mov [r11], r10d
	lea r10, var2
	mov r11, 0
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 5
	mov [r10], r11d
	lea r10, var2
	mov r11, 4
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r11, 10
	mov [r10], r11d
	lea r10, var2
	mov r11, 0
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10d, [r10]
	lea r11, var2
	mov rbx, 4
	mov r12, 4
	mov rax, r12
	mul rbx
	mov rbx, rax
	add r11, rbx
	mov r11d, [r11]
	add r10, r11
	mov r11, r10
	jmp .L291
.L291:
	mov r10, rbp
	sub r10, 686
	mov [r10], r11d
	mov r10, rbp
	sub r10, 682
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 686
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L293
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str245
	lea rbx, .L.str246
	mov r12, rbp
	sub r12, 686
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
	jmp .L292
.L293:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str247
	mov rbx, 101
	lea r12, .L.str248
	mov r13, rbp
	sub r13, 682
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 686
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
.L292:
	mov r10, 0
	cmp r10, 0
	jne .L290
.L295:
	mov r10, 5
	mov r11, rbp
	sub r11, 690
	mov [r11], r10d
	lea r10, global_arr
	mov r11, 0
	mov rbx, 4
	mov rax, rbx
	mul r11
	mov r11, rax
	add r10, r11
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 694
	mov [r11], r10d
	mov r10, rbp
	sub r10, 690
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 694
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L297
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str249
	lea rbx, .L.str250
	mov r12, rbp
	sub r12, 694
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
	jmp .L296
.L297:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str251
	mov rbx, 102
	lea r12, .L.str252
	mov r13, rbp
	sub r13, 690
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 694
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
.L296:
	mov r10, 0
	cmp r10, 0
	jne .L295
.L299:
	mov r10, 8
	mov r11, rbp
	sub r11, 698
	mov [r11], r10d
	mov r10, 3
	mov r11, 5
	mov rbx, r11
	jmp .L301
.L301:
	add r10, rbx
	mov r11, r10
	jmp .L300
.L300:
	mov r10, rbp
	sub r10, 702
	mov [r10], r11d
	mov r10, rbp
	sub r10, 698
	mov r10d, [r10]
	mov r11, rbp
	sub r11, 702
	mov r11d, [r11]
	cmp r10, r11
	sete r10b
	movzb r10, r10b
	cmp r10, 0
	je .L303
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str253
	lea rbx, .L.str254
	mov r12, rbp
	sub r12, 702
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
	jmp .L302
.L303:
	lea r10, stderr
	mov r10, [r10]
	lea r11, .L.str255
	mov rbx, 104
	lea r12, .L.str256
	mov r13, rbp
	sub r13, 698
	mov r13d, [r13]
	mov r14, rbp
	sub r14, 702
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
.L302:
	mov r10, 0
	cmp r10, 0
	jne .L299
	lea r10, .L.str257
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
