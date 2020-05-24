// int add2(int (*a)[2]) { return a[0][0] + a[0][1]; }
int add(int a, int b, int c, int d, int e, int f) { return a+b+c+d+e+f; }
int main() {
	// int ary[2][2]; ary[0][0]=3; ary[0][1]=5; return add2(ary);
	// int ary[2][2]; ary[0][0]=3; ary[1][0]=5; return ary[0][0]+ary[1][0];
	// char *s = "abc";
	// return s[2];
	int c = add(43, 3, 43, 3, 3, 3423);
}