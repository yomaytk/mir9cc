int main() {
	// struct { int a; } x; x.a=3; return x.a;
	struct tag { char a; int b; } x; struct tag *p = &x; x.a=3; x.b=5; return p->a+p->b;
}