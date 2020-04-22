int add4(int a[5][7]) { return a[0][0] + a[1][0]; }

int main(){
	int aab[5][7]; aab[0][0] = 3; aab[1][0] = 5; return add4(aab); 
}