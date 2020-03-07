#!/bin/bash
try() {
	expected="$1"
	input="$2"

	./target/debug/mir9cc "$input" > compile1.s
	gcc -static -o compile compile1.s
	./compile
	actual="$?"

	if [ "$actual" = "$expected" ]; then
		echo "$input => $actual"
	else
		echo -e "\e[31mFAILED\e[m"
		echo "$input => $expected expected, but got $actual"
		exit 1
	fi
}

compile_err() {
	input="$1"

	echo "$input"
	./target/debug/mir9cc "$input" > compile2.s
	if [ $? -gt 0 ]; then
		:
	else
		echo -e "\e[31mFAILED\e[m"
		echo "$input"
		echo "must be abort"
		exit 1
	fi
	
}

echo -e "\n\e[32m*** try test start ***\e[m\n"

try 10 'return 2*3+4;'
try 14 'return 2+3*4;'
try 26 'return 2*3+4*5;'
try 5 'return 50/10;'
try 9 'return 6*3/2;'

try 0 'return 0;'
try 42 'return 42;'
try 21 '1+2; return 5+20-4;'
try 41 'return  12 + 34 - 5 ;'
try 45 'return (2+3)*(4+5);'
try 153 'return 1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17;'

try 2 'a=2; return a;'
try 10 'a=2; b=3+2; return a*b;'

try 2 'if (1) return 2; return 3;'
try 3 'if (0) return 2; return 3;'


echo -e "\n\e[32m*** SUCCESS! ***\e[m\n"

# echo -e "\e[32m=== compile_err test start ===\e[m\n"

# echo -e "\n\e[32m=== compile_err test SUCCESS ===\e[m"
