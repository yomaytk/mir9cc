#!/bin/bash
try() {
	expected="$1"
	input="$2"

	./target/debug/Mir9cc "$input" > compile1.s
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
	./target/debug/Mir9cc "$input" > compile2.s
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

try 0 0
try 42 42
try 27 23+4
try 20 12-5+5+10-2

echo -e "\n\e[32m*** SUCCESS! ***\e[m\n"

# echo -e "\e[32m=== compile_err test start ===\e[m\n"

# echo -e "\n\e[32m=== compile_err test SUCCESS ===\e[m"
