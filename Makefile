mir9cc:
	cargo build

run:
	cargo run ${ARG}

test: mir9cc test/test.c
	make mir9cc
	# ./test.sh
	@gcc -E -P test/test.c > tmp-test.tmp
	@./target/debug/mir9cc tmp-test.tmp > compile1.s
	@./target/debug/mir9cc test/token.c > compile2.s
	@gcc -c -o tmp-test2.o test/gcc.c
	@gcc -static -o compile compile1.s compile2.s tmp-test2.o
	@echo -e "\n\e[32m*** try test start ***\e[m\n"
	@./compile
	@echo -e "\n\e[32m*** SUCCESS! ***\e[m\n"

clean:
	cargo clean

.PHONY: test clean