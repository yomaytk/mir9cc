mir9cc:
	cargo build

run:
	cargo run ${ARG}

test: mir9cc test/test.c test/token.c
	make mir9cc
	@# ./test.sh
	@gcc -E -P test/test.c > tmp-test.tmp
	@./target/debug/mir9cc tmp-test.tmp > compile1.s
	@./target/debug/mir9cc test/token.c > compile2.s
	@gcc -c -o tmp-test2.o test/gcc.c
	@gcc -static -o compile compile1.s compile2.s tmp-test2.o
	@echo -e "\n\e[32m*** try test start ***\e[m\n"
	@./compile
	@echo -e "\n\e[32m*** SUCCESS! ***\e[m\n"

debug: mir9cc test/singletest.c test/sub-test.c
	@gcc -c -o sub-test.o test/sub-test.c
	@./target/debug/mir9cc test/singletest.c > debugcompile1.s
	@gcc -static -o debugcompile debugcompile1.s sub-test.o
	@#./debugcompile
	@#echo
	@#echo -e "\n$?"
	@cat debugcompile1.s

clean:
	cargo clean

.PHONY: test clean