mir9cc:
	cargo build

run:
	cargo run ${ARG}

test: mir9cc test/test.c test/token.c
	make mir9cc
	@# ./test.sh
	@./target/debug/mir9cc test/test.c > test1.s
	@gcc -c -o tmp-test.o test/gcc.c
	@gcc -static -o test1 test1.s tmp-test.o
	@echo -e "\n\e[33mtest.c TEST start...\e[m\n"
	@./test1
	@echo -e "\n\e[32m*** SUCCESS! ***\e[m\n"

	@./target/debug/mir9cc test/token.c > test2.s
	@gcc -static -o test2 test2.s
	@echo -e "\n\e[33mtoken.c TEST start...\e[m\n"
	@./test2
	@echo -e "\n\e[32m*** SUCCESS! ***\e[m\n"

debug: mir9cc test/singletest.c
	@./target/debug/mir9cc test/singletest.c > debug.s
	@cat debug.s

clean:
	@-rm *.s
	@-rm test1 test2 tmp-test.o
	@-cargo clean

.PHONY: test clean