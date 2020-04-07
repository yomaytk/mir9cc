mir9cc:
	cargo build

run:
	cargo run ${ARG}

test: mir9cc test/test.c
	make mir9cc
	# ./test.sh
	@./target/debug/mir9cc "$$(gcc -E -P test/test.c)" > compile1.s
	@echo 'int global_arr[1] = {5};' | gcc -xc -c -o tmp-test2.o -
	@gcc -static -o compile compile1.s tmp-test2.o
	@echo "\n\e[32m*** try test start ***\e[m\n"
	@./compile
	@echo "\n\e[32m*** SUCCESS! ***\e[m\n"

clean:
	cargo clean

.PHONY: test clean