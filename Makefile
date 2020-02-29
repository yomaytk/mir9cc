mir9cc:
	cargo build

run:
	cargo run ${ARG}

test: mir9cc
	make mir9cc
	./test.sh

clean:
	cargo clean