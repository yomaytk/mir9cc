Mir9cc:
	cargo build

run:
	cargo run ${ARG}

test: Mir9cc
	./test.sh

clean:
	cargo clean