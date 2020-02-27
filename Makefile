Mir9cc:
	cargo build

test: Mir9cc
	./test.sh

clean:
	cargo clean