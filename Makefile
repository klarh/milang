.PHONY: all clean test

all:
	cd hs && cabal build
	ln -sf $$(cd hs && cabal list-bin milang) milang

clean:
	cd hs && cabal clean
	rm -f milang

test: all
	@echo "=== Running tests ==="
	@TMPDIR=$$(mktemp -d); \
	ls tests/*.mi | xargs -P $$(nproc) -I{} sh -c \
		'if timeout 15 ./milang run "{}" >/dev/null 2>&1; then \
			touch "$$0/pass_$$(basename {})"; \
		else \
			touch "$$0/fail_$$(basename {})"; \
			echo "FAIL: {}"; \
		fi' "$$TMPDIR"; \
	PASS=$$(ls "$$TMPDIR"/pass_* 2>/dev/null | wc -l); \
	FAIL=$$(ls "$$TMPDIR"/fail_* 2>/dev/null | wc -l); \
	rm -rf "$$TMPDIR"; \
	echo "Passed: $$PASS, Failed: $$FAIL"; \
	[ $$FAIL -eq 0 ]
