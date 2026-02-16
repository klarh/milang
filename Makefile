.PHONY: all clean test

all:
	cd hs && cabal build
	ln -sf $$(cd hs && cabal list-bin milang) milang

clean:
	cd hs && cabal clean
	rm -f milang

test: all
	@echo "=== Running tests ==="
	@PASS=0; FAIL=0; \
	for f in tests/*.mi; do \
		if timeout 15 ./milang run "$$f" >/dev/null 2>&1; then \
			PASS=$$((PASS+1)); \
		else \
			echo "FAIL: $$f"; \
			FAIL=$$((FAIL+1)); \
		fi; \
	done; \
	echo "Passed: $$PASS, Failed: $$FAIL"; \
	[ $$FAIL -eq 0 ]
