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
		'ERRFILE="{}.errors"; \
		if [ -f "$$ERRFILE" ]; then \
			STDERR=$$(timeout 15 ./milang run "{}" >/dev/null 2>&1; echo "$$?" > "$$0/rc_$$(basename {})"); \
			STDERR=$$(timeout 15 ./milang run "{}" 2>&1 >/dev/null); \
			PASS=1; \
			while IFS= read -r expected; do \
				case "$$expected" in \#*|"") continue ;; esac; \
				if ! echo "$$STDERR" | grep -qF "$$expected"; then \
					PASS=0; break; \
				fi; \
			done < "$$ERRFILE"; \
			if [ $$PASS -eq 1 ]; then \
				touch "$$0/pass_$$(basename {})"; \
			else \
				touch "$$0/fail_$$(basename {})"; \
				echo "FAIL: {} (missing expected error)"; \
			fi; \
		elif timeout 15 ./milang run "{}" >/dev/null 2>&1; then \
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
