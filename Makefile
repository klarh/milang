.PHONY: all clean test test-hs docs docs-serve docs-publish

MILANG:=milang
PYTHON:=python

DYNAMIC:=0
DYNAMIC_0:=
DYNAMIC_1:=-dynamic
GHC_OPTIONS:=-Wall -O2 ${DYNAMIC_${DYNAMIC}}

all:
	@echo 'module Core.Version (version) where' > core/src/Core/Version.hs
	@echo '' >> core/src/Core/Version.hs
	@echo 'version :: String' >> core/src/Core/Version.hs
	@echo 'version = "'$$(date +%Y%m%d%H%M%S)-$$(git rev-parse --short HEAD)'"' >> core/src/Core/Version.hs
	cd core && cabal build --ghc-options="${GHC_OPTIONS}"
	ln -sf "$$(cd core && cabal list-bin -v0 exe:milang-core | tr -d '\r' | tr '\\' '/')" "$(MILANG)"

clean:
	cd core && cabal clean
	cd hs && cabal clean
	rm -f $(MILANG) milang

test:
	$(PYTHON) tests/run.py -m ${MILANG}

test-hs:
	cd hs && cabal build
	ln -sf $$(cd hs && cabal list-bin milang) milang
	$(MAKE) test

docs:
	cd docs && mdbook build

docs-serve:
	cd docs && mdbook serve --open

docs-publish: docs
	@TMP=$$(mktemp -d) && \
	cp -r docs-out/. $$TMP/ && \
	cd $$TMP && git init -b gh-pages && git add -A && \
	git commit -m "Deploy docs $$(date +%Y-%m-%d)" && \
	git push -f git@github.com:klarh/milang.git HEAD:gh-pages && \
	rm -rf $$TMP

# Build WebAssembly using ghc-wasm toolchain (requires wasm32-wasi-cabal on PATH)
.PHONY: wasm
wasm:
@echo "Building wasm executable (requires ghc-wasm toolchain)"
cd core && wasm32-wasi-cabal build exe:milang-wasm -j1
@mkdir -p docs/assets/wasm
@find core -type f -name "milang-wasm.wasm" -print -quit | xargs -I{} cp {} docs/assets/wasm/milang-wasm.wasm || true
