# Dockerfile.builder - build stage that depends on milang-base
FROM milang-base AS builder

# 5. Copy project sources
COPY . .

# 6. Build the final executable (force static linking at linker level)
RUN cabal update && cabal build --disable-shared --enable-executable-static --enable-static \
    --ghc-options="-static -optl-static -optl-pthread"

# 7. Extract the binary to a predictable location for extraction
RUN mkdir -p /build && cp $(cabal list-bin exe:milang-core) /build/milang-bin

# Final minimal image
FROM scratch AS release
COPY --from=builder /build/milang-bin /milang
ENTRYPOINT ["/milang"]
