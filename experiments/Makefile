EXT = .wasm

WASM_AS ?= wasm-as --enable-mutable-globals --enable-bulk-memory --enable-reference-types --enable-extended-const --enable-gc --enable-strings

%.wasm: %.wat
	$(WASM_AS) --debuginfo --source-map $@.map $< -o $@

.PHONY: clean

clean:
	rm -f *.wasm */*.wasm *.map */*.map
