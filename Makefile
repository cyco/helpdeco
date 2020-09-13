OUT=out
SOURCES=src/helpdeco.c src/helpdec1.c src/compat.c

helpdeco.wasm:
	@ mkdir -p $(OUT)
	emcc -s WASM=1 -o $(OUT)/helpdeco.js src/helpdeco_wasm.c $(SOURCES) \
		-s EXPORTED_FUNCTIONS="['_get_version', '_render']" \
		-s EXTRA_EXPORTED_RUNTIME_METHODS='["cwrap", "ccall", "FS"]' \
		-s FORCE_FILESYSTEM=1 \
		-s FILESYSTEM=1 \
		-s EXIT_RUNTIME=1 \
		-s ERROR_ON_UNDEFINED_SYMBOLS=0 \
		-s ALLOW_MEMORY_GROWTH=1 \
		-D DEBUG=1
