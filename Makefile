binaryen-debug: ./binaryen/src/binaryen-c.h
	cd binaryen && emcmake cmake -DBUILD_STATIC_LIB=ON -DBUILD_TOOLS=OFF -DBUILD_TESTS=OFF -DCMAKE_BUILD_TYPE=Debug . && emmake make binaryen

binaryen-debug: ./binaryen/src/binaryen-c.h
	cd binaryen && emcmake cmake -DBUILD_STATIC_LIB=ON -DBUILD_TOOLS=OFF -DBUILD_TESTS=OFF -DCMAKE_BUILD_TYPE=Release . && emmake make binaryen

debug: ./src/main.cpp ./binaryen/lib/libbinaryen.a
	emcc ./src/main.cpp ./binaryen/lib/libbinaryen.a -I./binaryen/src -std=c++2a -O1 -Wall -sEXPORTED_FUNCTIONS=_main -sERROR_ON_WASM_CHANGES_AFTER_LINK -sWASM_BIGINT -o ./lib/main.js

release: ./src/main.cpp ./binaryen/lib/libbinaryen.a
	emcc ./src/main.cpp ./binaryen/lib/libbinaryen.a -I./binaryen/src -std=c++2a -Oz -Wall -sEXPORTED_FUNCTIONS=_main -o ./lib/main.js
