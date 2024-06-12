binaryen: ./binaryen/src/binaryen-c.h
	cd binaryen && emcmake cmake -DBUILD_STATIC_LIB=ON -DBUILD_TOOLS=OFF -DBUILD_TESTS=OFF . && emmake make binaryen

debug: ./src/main.cpp ./binaryen/lib/libbinaryen.a
	emcc ./src/main.cpp ./binaryen/lib/libbinaryen.a -I./binaryen/src -std=c++2a -sEXPORTED_FUNCTIONS=_main -g -o ./lib/main.js

release: ./src/main.cpp ./binaryen/lib/libbinaryen.a
	emcc ./src/main.cpp ./binaryen/lib/libbinaryen.a -I./binaryen/src -std=c++2a -sEXPORTED_FUNCTIONS=_main -Oz -o ./lib/main.js
