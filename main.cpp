#include "binaryen/src/binaryen-c.h"

int main() {
    BinaryenModuleRef module = BinaryenModuleCreate();
    BinaryenAddFunction(
        module,
        "foo",
        BinaryenTypeInt32(),
        BinaryenTypeInt32(),
        NULL,
        0,
        BinaryenConst(module, BinaryenLiteralInt32(42))
    );
    BinaryenAddFunctionExport(module, "foo", "foo");
    BinaryenModulePrint(module);
    BinaryenModuleDispose(module);

    return 0;
}
