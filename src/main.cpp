#include "../binaryen/src/binaryen-c.h"
#include "builtins/functions.cpp"
#include <stdio.h>

int main() {
    BinaryenModuleRef module = BinaryenModuleCreate();
    BinaryenSetMemory(module, 0, 256, "main_memory", {}, {}, {}, {}, {}, 0,
                      false, false, "main_memory");
    BinaryenModuleSetFeatures(module, BinaryenFeatureAll());
    BinaryenAddFunction(module, "foo", BinaryenTypeInt32(), BinaryenTypeInt32(),
                        NULL, 0,
                        BinaryenConst(module, BinaryenLiteralInt32(42)));
    BinaryenAddFunctionExport(module, "foo", "foo");
    (void)strcmp(module);
    BinaryenAddFunctionExport(module, "strcmp", "strcmp");
    BinaryenModuleAutoDrop(module);
    if (BinaryenModuleValidate(module)) {
        printf("Validation succeeded\n");
    } else {
        printf("Validation failed\n");
        return 1;
    }
    BinaryenSetOptimizeLevel(3);
    BinaryenSetShrinkLevel(2);
    BinaryenModuleOptimize(module);
    BinaryenModulePrint(module);
    BinaryenModuleDispose(module);

    return 0;
}
