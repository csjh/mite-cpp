#include "../binaryen/src/binaryen-c.h"
#include "builtins/functions.cpp"
#include <stdio.h>

int main() {
    auto foomod =
        BinaryenModuleParse("(module"
                            "(type $0 (func (param i32) (result i32)))"
                            "(export \"foo\" (func $foo))"
                            "(func $foo (param $0 i32) (result i32)"
                            "(i32.const 43)"
                            ")"
                            ")");
    auto foo = BinaryenGetFunction(foomod, "foo");

    auto module = BinaryenModuleCreate();
    BinaryenSetMemory(module, 0, 256, "main_memory", {}, {}, {}, {}, {}, 0,
                      false, false, "main_memory");
    BinaryenModuleSetFeatures(module, BinaryenFeatureAll());
    // BinaryenAddFunction(module, "foo", BinaryenTypeInt32(),
    // BinaryenTypeInt32(),
    //                     NULL, 0,
    //                     BinaryenConst(module, BinaryenLiteralInt32(42)));

    auto num_locals = BinaryenFunctionGetNumVars(foo);
    auto *local_types = new BinaryenType[num_locals];
    for (size_t i = 0; i < num_locals; i++) {
        local_types[i] = BinaryenFunctionGetVar(foo, i);
    }
    BinaryenAddFunction(
        module,
        BinaryenFunctionGetName(foo),
        BinaryenFunctionGetParams(foo),
        BinaryenFunctionGetResults(foo),
        local_types,
        num_locals,
        BinaryenFunctionGetBody(foo)
    );

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
