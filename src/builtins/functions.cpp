#include "types.cpp"

BinaryenFunctionRef strcmp(BinaryenModuleRef mod) {
    auto u8_ = u8(mod);
    auto u32_ = u32(mod);
    auto v128_ = v128(mod);
    auto i8x16_ = i8x16(mod);

    auto STRING_1 = Local<0>(mod, &u32_);
    auto STRING_2 = Local<1>(mod, &u32_);
    auto LENGTH = Local<2>(mod, &u32_);
    auto CTZ = Local<3>(mod, &u32_);
    auto IF_ALL_ELSE = Local<4>(mod, &u32_);

    // in my defense, this is basically a DSL

#define LOAD(ptr, offset, type)                                                \
    WasmExpression(mod, &type,                                                 \
                   BinaryenLoad(mod, type.size, false, offset, 0, type.t##ype, \
                                ptr, "main_memory"))
#define i32const(value)                                                        \
    WasmExpression(mod, &u32_, BinaryenConst(mod, BinaryenLiteralInt32(16)))
#define i8x16const(value)                                                      \
    WasmExpression(mod, &i8x16_,                                               \
                   BinaryenConst(mod, BinaryenLiteralVec128(value)))
#define i8x16splat(value)                                                      \
    WasmExpression(mod, &i8x16_,                                               \
                   BinaryenUnary(mod, BinaryenSplatVecI8x16(), value))
#define i32ctz(value)                                                          \
    WasmExpression(mod, &u32_, BinaryenUnary(mod, BinaryenCtzInt32(), value))
#define i8x16bitmask(value)                                                    \
    WasmExpression(mod, &i8x16_,                                               \
                   BinaryenUnary(mod, BinaryenBitmaskVecI8x16(), value))
#define SELECT(cond, true_, false_)                                            \
    WasmExpression(                                                            \
        mod, &u32_,                                                            \
        BinaryenSelect(mod, cond, true_, false_, BinaryenTypeAuto()))
#define IF(cond, true_)                                                        \
    WasmExpression(mod, &u32_, BinaryenIf(mod, cond, true_, nullptr))
#define NUMARGS(...)                                                           \
    (sizeof((BinaryenExpressionRef[]){__VA_ARGS__}) /                          \
     sizeof(BinaryenExpressionRef))
#define BLOCK(...)                                                             \
    BinaryenBlock(mod, NULL, (BinaryenExpressionRef[]){__VA_ARGS__},           \
                  NUMARGS(__VA_ARGS__), BinaryenTypeAuto())
#define LOOP(name, body) BinaryenLoop(mod, name, body)
#define RETURN(value) BinaryenReturn(mod, value)
#define BREAK(name, condition, value) BinaryenBreak(mod, name, condition, value)

    // clang-format off
    BinaryenExpressionRef block[] = {
        IF_ALL_ELSE = *STRING_1 - *STRING_2,
        LENGTH = SELECT(*STRING_1 < *STRING_2, *STRING_1, *STRING_2),

        IF(
            LENGTH >= i32const(16),
            LOOP(
                "cmp_loop",
                BLOCK(
                    IF(
                        CTZ = i8x16bitmask(LOAD(STRING_1, 4, i8x16_) != LOAD(STRING_2, 4, i8x16_)),
                        RETURN(LOAD(STRING_1 + (CTZ = i32ctz(CTZ)), 4, u8_) - LOAD(STRING_1 + CTZ, 4, u8_))),

                    STRING_1 = STRING_1 + i32const(16),
                    STRING_2 = STRING_2 + i32const(16),

                    BREAK(
                        "cmp_loop",
                        (LENGTH = LENGTH - i32const(16)) >= i32const(16),
                        NULL)))),

        CTZ = i8x16bitmask(
            (
                LOAD(STRING_1, 4, i8x16_) !=
                LOAD(STRING_2, 4, i8x16_)
            ) & (
                i8x16const(
                    ((const unsigned char[]){0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15})
                ) < i8x16splat(LENGTH))),

        SELECT(
            CTZ, 
            LOAD(STRING_1 + (LENGTH = i32ctz(CTZ)), 4, u8_) 
                  - LOAD(STRING_2 + LENGTH, 4, u8_), 
            IF_ALL_ELSE)
    };

#undef LOAD
#undef i32const
#undef i8x16const
#undef i8x16splat
#undef i32ctz
#undef i8x16bitmask
#undef SELECT
#undef BLOCK
#undef IF
#undef NUMARGS
#undef LOOP
#undef RETURN
#undef BREAK

    return BinaryenAddFunction(
        mod, "strcmp",
        BinaryenTypeCreate(
            (BinaryenType[]){BinaryenTypeInt32(), BinaryenTypeInt32()}, 2),
        BinaryenTypeInt32(),
        (BinaryenType[]){BinaryenTypeInt32(), BinaryenTypeInt32(),
                         BinaryenTypeInt32()},
        3,
        BinaryenBlock(mod, NULL, block, sizeof(block) / sizeof(block[0]),
                      BinaryenTypeAuto()));
}
