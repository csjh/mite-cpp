#include "types.cpp"

BinaryenFunctionRef strcmp(BinaryenModuleRef mod) {
    auto u8_ = u8(mod);
    auto u32_ = u32(mod);
    auto u8x16_ = i8x16(mod);

    auto s1 = Local<0>(mod, &u32_);
    auto s2 = Local<1>(mod, &u32_);
    auto len = Local<2>(mod, &u32_);
    auto ctz = Local<3>(mod, &u32_);
    auto tiebreaker = Local<4>(mod, &u32_);

    // in my defense, this is basically a DSL

#define LOAD(type, ptr, offset)                                                \
    WasmExpression(mod, &type,                                                 \
                   BinaryenLoad(mod, type.size, false, offset, 0, type.t##ype, \
                                ptr, "main_memory"))
#define i32const(value)                                                        \
    WasmExpression(mod, &u32_, BinaryenConst(mod, BinaryenLiteralInt32(16)))
#define i8x16const(value)                                                      \
    WasmExpression(mod, &u8x16_,                                               \
                   BinaryenConst(mod, BinaryenLiteralVec128(value)))
#define i8x16splat(value)                                                      \
    WasmExpression(mod, &u8x16_,                                               \
                   BinaryenUnary(mod, BinaryenSplatVecI8x16(), value))
#define i32ctz(value)                                                          \
    WasmExpression(mod, &u32_, BinaryenUnary(mod, BinaryenCtzInt32(), value))
#define i8x16bitmask(value)                                                    \
    WasmExpression(mod, &u8x16_,                                               \
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
#define BREAK_IF(name, condition) BinaryenBreak(mod, name, condition, NULL)

    const char *loopname = "cmp_loop";
    uint8_t mask[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};

    // clang-format off
    BinaryenExpressionRef block[] = {
        tiebreaker = *s1 - *s2,
        len = SELECT(*s1 < *s2, *s1, *s2),

        IF(
            len >= i32const(u8x16_.size),
            LOOP(
                loopname,
                BLOCK(
                    IF(
                        ctz = i8x16bitmask(LOAD(u8x16_, s1, u32_.size) != LOAD(u8x16_, s2, u32_.size)),
                        RETURN(LOAD(u8_, s1 + (ctz = i32ctz(ctz)), u32_.size) - LOAD(u8_, s1 + ctz, u32_.size))),

                    s1 += i32const(u8x16_.size),
                    s2 += i32const(u8x16_.size),

                    BREAK_IF(loopname, (len -= i32const(u8x16_.size)) >= i32const(u8x16_.size))))),

        ctz = i8x16bitmask(
            LOAD(u8x16_, s1, u32_.size) != LOAD(u8x16_, s2, u32_.size)
                   & (i8x16const(mask) < i8x16splat(len))),

        SELECT(
            ctz, 
            LOAD(u8_, s1 + (len = i32ctz(ctz)), u32_.size) - LOAD(u8_, s2 + len, u32_.size), 
            tiebreaker)
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
