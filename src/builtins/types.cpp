#include "../../binaryen/src/binaryen-c.h"
#include <stdexcept>

class WasmExpression;

class WasmType {
  public:
    BinaryenModuleRef module;
    BinaryenType type;
    uint32_t size;

    virtual WasmExpression add(const WasmExpression &lhs,
                               const WasmExpression &rhs) = 0;
    virtual WasmExpression sub(const WasmExpression &lhs,
                               const WasmExpression &rhs) = 0;
    virtual WasmExpression mul(const WasmExpression &lhs,
                               const WasmExpression &rhs) = 0;
    virtual WasmExpression div(const WasmExpression &lhs,
                               const WasmExpression &rhs) = 0;
    virtual WasmExpression mod(const WasmExpression &lhs,
                               const WasmExpression &rhs) = 0;
    virtual WasmExpression and_(const WasmExpression &lhs,
                                const WasmExpression &rhs) = 0;
    virtual WasmExpression or_(const WasmExpression &lhs,
                               const WasmExpression &rhs) = 0;
    virtual WasmExpression xor_(const WasmExpression &lhs,
                                const WasmExpression &rhs) = 0;
    virtual WasmExpression shl(const WasmExpression &lhs,
                               const WasmExpression &rhs) = 0;
    virtual WasmExpression shr(const WasmExpression &lhs,
                               const WasmExpression &rhs) = 0;
    virtual WasmExpression lt(const WasmExpression &lhs,
                              const WasmExpression &rhs) = 0;
    virtual WasmExpression le(const WasmExpression &lhs,
                              const WasmExpression &rhs) = 0;
    virtual WasmExpression gt(const WasmExpression &lhs,
                              const WasmExpression &rhs) = 0;
    virtual WasmExpression ge(const WasmExpression &lhs,
                              const WasmExpression &rhs) = 0;
    virtual WasmExpression eq(const WasmExpression &lhs,
                              const WasmExpression &rhs) = 0;
    virtual WasmExpression ne(const WasmExpression &lhs,
                              const WasmExpression &rhs) = 0;
    virtual WasmExpression bitwise_not(const WasmExpression &expr) = 0;
    virtual WasmExpression logical_not(const WasmExpression &expr) = 0;
    virtual WasmExpression neg(const WasmExpression &expr) = 0;
    virtual WasmExpression deref(const WasmExpression &expr) = 0;
};

class WasmExpression {
  private:
    BinaryenExpressionRef expression;
    WasmType *type;

  public:
    BinaryenModuleRef module;

    WasmExpression(BinaryenModuleRef module, WasmType *type,
                   BinaryenExpressionRef expression)
        : module(module), type(type), expression(expression) {}

    WasmExpression operator+(const WasmExpression &rhs) {
        return type->add(*this, rhs);
    }
    WasmExpression operator-(const WasmExpression &rhs) {
        return type->sub(*this, rhs);
    }
    WasmExpression operator*(const WasmExpression &rhs) {
        return type->mul(*this, rhs);
    }
    WasmExpression operator/(const WasmExpression &rhs) {
        return type->div(*this, rhs);
    }
    WasmExpression operator%(const WasmExpression &rhs) {
        return type->mod(*this, rhs);
    }
    WasmExpression operator&(const WasmExpression &rhs) {
        return type->and_(*this, rhs);
    }
    WasmExpression operator|(const WasmExpression &rhs) {
        return type->or_(*this, rhs);
    }
    WasmExpression operator^(const WasmExpression &rhs) {
        return type->xor_(*this, rhs);
    }
    WasmExpression operator<<(const WasmExpression &rhs) {
        return type->shl(*this, rhs);
    }
    WasmExpression operator>>(const WasmExpression &rhs) {
        return type->shr(*this, rhs);
    }
    WasmExpression operator<(const WasmExpression &rhs) {
        return type->lt(*this, rhs);
    }
    WasmExpression operator<=(const WasmExpression &rhs) {
        return type->le(*this, rhs);
    }
    WasmExpression operator>(const WasmExpression &rhs) {
        return type->gt(*this, rhs);
    }
    WasmExpression operator>=(const WasmExpression &rhs) {
        return type->ge(*this, rhs);
    }
    WasmExpression operator==(const WasmExpression &rhs) {
        return type->eq(*this, rhs);
    }
    WasmExpression operator!=(const WasmExpression &rhs) {
        return type->ne(*this, rhs);
    }
    WasmExpression operator~() { return type->bitwise_not(*this); }
    WasmExpression operator!() { return type->logical_not(*this); }
    WasmExpression operator-() { return type->neg(*this); }
    WasmExpression operator*() { return type->deref(*this); }

    operator BinaryenExpressionRef() const {
        return BinaryenExpressionCopy(expression, module);
    }
};

#define UNARY_OP(name)                                                         \
    WasmExpression(module, this, BinaryenUnary(module, Binaryen##name(), expr));

#define BINARY_OP(name)                                                        \
    WasmExpression(module, this,                                               \
                   BinaryenBinary(module, Binaryen##name(), lhs, rhs));

class i32 : public WasmType {
  public:
    i32(BinaryenModuleRef mod) {
        type = BinaryenTypeInt32();
        module = mod;
        size = 4;
    }

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddInt32);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubInt32);
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(MulInt32);
    }
    WasmExpression div(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(DivSInt32);
    }
    WasmExpression mod(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(RemSInt32);
    }
    WasmExpression and_(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AndInt32);
    }
    WasmExpression or_(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(OrInt32);
    }
    WasmExpression xor_(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(XorInt32);
    }
    WasmExpression shl(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShlInt32);
    }
    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrSInt32);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtSInt32);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeSInt32);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtSInt32);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeSInt32);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqInt32);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeInt32);
    }
    WasmExpression bitwise_not(const WasmExpression &lhs) {
        WasmExpression rhs = WasmExpression(
            module, this, BinaryenConst(module, BinaryenLiteralInt32(-1)));
        return BINARY_OP(XorInt32);
    }
    WasmExpression logical_not(const WasmExpression &expr) {
        return UNARY_OP(EqZInt32);
    }
    WasmExpression neg(const WasmExpression &rhs) {
        WasmExpression lhs = WasmExpression(
            module, this, BinaryenConst(module, BinaryenLiteralInt32(0)));
        return BINARY_OP(SubInt32);
    }
    WasmExpression deref(const WasmExpression &expr) {
        return WasmExpression(
            module, this,
            BinaryenLoad(module, 4, true, 0, 0, type, expr, "main_memory"));
    }
};

class u32 : public i32 {
  public:
    u32(BinaryenModuleRef mod) : i32(mod) {}

    WasmExpression div(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(DivUInt32);
    }
    WasmExpression mod(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(RemUInt32);
    }
    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrUInt32);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtUInt32);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeUInt32);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtUInt32);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeUInt32);
    }
};

class i8 : public i32 {
  public:
    i8(BinaryenModuleRef mod) : i32(mod) { size = 1; }
};

class u8 : public u32 {
  public:
    u8(BinaryenModuleRef mod) : u32(mod) { size = 1; }
};

class i16 : public i32 {
  public:
    i16(BinaryenModuleRef mod) : i32(mod) { size = 2; }
};

class u16 : public u32 {
  public:
    u16(BinaryenModuleRef mod) : u32(mod) { size = 2; }
};

class i64 : public WasmType {
  public:
    i64(BinaryenModuleRef mod) {
        type = BinaryenTypeInt64();
        module = mod;
        size = 8;
    }

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddInt64);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubInt64);
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(MulInt64);
    }
    WasmExpression div(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(DivSInt64);
    }
    WasmExpression mod(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(RemSInt64);
    }
    WasmExpression and_(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AndInt64);
    }
    WasmExpression or_(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(OrInt64);
    }
    WasmExpression xor_(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(XorInt64);
    }
    WasmExpression shl(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShlInt64);
    }
    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrSInt64);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtSInt64);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeSInt64);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtSInt64);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeSInt64);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqInt64);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeInt64);
    }
    WasmExpression bitwise_not(const WasmExpression &lhs) {
        WasmExpression rhs = WasmExpression(
            module, this, BinaryenConst(module, BinaryenLiteralInt64(-1)));
        return BINARY_OP(XorInt64);
    }
    WasmExpression logical_not(const WasmExpression &expr) {
        return UNARY_OP(EqZInt64);
    }
    WasmExpression neg(const WasmExpression &rhs) {
        WasmExpression lhs = WasmExpression(
            module, this, BinaryenConst(module, BinaryenLiteralInt64(0)));
        return BINARY_OP(SubInt64);
    }
    WasmExpression deref(const WasmExpression &expr) {
        // cannot deref a 64-bit value
        throw std::runtime_error("cannot deref a 64-bit value");
    }
};

class u64 : public i64 {
  public:
    u64(BinaryenModuleRef mod) : i64(mod) {}

    WasmExpression div(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(DivUInt64);
    }
    WasmExpression mod(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(RemUInt64);
    }
    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrUInt64);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtUInt64);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeUInt64);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtUInt64);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeUInt64);
    }
};

class f32 : public WasmType {
  public:
    f32(BinaryenModuleRef mod) {
        type = BinaryenTypeFloat32();
        module = mod;
        size = 4;
    }

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddFloat32);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubFloat32);
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(MulFloat32);
    }
    WasmExpression div(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(DivFloat32);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtFloat32);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeFloat32);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtFloat32);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeFloat32);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqFloat32);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeFloat32);
    }
    WasmExpression neg(const WasmExpression &expr) {
        return UNARY_OP(NegFloat32);
    }
    WasmExpression deref(const WasmExpression &expr) {
        throw std::runtime_error("cannot deref a 32-bit float");
    }
};

class f64 : public WasmType {
  public:
    f64(BinaryenModuleRef mod) {
        type = BinaryenTypeFloat32();
        module = mod;
        size = 8;
    }

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddFloat64);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubFloat64);
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(MulFloat64);
    }
    WasmExpression div(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(DivFloat64);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtFloat64);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeFloat64);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtFloat64);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeFloat64);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqFloat64);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeFloat64);
    }
    WasmExpression neg(const WasmExpression &expr) {
        return UNARY_OP(NegFloat64);
    }
    WasmExpression deref(const WasmExpression &expr) {
        throw std::runtime_error("cannot deref a 64-bit float");
    }
};

class v128 : public WasmType {
  public:
    v128(BinaryenModuleRef mod) {
        type = BinaryenTypeVec128();
        module = mod;
        size = 16;
    }

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot add v128");
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot sub v128");
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot mul v128");
    }
    WasmExpression div(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot div v128");
    }
    WasmExpression mod(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot mod v128");
    }
    WasmExpression and_(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AndVec128);
    }
    WasmExpression or_(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(OrVec128);
    }
    WasmExpression xor_(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(XorVec128);
    }
    WasmExpression shl(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot shl v128");
    }
    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot shr v128");
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot lt v128");
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot le v128");
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot gt v128");
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot ge v128");
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot eq v128");
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        throw std::runtime_error("cannot ne v128");
    }
    WasmExpression bitwise_not(const WasmExpression &expr) {
        return UNARY_OP(NotVec128);
    }
    WasmExpression logical_not(const WasmExpression &expr) {
        throw std::runtime_error("cannot logical_not v128");
    }
    WasmExpression neg(const WasmExpression &expr) {
        throw std::runtime_error("cannot neg v128");
    }
    WasmExpression deref(const WasmExpression &expr) {
        throw std::runtime_error("cannot deref v128");
    }
};

class i8x16 : public v128 {
  public:
    i8x16(BinaryenModuleRef mod) : v128(mod) {}

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddVecI8x16);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubVecI8x16);
    }
    WasmExpression shl(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShlVecI8x16);
    }
    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrSVecI8x16);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtSVecI8x16);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeSVecI8x16);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtSVecI8x16);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeSVecI8x16);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqVecI8x16);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeVecI8x16);
    }
    WasmExpression neg(const WasmExpression &expr) {
        return UNARY_OP(NegVecI8x16);
    }
};

class u8x16 : public i8x16 {
  public:
    u8x16(BinaryenModuleRef mod) : i8x16(mod) {}

    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrUVecI8x16);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtUVecI8x16);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeUVecI8x16);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtUVecI8x16);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeUVecI8x16);
    }
};

class i16x8 : public v128 {
  public:
    i16x8(BinaryenModuleRef mod) : v128(mod) {}

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddVecI16x8);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubVecI16x8);
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(MulVecI16x8);
    }
    WasmExpression shl(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShlVecI16x8);
    }
    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrSVecI16x8);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtSVecI16x8);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeSVecI16x8);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtSVecI16x8);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeSVecI16x8);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqVecI16x8);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeVecI16x8);
    }
    WasmExpression neg(const WasmExpression &expr) {
        return UNARY_OP(NegVecI16x8);
    }
};

class u16x8 : public i16x8 {
  public:
    u16x8(BinaryenModuleRef mod) : i16x8(mod) {}

    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrUVecI16x8);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtUVecI16x8);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeUVecI16x8);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtUVecI16x8);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeUVecI16x8);
    }
};

class i32x4 : public v128 {
  public:
    i32x4(BinaryenModuleRef mod) : v128(mod) {}

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddVecI32x4);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubVecI32x4);
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(MulVecI32x4);
    }
    WasmExpression shl(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShlVecI32x4);
    }
    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrSVecI32x4);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtSVecI32x4);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeSVecI32x4);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtSVecI32x4);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeSVecI32x4);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqVecI32x4);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeVecI32x4);
    }
    WasmExpression neg(const WasmExpression &expr) {
        return UNARY_OP(NegVecI32x4);
    }
};

class u32x4 : public i32x4 {
  public:
    u32x4(BinaryenModuleRef mod) : i32x4(mod) {}

    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrUVecI32x4);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtUVecI32x4);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeUVecI32x4);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtUVecI32x4);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeUVecI32x4);
    }
};

class i64x2 : public v128 {
  public:
    i64x2(BinaryenModuleRef mod) : v128(mod) {}

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddVecI64x2);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubVecI64x2);
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(MulVecI64x2);
    }
    WasmExpression shl(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShlVecI64x2);
    }
    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrSVecI64x2);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtSVecI64x2);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeSVecI64x2);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtSVecI64x2);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeSVecI64x2);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqVecI64x2);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeVecI64x2);
    }
    WasmExpression neg(const WasmExpression &expr) {
        return UNARY_OP(NegVecI64x2);
    }
};

class u64x2 : public i64x2 {
  public:
    u64x2(BinaryenModuleRef mod) : i64x2(mod) {}

    WasmExpression shr(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(ShrUVecI64x2);
    }
};

class f32x4 : public v128 {
  public:
    f32x4(BinaryenModuleRef mod) : v128(mod) {}

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddVecF32x4);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubVecF32x4);
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(MulVecF32x4);
    }
    WasmExpression div(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(DivVecF32x4);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtVecF32x4);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeVecF32x4);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtVecF32x4);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeVecF32x4);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqVecF32x4);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeVecF32x4);
    }
    WasmExpression neg(const WasmExpression &expr) {
        return UNARY_OP(NegVecF32x4);
    }
};

class f64x2 : public v128 {
  public:
    f64x2(BinaryenModuleRef mod) : v128(mod) {}

    WasmExpression add(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(AddVecF64x2);
    }
    WasmExpression sub(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(SubVecF64x2);
    }
    WasmExpression mul(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(MulVecF64x2);
    }
    WasmExpression div(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(DivVecF64x2);
    }
    WasmExpression lt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LtVecF64x2);
    }
    WasmExpression le(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(LeVecF64x2);
    }
    WasmExpression gt(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GtVecF64x2);
    }
    WasmExpression ge(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(GeVecF64x2);
    }
    WasmExpression eq(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(EqVecF64x2);
    }
    WasmExpression ne(const WasmExpression &lhs, const WasmExpression &rhs) {
        return BINARY_OP(NeVecF64x2);
    }
    WasmExpression neg(const WasmExpression &expr) {
        return UNARY_OP(NegVecF64x2);
    }
};

template <BinaryenIndex Index> class Local : public WasmExpression {
  private:
    BinaryenModuleRef module;
    WasmType *type;
    BinaryenExpressionRef expression;

  public:
    Local(BinaryenModuleRef module, WasmType *type)
        : module(module), type(type),
          expression(BinaryenLocalGet(module, Index, type->type)) {}

    WasmExpression operator=(const WasmExpression &expr) {
        return (*this = (BinaryenExpressionRef)expr);
    }

    WasmExpression operator=(const BinaryenExpressionRef expr) {
        // tee isn't really ideal but the cost of beauty
        // (autodrop + tee + wasm-opt = get anyways)
        return WasmExpression(
            module, type, BinaryenLocalTee(module, Index, expr, type->type));
    }
};
