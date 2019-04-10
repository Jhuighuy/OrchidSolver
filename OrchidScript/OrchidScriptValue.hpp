// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScriptScanner.hpp"

#include <functional>
#include <valarray>
#include <vector>
#include <string>
#include <map>

struct MhdInvalidOp
{
public:
    MhdInvalidOp(...) {}
};
struct MhdScriptRef;
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptVal final
{
public:
    enum class Type
    {
        LGC,
        INT,
        DBL, 
        STR,
        PTR,
        MAP,
        FUN,
    };	// enum class Type
public:
    Type m_type;
    union {
        void* m_val_ptr;
        std::string* m_val_str;
        std::valarray<bool  >* m_val_lgc;
        std::valarray<int   >* m_val_int;
        std::valarray<double>* m_val_dbl;
        std::map<MhdScriptVal, MhdScriptVal>* m_val_map;
        std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>* m_val_fun;
    };
public:
    ~MhdScriptVal();
public:
    MhdScriptVal()
        : m_type(Type::PTR)
        , m_val_ptr(nullptr) {}
    MhdScriptVal(void* val)
        : m_type(Type::PTR)
        , m_val_ptr(val) {}
public:
    MhdScriptVal(bool val)
        : m_type(Type::LGC)
        , m_val_lgc(new std::valarray<bool>(&val, 1)) {}
    MhdScriptVal(const std::valarray<bool>& val)
        : m_type(Type::LGC)
        , m_val_lgc(new std::valarray<bool>(val)) {}
public:
    MhdScriptVal(int val)
        : m_type(Type::INT)
        , m_val_int(new std::valarray<int>(&val, 1)) {}
    MhdScriptVal(const std::valarray<int>& val)
        : m_type(Type::INT)
        , m_val_int(new std::valarray<int>(val)) {}
public:
    MhdScriptVal(double val)
        : m_type(Type::DBL)
        , m_val_dbl(new std::valarray<double>(&val, 1)) {}
    MhdScriptVal(const std::valarray<double>& val)
        : m_type(Type::DBL)
        , m_val_dbl(new std::valarray<double>(val)) {}
public:
    MhdScriptVal(const char* val)
        : m_type(Type::STR)
        , m_val_str(new std::string(val)) {}
    MhdScriptVal(const std::string& val)
        : m_type(Type::STR)
        , m_val_str(new std::string(val)) {}
public:
    MhdScriptVal(const std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>& val)
        : m_type(Type::FUN)
        , m_val_fun(new std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>(val)) {}
public:
    MhdScriptVal(MhdScriptVal&& val) noexcept;
    MhdScriptVal(const MhdScriptVal& val);
    MhdScriptVal(const MhdScriptRef& ref);
    MhdScriptVal& operator=(MhdScriptVal&& other) noexcept;
    MhdScriptVal& operator=(const MhdScriptVal& other);
    MhdScriptVal& operator=(const MhdScriptRef& other);
    template<typename T>
    MhdScriptVal& operator=(const T& t)
    {
        return *this = MhdScriptVal(t);
    }
public:
    static MhdScriptVal operator_arithmetic(MhdScriptToken::Kind op,
                                            const MhdScriptVal& lhs);
    static MhdScriptVal operator_arithmetic(MhdScriptToken::Kind op,
                                            const MhdScriptVal& lhs, const MhdScriptVal& rhs);
    MhdScriptVal operator+() const
    {
        return operator_arithmetic(MhdScriptToken::Kind::OP_ADD, *this);
    }
    MhdScriptVal operator-() const
    {
        return operator_arithmetic(MhdScriptToken::Kind::OP_SUB, *this);
    }
    MhdScriptVal operator+(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptToken::Kind::OP_ADD, *this, other);
    }
    MhdScriptVal operator-(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptToken::Kind::OP_SUB, *this, other);
    }
    MhdScriptVal operator*(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptToken::Kind::OP_MUL, *this, other);
    }
    MhdScriptVal operator/(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptToken::Kind::OP_DIV, *this, other);
    }
    MhdScriptVal operator%(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptToken::Kind::OP_MOD, *this, other);
    }
public:
    static MhdScriptVal operator_logical(MhdScriptToken::Kind op,
                                         const MhdScriptVal& lhs);
    static MhdScriptVal operator_logical(MhdScriptToken::Kind op,
                                         const MhdScriptVal& lhs, const MhdScriptVal& rhs);
    MhdScriptVal operator!() const
    {
        return operator_logical(MhdScriptToken::Kind::OP_NOT, *this);
    }
    MhdScriptVal operator==(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptToken::Kind::OP_EQ, *this, other);
    }
    MhdScriptVal operator!=(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptToken::Kind::OP_NEQ, *this, other);
    }
    MhdScriptVal operator<(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptToken::Kind::OP_LT, *this, other);
    }
    MhdScriptVal operator<=(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptToken::Kind::OP_LTE, *this, other);
    }
    MhdScriptVal operator>(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptToken::Kind::OP_GT, *this, other);
    }
    MhdScriptVal operator>=(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptToken::Kind::OP_GTE, *this, other);
    }
    MhdScriptVal operator&&(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptToken::Kind::OP_AND, *this, other);
    }
    MhdScriptVal operator||(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptToken::Kind::OP_OR, *this, other);
    }
public:
    static MhdScriptVal operator_bitwise(MhdScriptToken::Kind op,
                                         const MhdScriptVal& lhs);
    static MhdScriptVal operator_bitwise(MhdScriptToken::Kind op,
                                         const MhdScriptVal& lhs, const MhdScriptVal& rhs);
    MhdScriptVal operator~() const
    {
        return operator_bitwise(MhdScriptToken::Kind::OP_NOT_BW, *this);
    }
    MhdScriptVal operator&(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptToken::Kind::OP_AND_BW, *this, other);
    }
    MhdScriptVal operator|(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptToken::Kind::OP_OR_BW, *this, other);
    }
    MhdScriptVal operator^(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptToken::Kind::OP_XOR_BW, *this, other);
    }
    MhdScriptVal operator<<(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptToken::Kind::OP_LSHIFT, *this, other);
    }
    MhdScriptVal operator>>(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptToken::Kind::OP_RSHIFT, *this, other);
    }
public:
    MhdScriptRef operator[](const MhdScriptVal& index) const;
    MhdScriptRef operator[](const std::vector<MhdScriptVal>& args) const;
    MhdScriptVal operator()(const std::vector<MhdScriptVal>& args) const;
public:
    static MhdScriptVal operator_cast(MhdScriptVal::Type tp,
                                      const MhdScriptVal& lhs);
    operator bool() const;
    operator int() const;
    operator double() const;
    operator std::string() const;
};	// struct MhdScriptVal
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptRef final
{
public:
    MhdScriptVal::Type m_type;
    union {
        MhdScriptVal* m_ref;
        void ** m_ref_ptr;
        bool  * m_ref_lgc;
        int   * m_ref_int;
        double* m_ref_dbl;
    };
public:
    MhdScriptRef(void* = nullptr): m_type(MhdScriptVal::Type::PTR), m_ref(nullptr) {}
    MhdScriptRef(int* ref): m_type(MhdScriptVal::Type::INT), m_ref_int(ref) {}
    MhdScriptRef(double* ref): m_type(MhdScriptVal::Type::DBL), m_ref_dbl(ref) {}
public:
    MhdScriptRef& operator=(const MhdScriptRef& other) 
    { 
        m_type = other.m_type;
        m_ref = other.m_ref;
        return *this; 
    }
    MhdScriptRef& operator=(const MhdScriptVal& val) 
    { 
        *m_ref_dbl = (*val.m_val_dbl)[0];
        return *this;
    }
    template<typename T>
    MhdScriptRef& operator=(const T& t)
    {
        return *this = MhdScriptVal(t);
    }
public:
    MhdScriptVal operator+() const
    {
        return +MhdScriptVal(*this);
    }
    MhdScriptVal operator-() const
    {
        return -MhdScriptVal(*this);
    }
    MhdScriptVal operator+(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) + other;
    }
    MhdScriptVal operator-(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) - other;
    }
    MhdScriptVal operator*(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) * other;
    }
    MhdScriptVal operator/(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) / other;
    }
    MhdScriptVal operator%(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) % other;
    }
public:
    MhdScriptVal operator!() const
    {
        return !MhdScriptVal(*this);
    }
    MhdScriptVal operator==(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) == other;
    }
    MhdScriptVal operator!=(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) != other;
    }
    MhdScriptVal operator<(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) < other;
    }
    MhdScriptVal operator<=(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) <= other;
    }
    MhdScriptVal operator>(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) > other;
    }
    MhdScriptVal operator>=(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) >= other;
    }
    MhdScriptVal operator&&(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) && other;
    }
    MhdScriptVal operator||(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) || other;
    }
public:
    MhdScriptVal operator~() const
    {
        return ~MhdScriptVal(*this);
    }
    MhdScriptVal operator&(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) & other;
    }
    MhdScriptVal operator|(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) | other;
    }
    MhdScriptVal operator^(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) ^ other;
    }
    MhdScriptVal operator<<(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) << other;
    }
    MhdScriptVal operator>>(const MhdScriptVal& other) const
    {
        return MhdScriptVal(*this) >> other;
    }
};	// struct MhdScriptRef
//########################################################################################################
//########################################################################################################
//########################################################################################################
