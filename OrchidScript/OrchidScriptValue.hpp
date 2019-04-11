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
#define MhdSciptInvalidOp MhdInvalidOp
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptRef;
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
    explicit MhdScriptVal(void* val)
        : m_type(Type::PTR)
        , m_val_ptr(val) {}
public:
    explicit MhdScriptVal(bool val)
        : m_type(Type::LGC)
        , m_val_lgc(new std::valarray<bool>(&val, 1)) {}
    explicit MhdScriptVal(const std::valarray<bool>& val)
        : m_type(Type::LGC)
        , m_val_lgc(new std::valarray<bool>(val)) {}
public:
    explicit MhdScriptVal(int val)
        : m_type(Type::INT)
        , m_val_int(new std::valarray<int>(&val, 1)) {}
    explicit MhdScriptVal(const std::valarray<int>& val)
        : m_type(Type::INT)
        , m_val_int(new std::valarray<int>(val)) {}
public:
    explicit MhdScriptVal(double val)
        : m_type(Type::DBL)
        , m_val_dbl(new std::valarray<double>(&val, 1)) {}
    explicit MhdScriptVal(const std::valarray<double>& val)
        : m_type(Type::DBL)
        , m_val_dbl(new std::valarray<double>(val)) {}
public:
    explicit MhdScriptVal(const char* val)
        : m_type(Type::STR)
        , m_val_str(new std::string(val)) {}
    explicit MhdScriptVal(const std::string& val)
        : m_type(Type::STR)
        , m_val_str(new std::string(val)) {}
public:
    explicit MhdScriptVal(const std::map<MhdScriptVal, MhdScriptVal>& val)
        : m_type(Type::MAP)
        , m_val_map(new std::map<MhdScriptVal, MhdScriptVal>(val)) {}
public:
    explicit MhdScriptVal(MhdScriptVal(*val)(const std::vector<MhdScriptVal>&))
        : m_type(Type::FUN)
        , m_val_fun(new std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>(val)) {}
    explicit MhdScriptVal(const std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>& val)
        : m_type(Type::FUN)
        , m_val_fun(new std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>(val)) {}
public:
    MhdScriptVal(MhdScriptVal&& other) noexcept
        : m_type(Type::PTR)
        , m_val_ptr(nullptr)
    {
        *this = std::forward<MhdScriptVal>(other);
    }
    MhdScriptVal(const MhdScriptVal& other)
        : m_type(Type::PTR)
        , m_val_ptr(nullptr)
    {
        *this = other;
    }
    explicit MhdScriptVal(const std::vector<MhdScriptVal>& others)
        : m_type(Type::PTR)
        , m_val_ptr(nullptr)
    {
        *this = others;
    }
    MhdScriptVal& operator=(MhdScriptVal&& other) noexcept;
    MhdScriptVal& operator=(const MhdScriptVal& other);
    MhdScriptVal& operator=(const std::vector<MhdScriptVal>& others);
    template<typename T>
    MhdScriptVal& operator=(const T& t)
    {
        return *this = MhdScriptVal(t);
    }
public:
    MhdScriptVal(const MhdScriptRef& other)
        : m_type(Type::PTR)
        , m_val_ptr(nullptr)
    {
        *this = other;
    }
    MhdScriptVal& operator=(const MhdScriptRef& other);
public:
    static MhdScriptVal operator_arithmetic(MhdScriptToken::Kind op,
                                            const MhdScriptVal& lhs);
    MhdScriptVal operator+() const
    {
        return operator_arithmetic(MhdScriptToken::Kind::OP_ADD, *this);
    }
    MhdScriptVal operator-() const
    {
        return operator_arithmetic(MhdScriptToken::Kind::OP_SUB, *this);
    }
public:
    static MhdScriptVal operator_arithmetic(MhdScriptToken::Kind op,
                                            const MhdScriptVal& lhs, const MhdScriptVal& rhs);
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
    MhdScriptVal operator!() const
    {
        return operator_logical(MhdScriptToken::Kind::OP_NOT, *this);
    }
public:
    static MhdScriptVal operator_logical(MhdScriptToken::Kind op,
                                         const MhdScriptVal& lhs, const MhdScriptVal& rhs);
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
    MhdScriptVal operator~() const
    {
        return operator_bitwise(MhdScriptToken::Kind::OP_NOT_BW, *this);
    }
public:
    static MhdScriptVal operator_bitwise(MhdScriptToken::Kind op,
                                         const MhdScriptVal& lhs, const MhdScriptVal& rhs);
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
    //template<typename T>
    //MhdScriptRef operator[](const T& index) const
    //{
    //    return (*this)[MhdScriptVal(index)];
    //}
public:
    MhdScriptVal operator()(const std::vector<MhdScriptVal>& args) const;
    template<typename... T>
    MhdScriptVal operator()(const T&... args) const
    {
        return (*this)({ MhdScriptVal(args)... });
    }
public:
    static MhdScriptVal operator_cast(MhdScriptVal::Type tp,
                                      const MhdScriptVal& lhs);
    operator bool() const;
    explicit operator int() const;
    explicit operator double() const;
    explicit operator std::string() const;
    explicit operator void*() const;
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
        bool  * m_ref_lgc;
        int   * m_ref_int;
        double* m_ref_dbl;
        void ** m_ref_ptr;
    };
public:
    MhdScriptRef()
        : m_type(MhdScriptVal::Type::PTR)
        , m_ref_ptr(nullptr) {}
    explicit MhdScriptRef(void** ref)
        : m_type(MhdScriptVal::Type::PTR)
        , m_ref_ptr(ref) {}
public:
    explicit MhdScriptRef(bool* ref)
        : m_type(MhdScriptVal::Type::LGC)
        , m_ref_lgc(ref) {}
public:
    explicit MhdScriptRef(int* ref)
        : m_type(MhdScriptVal::Type::INT)
        , m_ref_int(ref) {}
public:
    explicit MhdScriptRef(double* ref)
        : m_type(MhdScriptVal::Type::DBL)
        , m_ref_dbl(ref) {}
public:
    explicit MhdScriptRef(MhdScriptVal* ref)
        : m_type(MhdScriptVal::Type::MAP)
        , m_ref(ref) {}
public:
    MhdScriptRef& operator=(const MhdScriptVal& val);
    template<typename T>
    MhdScriptRef& operator=(const T& t)
    {
        return *this = MhdScriptVal(t);
    }
public:
    MhdScriptRef(const MhdScriptRef& other)
        : m_type(MhdScriptVal::Type::PTR)
        , m_ref_ptr(nullptr) 
    {
        *this = other;
    }
    MhdScriptRef& operator=(const MhdScriptRef& other) 
    { 
        m_type = other.m_type;
        m_ref = other.m_ref;
        return *this; 
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
public:
    MhdScriptRef operator[](const std::vector<MhdScriptVal>& args) const
    {
        if (m_type == MhdScriptVal::Type::MAP) {
            return (*m_ref)[args];
        } else {
            throw MhdInvalidOp(*this);
        }
    }
};	// struct MhdScriptRef
//########################################################################################################
//########################################################################################################
//########################################################################################################
