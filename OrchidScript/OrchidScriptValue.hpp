// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"

#include <functional>
#include <valarray>
#include <memory>
#include <vector>
#include <string>
#include <list>
#include <map>

struct MhdScriptVal;
struct MhdScriptRef;
struct MhdScriptInvalidOp
{
public:
    template<typename... T>
    MhdScriptInvalidOp(T...) {}
};
struct MhdScriptInvalidBinOp : MhdScriptInvalidOp
{
public:
    template<typename... T>
    MhdScriptInvalidBinOp(T... t): MhdScriptInvalidOp(t...) {}
};
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptVal final
{
    friend struct MhdScriptRef;
    enum class Type {
        LGC,
        INT,
        DBL, 
        STR,
        PTR,
        LST,
        MAP,
        FUN,
    };	// enum class Type
public:
    Type m_type;
    union {
        void* m_val_ptr;
        std::string m_val_str;
        std::valarray<bool  > m_val_lgc;
        std::valarray<int   > m_val_int;
        std::valarray<double> m_val_dbl;
        std::shared_ptr<
            std::list<MhdScriptVal>> m_val_lst;
        std::shared_ptr<
            std::map<MhdScriptVal, MhdScriptVal>> m_val_map;
        std::shared_ptr<
            std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>> m_val_fun;
    };
public:
    MHD_INTERFACE
    ~MhdScriptVal();
public:
    MhdScriptVal(Type type)
        : m_type(type)
    {
        switch (m_type) {
            case MhdScriptVal::Type::LGC:
                new (&m_val_lgc) std::valarray<bool>(1);
                break;
            case MhdScriptVal::Type::INT:
                new (&m_val_int) std::valarray<int>(1);
                break;
            case MhdScriptVal::Type::DBL:
                new (&m_val_dbl) std::valarray<double>(1);
                break;
            case MhdScriptVal::Type::STR:
                new (&m_val_str) std::string();
                break;
            case MhdScriptVal::Type::PTR:
                new (&m_val_ptr) void* (nullptr);
                break;
            default:
                ORCHID_ASSERT(0);
                break;
        }
    }
public:
    MhdScriptVal()
        : m_type(Type::PTR)
        , m_val_ptr(nullptr) {}
    explicit MhdScriptVal(std::nullptr_t)
        : m_type(Type::PTR)
        , m_val_ptr(nullptr) {}
    explicit MhdScriptVal(void* val)
        : m_type(Type::PTR)
        , m_val_ptr(val) {}
    explicit MhdScriptVal(bool val)
        : m_type(Type::LGC)
        , m_val_lgc(&val, 1) {}
    explicit MhdScriptVal(const std::valarray<bool>& val)
        : m_type(Type::LGC)
        , m_val_lgc(val) {}
    explicit MhdScriptVal(int val)
        : m_type(Type::INT)
        , m_val_int(&val, 1) {}
    explicit MhdScriptVal(unsigned int val)
        : MhdScriptVal((int)val) {}
    explicit MhdScriptVal(const std::valarray<int>& val)
        : m_type(Type::INT)
        , m_val_int(val) {}
    explicit MhdScriptVal(double val)
        : m_type(Type::DBL)
        , m_val_dbl(&val, 1) {}
    explicit MhdScriptVal(const std::valarray<double>& val)
        : m_type(Type::DBL)
        , m_val_dbl(val) {}
    explicit MhdScriptVal(const char* val)
        : m_type(Type::STR)
        , m_val_str(val) {}
    explicit MhdScriptVal(const std::string& val)
        : m_type(Type::STR)
        , m_val_str(val) {}
    explicit MhdScriptVal(const std::map<MhdScriptVal, MhdScriptVal>& val)
        : m_type(Type::MAP)
        , m_val_map(std::make_shared<
                    std::map<MhdScriptVal, MhdScriptVal>>(val)) {}
    explicit MhdScriptVal(MhdScriptVal(*val)(const std::vector<MhdScriptVal>&))
        : m_type(Type::FUN)
        , m_val_fun(std::make_shared<
                    std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>>(val)) {}
    explicit MhdScriptVal(const std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>& val)
        : m_type(Type::FUN)
        , m_val_fun(std::make_shared<
                    std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>>(val)) {}
    template<typename T>
    MhdScriptVal& operator=(const T& t)
    {
        return *this = MhdScriptVal(t);
    }
public:
    MHD_INTERFACE
    MhdScriptVal& operator=(MhdScriptVal&& other) noexcept;
    MhdScriptVal(MhdScriptVal&& other) noexcept
        : m_type(Type::PTR)
        , m_val_ptr(nullptr)
    {
        *this = std::forward<MhdScriptVal>(other);
    }
public:
    MHD_INTERFACE
    MhdScriptVal& operator=(const MhdScriptVal& other);
    MhdScriptVal(const MhdScriptVal& other)
        : m_type(Type::PTR)
        , m_val_ptr(nullptr)
    {
        *this = other;
    }
    MHD_INTERFACE
    MhdScriptVal& operator=(const std::vector<MhdScriptVal>& others);
    explicit MhdScriptVal(const std::vector<MhdScriptVal>& others)
        : m_type(Type::PTR)
        , m_val_ptr(nullptr)
    {
        *this = others;
    }
public:
    MHD_INTERFACE
    MhdScriptVal& operator=(const MhdScriptRef& other);
    MhdScriptVal(const MhdScriptRef& other)
        : m_type(Type::PTR)
        , m_val_ptr(nullptr)
    {
        *this = other;
    }
public:
    MHD_INTERFACE
    void* data()
    {
        switch (m_type) {
            case MhdScriptVal::Type::LGC:
                return &m_val_lgc[0];
            case MhdScriptVal::Type::INT:
                return &m_val_int[0];
            case MhdScriptVal::Type::DBL:
                return &m_val_dbl[0];
            case MhdScriptVal::Type::STR:
                return &m_val_str[0];
            case MhdScriptVal::Type::PTR:
                return &m_val_ptr;
            default:
                return nullptr;
        }
    }
public:
    MHD_INTERFACE
    static MhdScriptVal operator_arithmetic(MhdScriptKind op,
                                            const MhdScriptVal& lhs);
    MhdScriptVal operator+() const
    {
        return operator_arithmetic(MhdScriptKind::OP_ADD, *this);
    }
    MhdScriptVal operator-() const
    {
        return operator_arithmetic(MhdScriptKind::OP_SUB, *this);
    }
public:
    MHD_INTERFACE
    static MhdScriptVal operator_arithmetic(MhdScriptKind op,
                                            const MhdScriptVal& lhs, const MhdScriptVal& rhs);
    MhdScriptVal operator+(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptKind::OP_ADD, *this, other);
    }
    MhdScriptVal operator-(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptKind::OP_SUB, *this, other);
    }
    MhdScriptVal operator*(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptKind::OP_MUL, *this, other);
    }
    MhdScriptVal operator/(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptKind::OP_DIV, *this, other);
    }
    MhdScriptVal operator%(const MhdScriptVal& other) const
    {
        return operator_arithmetic(MhdScriptKind::OP_MOD, *this, other);
    }
    MhdScriptVal& operator+=(const MhdScriptVal& other)
    {
        return *this = *this + other;
    }
public:
    MHD_INTERFACE
    static MhdScriptVal operator_logical(MhdScriptKind op,
                                         const MhdScriptVal& lhs);
    MhdScriptVal operator!() const
    {
        return operator_logical(MhdScriptKind::OP_NOT, *this);
    }
public:
    MHD_INTERFACE
    static MhdScriptVal operator_logical(MhdScriptKind op,
                                         const MhdScriptVal& lhs, const MhdScriptVal& rhs);
    MhdScriptVal operator==(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptKind::OP_EQ, *this, other);
    }
    MhdScriptVal operator!=(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptKind::OP_NEQ, *this, other);
    }
    MhdScriptVal operator<(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptKind::OP_LT, *this, other);
    }
    MhdScriptVal operator<=(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptKind::OP_LTE, *this, other);
    }
    MhdScriptVal operator>(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptKind::OP_GT, *this, other);
    }
    MhdScriptVal operator>=(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptKind::OP_GTE, *this, other);
    }
    MhdScriptVal operator&&(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptKind::OP_AND, *this, other);
    }
    MhdScriptVal operator||(const MhdScriptVal& other) const
    {
        return operator_logical(MhdScriptKind::OP_OR, *this, other);
    }
public:
    MHD_INTERFACE
    static MhdScriptVal operator_bitwise(MhdScriptKind op,
                                         const MhdScriptVal& lhs);
    MhdScriptVal operator~() const
    {
        return operator_bitwise(MhdScriptKind::OP_NOT_BW, *this);
    }
public:
    MHD_INTERFACE
    static MhdScriptVal operator_bitwise(MhdScriptKind op,
                                         const MhdScriptVal& lhs, const MhdScriptVal& rhs);
    MhdScriptVal operator&(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptKind::OP_AND_BW, *this, other);
    }
    MhdScriptVal operator|(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptKind::OP_OR_BW, *this, other);
    }
    MhdScriptVal operator^(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptKind::OP_XOR_BW, *this, other);
    }
    MhdScriptVal operator<<(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptKind::OP_LSHIFT, *this, other);
    }
    MhdScriptVal operator>>(const MhdScriptVal& other) const
    {
        return operator_bitwise(MhdScriptKind::OP_RSHIFT, *this, other);
    }
public:
    MHD_INTERFACE
    MhdScriptRef operator[](const MhdScriptVal&);
    MHD_INTERFACE
    MhdScriptRef operator[](const std::vector<MhdScriptVal>&);
    MHD_INTERFACE
    MhdScriptVal operator[](const MhdScriptVal&) const;
    MHD_INTERFACE
    MhdScriptVal operator[](const std::vector<MhdScriptVal>&) const;
public:
    MHD_INTERFACE
    MhdScriptVal operator()(const std::vector<MhdScriptVal>& args) const;
    template<typename... T>
    MhdScriptVal operator()(const T&... args) const
    {
        return (*this)({ MhdScriptVal(args)... });
    }
public:
    MHD_INTERFACE
    static MhdScriptVal operator_cast(MhdScriptVal::Type tp,
                                      const MhdScriptVal& lhs);
    MHD_INTERFACE
    operator bool() const;
    MHD_INTERFACE
    explicit operator int() const;
    MHD_INTERFACE
    explicit operator double() const;
    MHD_INTERFACE
    explicit operator std::string() const;
    explicit operator const char*() const
    {
        return static_cast<std::string>(*this).c_str();
    }
    MHD_INTERFACE
    explicit operator void*() const;
};	// struct MhdScriptVal
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptRef final
{
    friend struct MhdScriptVal;
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
    explicit MhdScriptRef(bool* ref)
        : m_type(MhdScriptVal::Type::LGC)
        , m_ref_lgc(ref) {}
    explicit MhdScriptRef(int* ref)
        : m_type(MhdScriptVal::Type::INT)
        , m_ref_int(ref) {}
    explicit MhdScriptRef(double* ref)
        : m_type(MhdScriptVal::Type::DBL)
        , m_ref_dbl(ref) {}
    explicit MhdScriptRef(MhdScriptVal* ref)
        : m_type(MhdScriptVal::Type::MAP)
        , m_ref(ref) {}
    template<typename T>
    MhdScriptRef& operator=(const T& t)
    {
        return *this = MhdScriptVal(t);
    }
public:
    MHD_INTERFACE
    MhdScriptRef& operator=(const MhdScriptVal& val);
    MhdScriptRef& operator=(const MhdScriptRef& other) 
    { 
        m_type = other.m_type;
        m_ref = other.m_ref;
        return *this; 
    }
    MhdScriptRef(const MhdScriptRef& other)
        : m_type(MhdScriptVal::Type::PTR)
        , m_ref_ptr(nullptr) 
    {
        *this = other;
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
    MhdScriptRef operator[](const std::vector<MhdScriptVal>& args)
    {
        if (m_type == MhdScriptVal::Type::MAP) {
            return (*m_ref)[args];
        } else {
            throw MhdScriptInvalidOp(*this);
        }
    }
    MhdScriptVal operator[](const std::vector<MhdScriptVal>& args) const
    {
        if (m_type == MhdScriptVal::Type::MAP) {
            return (*m_ref)[args];
        } else {
            throw MhdScriptInvalidOp(*this);
        }
    }
public:
    MhdScriptVal operator()(const std::vector<MhdScriptVal>& args) const
    {
        if (m_type == MhdScriptVal::Type::MAP) {
            return (*m_ref)(args);
        } else {
            throw MhdScriptInvalidOp(*this);
        }
    }
};	// struct MhdScriptRef
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptValIter
{
};  // struct MhdScriptValIter
//########################################################################################################
//########################################################################################################
//########################################################################################################
