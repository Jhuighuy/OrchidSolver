// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"

#include <cstdint>
#include <cmath>

#define MHD_NAN_TAGGING 1
#ifndef MHD_NAN_TAGGING
#define MHD_NAN_TAGGING 0
#endif  // ifndef MHD_NAN_TAGGING

namespace Val2 {
//########################################################################################################
//########################################################################################################
//########################################################################################################
enum struct MhdScriptType : std::uint8_t
{
    TYPE_BOOL,
    TYPE_INT,
    TYPE_DOUBLE,
    TYPE_STRING,
    TYPE_POINTER,
    TYPE_LIST,
    TYPE_MAP,
    TYPE_PROC,
};  // enum struct MhdScriptType
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptTaggedVal
{
#if MHD_NAN_TAGGING
private:
    enum : std::uint64_t {
        s_tag_dbl = 0xFFF8000000000000,
        s_tag_int = 0xFFF9000000000000,
        s_tag_ptr = 0xFFFA000000000000,
    };
    union {
        double m_val_dbl;
        std::uint64_t m_val_bits;
    };
public:
    MhdScriptTaggedVal()
        : m_val_bits(s_tag_ptr) {}
    MhdScriptTaggedVal(std::nullptr_t)
        : m_val_bits(s_tag_ptr) {}
public:
    explicit MhdScriptTaggedVal(float val)
        : m_val_dbl(val) {}
    explicit MhdScriptTaggedVal(double val)
        : m_val_dbl(val) {}
public:
    inline
    explicit MhdScriptTaggedVal(std::int16_t val)
        : m_val_bits(s_tag_int | val) {}
    inline
    explicit MhdScriptTaggedVal(std::uint16_t val)
        : m_val_bits(s_tag_int | val) {}
    inline
    explicit MhdScriptTaggedVal(std::int32_t val)
        : m_val_bits(s_tag_int | val) {}
    inline
    explicit MhdScriptTaggedVal(std::uint32_t val)
        : m_val_bits(s_tag_int | val) {}
    inline
    explicit MhdScriptTaggedVal(std::int64_t val)
        : m_val_bits(s_tag_int | val) 
    {
        ORCHID_ASSERT(val == static_cast<std::int32_t>(val));
    }
    inline
    explicit MhdScriptTaggedVal(std::uint64_t val)
        : m_val_bits(s_tag_int | val) 
    {
        ORCHID_ASSERT(val == static_cast<std::uint32_t>(val));
    }
public:
    inline
    explicit MhdScriptTaggedVal(const void* val)
        : m_val_bits(s_tag_ptr | reinterpret_cast<std::uintptr_t>(val))
    {
        ORCHID_ASSERT((s_tag_ptr & reinterpret_cast<std::uintptr_t>(val)) == 0);
    }
public:
    inline 
    bool is_dbl() const
    {
        return (m_val_bits < s_tag_dbl);
    }
    inline 
    bool is_int() const
    {
        return (m_val_bits & s_tag_int) == s_tag_int;
    }
    inline
    bool is_ptr() const
    {
        return (m_val_bits & s_tag_ptr) == s_tag_ptr;
    }
public:
    inline 
    double get_dbl() const
    {
        ORCHID_ASSERT(is_dbl());
        return m_val_dbl;
    }
    inline 
    int get_int() const
    {
        ORCHID_ASSERT(is_int());
        return static_cast<int>(m_val_bits | ~s_tag_int);
    }
    inline
    void* get_ptr() const
    {
        ORCHID_ASSERT(is_ptr());
        return reinterpret_cast<void*>(m_val_bits | ~s_tag_ptr);
    }
#else   // if MHD_NAN_TAGGING
#error Implement me!
#endif  // if MHD_NAN_TAGGING
};  // struct MhdScriptTaggedVal
//--------------------------------------------------------------------------------------------------------
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptVal : public MhdScriptTaggedVal
{
public:
    MhdScriptVal()
        : MhdScriptTaggedVal() {}
    template<typename T>
    explicit MhdScriptVal(T val)
        : MhdScriptTaggedVal(val) {}
public:
    MHD_INTERFACE
    static MhdScriptVal operator_arithmetic(MhdScriptKind op,
                                            const MhdScriptVal& lhs);
    MHD_INTERFACE
    static MhdScriptVal operator_arithmetic(MhdScriptKind op,
                                            const MhdScriptVal& lhs, const MhdScriptVal& rhs);
};  // struct MhdScriptVal
//--------------------------------------------------------------------------------------------------------
inline
MhdScriptVal 
operator+(MhdScriptVal lhs)
{
    /// Plus value.
    if (lhs.is_int() || lhs.is_dbl()) {
        return lhs;
    } else {
        return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_ADD, lhs);
    }
}
inline
MhdScriptVal 
operator-(MhdScriptVal lhs)
{
    /// Plus value.
    if (lhs.is_int()) {
        const int lhs_val = lhs.get_int();
        return MhdScriptVal{ -lhs_val };
    } else if (lhs.is_dbl()) {
        const double lhs_val = lhs.get_dbl();
        return MhdScriptVal{ -lhs_val };
    } else {
        return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_ADD, lhs);
    }
}

//--------------------------------------------------------------------------------------------------------
inline
MhdScriptVal 
operator+(MhdScriptVal lhs, MhdScriptVal rhs)
{
    /// Add two values.
    if (lhs.is_int() && rhs.is_int()) {
        const int lhs_val = lhs.get_int();
        const int rhs_val = rhs.get_int();
        return MhdScriptVal{ lhs_val + rhs_val };
    } else {
        double lhs_val;
        if (lhs.is_dbl()) {
            lhs_val = lhs.get_dbl();
        } else if (lhs.is_int()) {
            lhs_val = lhs.get_int();
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_ADD, lhs, rhs);
        }
        double rhs_val;
        if (rhs.is_dbl()) {
            rhs_val = rhs.get_dbl();
        } else if (rhs.is_int()) {
            rhs_val = rhs.get_int();
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_ADD, lhs, rhs);
        }
        return MhdScriptVal{ lhs_val + rhs_val };
    }
}
inline
MhdScriptVal 
operator-(MhdScriptVal lhs, MhdScriptVal rhs)
{
    /// Subtract two values.
    if (lhs.is_int() && rhs.is_int()) {
        const int lhs_val = lhs.get_int();
        const int rhs_val = rhs.get_int();
        return MhdScriptVal{ lhs_val - rhs_val };
    } else {
        double lhs_val;
        if (lhs.is_dbl()) {
            lhs_val = lhs.get_dbl();
        } else if (lhs.is_int()) {
            lhs_val = lhs.get_int();
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_SUB, lhs, rhs);
        }
        double rhs_val;
        if (rhs.is_dbl()) {
            rhs_val = rhs.get_dbl();
        } else if (rhs.is_int()) {
            rhs_val = rhs.get_int();
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_SUB, lhs, rhs);
        }
        return MhdScriptVal{ lhs_val - rhs_val };
    }
}
inline
MhdScriptVal 
operator*(MhdScriptVal lhs, MhdScriptVal rhs)
{
    /// Multiply two values.
    if (lhs.is_int() && rhs.is_int()) {
        const int lhs_val = lhs.get_int();
        const int rhs_val = rhs.get_int();
        return MhdScriptVal{ lhs_val * rhs_val };
    } else {
        double lhs_val;
        if (lhs.is_dbl()) {
            lhs_val = lhs.get_dbl();
        } else if (lhs.is_int()) {
            lhs_val = lhs.get_int();
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_MUL, lhs, rhs);
        }
        double rhs_val;
        if (rhs.is_dbl()) {
            rhs_val = rhs.get_dbl();
        } else if (rhs.is_int()) {
            rhs_val = rhs.get_int();
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_MUL, lhs, rhs);
        }
        return MhdScriptVal{ lhs_val * rhs_val };
    }
}
inline
MhdScriptVal 
operator/(MhdScriptVal lhs, MhdScriptVal rhs)
{
    /// Divide two values.
    if (lhs.is_int() && rhs.is_int()) {
        const int lhs_val = lhs.get_int();
        const int rhs_val = rhs.get_int();
        if (rhs_val == 0) {
            throw MhdScriptVal{ "Integer division by zero." };
        }
        return MhdScriptVal{ lhs_val / rhs_val };
    } else {
        double lhs_val;
        if (lhs.is_dbl()) {
            lhs_val = lhs.get_dbl();
        } else if (lhs.is_int()) {
            lhs_val = lhs.get_int();
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_DIV, lhs, rhs);
        }
        double rhs_val;
        if (rhs.is_dbl()) {
            rhs_val = rhs.get_dbl();
        } else if (rhs.is_int()) {
            rhs_val = rhs.get_int();
            if (rhs_val == 0) {
                throw MhdScriptVal{ "Integer division by zero." };
            }
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_DIV, lhs, rhs);
        }
        return MhdScriptVal{ lhs_val * rhs_val };
    }
}
inline
MhdScriptVal 
operator%(MhdScriptVal lhs, MhdScriptVal rhs)
{
    /// Remainder of division.
    if (lhs.is_int() && rhs.is_int()) {
        const int lhs_val = lhs.get_int();
        const int rhs_val = rhs.get_int();
        if (rhs_val == 0) {
            throw MhdScriptVal{ "Integer division by zero." };
        }
        return MhdScriptVal{ lhs_val % rhs_val };
    } else {
        double lhs_val;
        if (lhs.is_dbl()) {
            lhs_val = lhs.get_dbl();
        } else if (lhs.is_int()) {
            lhs_val = lhs.get_int();
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_MOD, lhs, rhs);
        }
        double rhs_val;
        if (rhs.is_dbl()) {
            rhs_val = rhs.get_dbl();
        } else if (rhs.is_int()) {
            rhs_val = rhs.get_int();
            if (rhs_val == 0) {
                throw MhdScriptVal{ "Integer division by zero." };
            }
        } else {
            return MhdScriptVal::operator_arithmetic(MhdScriptKind::OP_MOD, lhs, rhs);
        }
        return MhdScriptVal{ std::fmod(lhs_val, rhs_val) };
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
}   // namespace Val2
