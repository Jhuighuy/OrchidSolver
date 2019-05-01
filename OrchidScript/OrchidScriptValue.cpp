// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptValue.hpp"

#include <type_traits>
#include <algorithm>
#include <sstream>

//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
inline void
operator_dtor(T& lhs)
{
    /// Destruct an arbitrary VALUE.
    lhs.~T();
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal::~MhdScriptVal()
{
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            operator_dtor(m_val_lgc);
            break;
        case MhdScriptVal::Type::INT:
            operator_dtor(m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_dtor(m_val_dbl);
            break;
        case MhdScriptVal::Type::STR:
            operator_dtor(m_val_str);
            break;
        case MhdScriptVal::Type::PTR:
            operator_dtor(m_val_ptr);
            break;
        case MhdScriptVal::Type::MAP:
            operator_dtor(m_val_map);
            break;
        case MhdScriptVal::Type::FUN:
            operator_dtor(m_val_fun);
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    m_type = Type::PTR;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
inline void
operator_assignment_move_apply(T& lhs, T& rhs)
{
    /// Apply MOVE-ASSIGNMENT operator for arbitrary objects.
    new (&lhs) T(std::move(rhs));
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal& 
MhdScriptVal::operator=(MhdScriptVal&& other) noexcept
{
    /// Assign VALUE of a VALUE.
    if (this != &other) {
        this->~MhdScriptVal();
        std::swap(m_type, other.m_type);
        switch (m_type) {
            case MhdScriptVal::Type::LGC:
                operator_assignment_move_apply(m_val_lgc, other.m_val_lgc);
                break;
            case MhdScriptVal::Type::INT:
                operator_assignment_move_apply(m_val_int, other.m_val_int);
                break;
            case MhdScriptVal::Type::DBL:
                operator_assignment_move_apply(m_val_dbl, other.m_val_dbl);
                break;
            case MhdScriptVal::Type::STR:
                operator_assignment_move_apply(m_val_str, other.m_val_str);
                break;
            case MhdScriptVal::Type::PTR:
                operator_assignment_move_apply(m_val_ptr, other.m_val_ptr);
                break;
            case MhdScriptVal::Type::MAP:
                operator_assignment_move_apply(m_val_map, other.m_val_map);
                break;
            case MhdScriptVal::Type::FUN:
                operator_assignment_move_apply(m_val_fun, other.m_val_fun);
                break;
            default:
                ORCHID_ASSERT(0); 
                break;
        }
    }
    return *this;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
inline void
operator_assignment_apply(T& lhs, const T& rhs)
{
    /// Apply ASSIGNMENT operator for arbitrary objects.
    new (&lhs) T(rhs);
}
template<typename T, typename U>
inline void
operator_assignment_apply(std::valarray<T>& lhs, const std::vector<U>& rhs)
{
    /// Apply ASSIGNMENT operator for arbitrary value arrays.
    new (&lhs) std::valarray<T>(rhs.size());
    for (std::size_t i = 0; i < rhs.size(); ++i) {
        lhs[i] = static_cast<T>(rhs[i]);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal& 
MhdScriptVal::operator=(const MhdScriptVal& other)
{
    /// Assign VALUE of a VALUE.
    if (this != &other) {
        this->~MhdScriptVal();
        m_type = other.m_type;
        switch (m_type) {
            case MhdScriptVal::Type::LGC:
                operator_assignment_apply(m_val_lgc, other.m_val_lgc);
                break;
            case MhdScriptVal::Type::INT:
                operator_assignment_apply(m_val_int, other.m_val_int);
                break;
            case MhdScriptVal::Type::DBL:
                operator_assignment_apply(m_val_dbl, other.m_val_dbl);
                break;
            case MhdScriptVal::Type::STR:
                operator_assignment_apply(m_val_str, other.m_val_str);
                break;
            case MhdScriptVal::Type::PTR:
                operator_assignment_apply(m_val_ptr, other.m_val_ptr);
                break;
            case MhdScriptVal::Type::FUN:
                operator_assignment_apply(m_val_fun, other.m_val_fun);
                break;
            case MhdScriptVal::Type::MAP:
                operator_assignment_apply(m_val_map, other.m_val_map);
                break;
            default:
                ORCHID_ASSERT(0); 
                break;
        }
    }
    return *this;
}
MHD_INTERFACE
MhdScriptVal&
MhdScriptVal::operator=(const std::vector<MhdScriptVal>& others)
{
    /// Assign VALUE of an ARRAY OF VALUES.
    this->~MhdScriptVal();
    m_type = MhdScriptVal::Type::LGC;
    for (const MhdScriptVal& val : others) {
        switch (val.m_type) {
            case MhdScriptVal::Type::LGC:
            case MhdScriptVal::Type::INT:
            case MhdScriptVal::Type::DBL:
                m_type = std::max(m_type, val.m_type);
                break;
            default:
                throw MhdScriptInvalidOp(val);
        }
    }
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            operator_assignment_apply(m_val_lgc, others);
            break;
        case MhdScriptVal::Type::INT:
            operator_assignment_apply(m_val_int, others);
            break;
        case MhdScriptVal::Type::DBL:
            operator_assignment_apply(m_val_dbl, others);
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    return *this;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal& 
MhdScriptVal::operator=(const MhdScriptRef& other)
{
    /// Assign VALUE of a REFERENCE.
    switch (other.m_type) {
        case MhdScriptVal::Type::LGC:
            *this = *other.m_ref_lgc;
            break;
        case MhdScriptVal::Type::INT:
            *this = *other.m_ref_int;
            break;
        case MhdScriptVal::Type::DBL:
            *this = *other.m_ref_dbl;
            break;
        case MhdScriptVal::Type::MAP:
            *this = *other.m_ref;
            break;
        case MhdScriptVal::Type::PTR:
            *this = *other.m_ref_ptr;
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    return *this;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptRef& 
MhdScriptRef::operator=(const MhdScriptVal& other) 
{ 
    /// Assign VALUE of a VALUE.
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            *m_ref_lgc = static_cast<bool>(other);
            break;
        case MhdScriptVal::Type::INT:
            *m_ref_int = static_cast<int>(other);
            break;
        case MhdScriptVal::Type::DBL:
            *m_ref_dbl = static_cast<double>(other);
            break;
        case MhdScriptVal::Type::PTR:
            *m_ref_ptr = static_cast<void*>(other);
            break;
        case MhdScriptVal::Type::MAP:
            *m_ref = other;
            break;
        default:
            throw MhdScriptInvalidOp(other);
    }
    return *this;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
inline typename std::enable_if<std::is_arithmetic<T>::value>::type
operator_arithmetic_apply(MhdScriptKind op,
                          std::valarray<T>& lhs)
{
    /// Apply an UNARY ARITHMETIC operator for arithmetic value arrays. 
    switch (op) {
        case MhdScriptKind::OP_ADD: 
            lhs = std::move(+lhs);
            break;
        case MhdScriptKind::OP_SUB:
            lhs = std::move(-lhs);
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
template<typename T, typename S, typename U>
inline void
operator_arithmetic_apply(MhdScriptKind op, U& val,
                          std::map<T, S>& lhs)
{
    /// Apply an UNARY ARITHMETIC operator for maps.
    switch (op) {
        case MhdScriptKind::OP_ADD:
            val = lhs[T("operator{+}")]({val});
            break;
        case MhdScriptKind::OP_SUB:
            val = lhs[T("operator{-}")]({val});
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_arithmetic(MhdScriptKind op,
                                  const MhdScriptVal& lhs)
{
    /// Apply an UNARY ARITHMETIC operator.
    MhdScriptVal::Type tp;
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
            tp = MhdScriptVal::Type::INT;
            break;
        case MhdScriptVal::Type::DBL:
            tp = MhdScriptVal::Type::DBL;
            break;
        case MhdScriptVal::Type::MAP:
            tp = MhdScriptVal::Type::MAP;
            break;
        default:
            throw MhdScriptInvalidOp(op, lhs);
    }
    MhdScriptVal new_lhs;
    if (lhs.m_type != tp) {
        new_lhs = MhdScriptVal::operator_cast(tp, lhs);
    } else {
        new_lhs = lhs;
    }
    switch (tp) {
        case MhdScriptVal::Type::INT:
            operator_arithmetic_apply(op, new_lhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_arithmetic_apply(op, new_lhs.m_val_dbl);
            break;
        case MhdScriptVal::Type::MAP:
            operator_arithmetic_apply(op, new_lhs, *new_lhs.m_val_map);
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    return new_lhs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
inline typename std::enable_if<std::is_integral<T>::value>::type
operator_arithmetic_apply(MhdScriptKind op,
                          std::valarray<T>& lhs, const std::valarray<T>& rhs)
{
    /// Apply a BINARY ARITHMETIC operator for integer value arrays. 
    switch (op) {
        case MhdScriptKind::OP_ADD: 
            lhs += rhs;
            break;
        case MhdScriptKind::OP_SUB:
            lhs -= rhs;
            break;
        case MhdScriptKind::OP_MUL: 
            lhs *= rhs;
            break;
        case MhdScriptKind::OP_DIV: 
            lhs /= rhs;
            break;
        case MhdScriptKind::OP_MOD: 
            lhs %= rhs;
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
template<typename T>
inline typename std::enable_if<std::is_floating_point<T>::value>::type
operator_arithmetic_apply(MhdScriptKind op,
                          std::valarray<T>& lhs, const std::valarray<T>& rhs)
{
    /// Apply a BINARY ARITHMETIC operator for floating value arrays. 
    switch (op) {
        case MhdScriptKind::OP_ADD: 
            lhs += rhs;
            break;
        case MhdScriptKind::OP_SUB:
            lhs -= rhs;
            break;
        case MhdScriptKind::OP_MUL: 
            lhs *= rhs;
            break;
        case MhdScriptKind::OP_DIV: 
            lhs /= rhs;
            break;
        case MhdScriptKind::OP_POW: 
            lhs = std::move(std::pow(lhs, rhs));
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
template<typename T>
inline void
operator_arithmetic_apply(MhdScriptKind op,
                          std::basic_string<T>& lhs, const std::basic_string<T>& rhs)
{
    /// Apply a BINARY ARITHMETIC operator for strings. 
    switch (op) {
        case MhdScriptKind::OP_ADD: 
            lhs += rhs;
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
template<typename U>
inline void
operator_arithmetic_apply(MhdScriptKind op, 
                          U& lhs, const U& rhs)
{
    /// Apply an BINARY ARITHMETIC operator for maps.
    switch (op) {
        case MhdScriptKind::OP_ADD:
            lhs = std::move(lhs[U("operator+")]({ lhs, rhs }));
            break;
        case MhdScriptKind::OP_SUB:
            lhs = std::move(lhs[U("operator-")]({ lhs, rhs }));
            break;
        case MhdScriptKind::OP_MUL:
            lhs = std::move(lhs[U("operator*")]({ lhs, rhs }));
            break;
        case MhdScriptKind::OP_DIV:
            lhs = std::move(lhs[U("operator/")]({ lhs, rhs }));
            break;
        case MhdScriptKind::OP_MOD:
            lhs = std::move(lhs[U("operator%")]({ lhs, rhs }));
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_arithmetic(MhdScriptKind op,
                                  const MhdScriptVal& lhs, const MhdScriptVal& rhs)
{
    /// Apply a BINARY ARITHMETIC operator.
    MhdScriptVal::Type tp;
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
            tp = MhdScriptVal::Type::INT;
            break;
        case MhdScriptVal::Type::DBL:
            tp = MhdScriptVal::Type::DBL;
            break;
        case MhdScriptVal::Type::STR:
            tp = MhdScriptVal::Type::STR;
            if (tp != rhs.m_type) {
                throw MhdScriptInvalidOp(rhs);
            }
            break;
        case MhdScriptVal::Type::MAP:
            tp = MhdScriptVal::Type::MAP;
            break;
        default:
            throw MhdScriptInvalidOp(lhs);
    }
    switch (rhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
        case MhdScriptVal::Type::DBL:
            tp = std::max(tp, rhs.m_type);
            break;
        case MhdScriptVal::Type::STR:
        case MhdScriptVal::Type::MAP:
            break;
        default:
            throw MhdScriptInvalidOp(rhs);
    }
    MhdScriptVal new_lhs;
    MhdScriptVal new_rhs;
    if (lhs.m_type != tp) {
        new_lhs = MhdScriptVal::operator_cast(tp, lhs);
    } else {
        new_lhs = lhs;
    }
    if (rhs.m_type != tp && tp != MhdScriptVal::Type::MAP) {
        new_rhs = MhdScriptVal::operator_cast(tp, rhs);
    } else {
        new_rhs = rhs;
    }
    switch (tp) {
        case MhdScriptVal::Type::INT:
            operator_arithmetic_apply(op, new_lhs.m_val_int, new_rhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_arithmetic_apply(op, new_lhs.m_val_dbl, new_rhs.m_val_dbl);
            break;
        case MhdScriptVal::Type::STR:
            operator_arithmetic_apply(op, new_lhs.m_val_str, new_rhs.m_val_str);
            break;
        case MhdScriptVal::Type::MAP:
            operator_arithmetic_apply(op, new_lhs, new_rhs);
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    return new_lhs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T, typename U>
inline typename std::enable_if<std::is_arithmetic<T>::value>::type
operator_logical_apply(MhdScriptKind op,
                       U& val, const std::valarray<T>& lhs)
{
    /// Apply an UNARY LOGICAL operator for arithmetic value array. 
    switch (op) {
        case MhdScriptKind::OP_NOT:
            val = std::move(!lhs);
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
template<typename T, typename U>
inline void
operator_logical_apply(MhdScriptKind op,
                       U& val, const T& lhs)
{
    /// Apply an UNARY LOGICAL operator for arbitrary objects. 
    switch (op) {
        case MhdScriptKind::OP_NOT: 
            val = std::move(!lhs);
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_logical(MhdScriptKind op,
                               const MhdScriptVal& lhs)
{
    /// Apply an UNARY LOGICAL operator.
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
        case MhdScriptVal::Type::DBL:
        case MhdScriptVal::Type::PTR:
            break;
        default:
            throw MhdScriptInvalidOp(lhs);
    }
    MhdScriptVal new_lhs = lhs;
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
            operator_logical_apply(op, new_lhs, new_lhs.m_val_lgc);
            break;
        case MhdScriptVal::Type::INT:
            operator_logical_apply(op, new_lhs, new_lhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_logical_apply(op, new_lhs, new_lhs.m_val_dbl);
            break;
        case MhdScriptVal::Type::PTR:
            operator_logical_apply(op, new_lhs, new_lhs.m_val_ptr);
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    return new_lhs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T, typename U>
inline typename std::enable_if<std::is_arithmetic<T>::value>::type
operator_logical_apply(MhdScriptKind op, U& val, 
                       const std::valarray<T>& lhs, const std::valarray<T>& rhs)
{ 
    /// Apply a BINARY LOGICAL operator for arithmetic value arrays. 
    switch (op) {
        case MhdScriptKind::OP_EQ: 
            val = std::move(lhs == rhs);
            break;
        case MhdScriptKind::OP_NEQ: 
            val = std::move(lhs != rhs);
            break;
        case MhdScriptKind::OP_LT: 
            val = std::move(lhs <  rhs);
            break;
        case MhdScriptKind::OP_LTE: 
            val = std::move(lhs <= rhs);
            break;
        case MhdScriptKind::OP_GT: 
            val = std::move(lhs >  rhs);
            break;
        case MhdScriptKind::OP_GTE: 
            val = std::move(lhs >= rhs);
            break;
        case MhdScriptKind::OP_AND: 
            val = std::move(lhs && rhs);
            break;
        case MhdScriptKind::OP_OR: 
            val = std::move(lhs || rhs);
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
template<typename T, typename U>
inline void
operator_logical_apply(MhdScriptKind op, U& val, 
                       const std::basic_string<T>& lhs, const std::basic_string<T>& rhs)
{
    /// Apply a BINARY LOGICAL operator for strings. 
    switch (op) {
        case MhdScriptKind::OP_EQ: 
            val = std::move(lhs == rhs);
            break;
        case MhdScriptKind::OP_NEQ: 
            val = std::move(lhs != rhs);
            break;
        case MhdScriptKind::OP_LT: 
            val = std::move(lhs <  rhs);
            break;
        case MhdScriptKind::OP_LTE: 
            val = std::move(lhs <= rhs);
            break;
        case MhdScriptKind::OP_GT: 
            val = std::move(lhs >  rhs);
            break;
        case MhdScriptKind::OP_GTE: 
            val = std::move(lhs >= rhs);
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
template<typename T, typename U>
inline void
operator_logical_apply(MhdScriptKind op, U& val, 
                       const T& lhs, const T& rhs)
{
    /// Apply a BINARY LOGICAL operator for arbitrary objects. 
    switch (op) {
        case MhdScriptKind::OP_EQ: 
            val = std::move(lhs == rhs);
            break;
        case MhdScriptKind::OP_NEQ: 
            val = std::move(lhs != rhs);
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_logical(MhdScriptKind op,
                               const MhdScriptVal& lhs, const MhdScriptVal& rhs)
{
    /// Apply a BINARY LOGICAL operator.
    MhdScriptVal::Type tp;
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
        case MhdScriptVal::Type::DBL:
            tp = lhs.m_type;
            break;
        case MhdScriptVal::Type::STR:
        case MhdScriptVal::Type::PTR:
            tp = lhs.m_type;
            if (tp != rhs.m_type) {
                throw MhdScriptInvalidOp(rhs);
            }
            break;
        default:
            throw MhdScriptInvalidOp(lhs);
    }
    switch (rhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
        case MhdScriptVal::Type::DBL:
            tp = std::max(tp, rhs.m_type);
            break;
        case MhdScriptVal::Type::STR:
        case MhdScriptVal::Type::PTR:
            break;
        default:
            throw MhdScriptInvalidOp(rhs);
    }
    MhdScriptVal new_lhs;
    MhdScriptVal new_rhs;
    if (lhs.m_type != tp) {
        new_lhs = MhdScriptVal::operator_cast(tp, lhs);
    } else {
        new_lhs = lhs;
    }
    if (rhs.m_type != tp) {
        new_rhs = MhdScriptVal::operator_cast(tp, rhs);
    } else {
        new_rhs = rhs;
    }
    switch (tp) {
        case MhdScriptVal::Type::LGC:
            operator_logical_apply(op, new_lhs, new_lhs.m_val_lgc, new_rhs.m_val_lgc);
            break;
        case MhdScriptVal::Type::INT:
            operator_logical_apply(op, new_lhs, new_lhs.m_val_int, new_rhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_logical_apply(op, new_lhs, new_lhs.m_val_dbl, new_rhs.m_val_dbl);
            break;
        case MhdScriptVal::Type::STR:
            operator_logical_apply(op, new_lhs, new_lhs.m_val_str, new_rhs.m_val_str);
            break;
        case MhdScriptVal::Type::PTR:
            operator_logical_apply(op, new_lhs, new_lhs.m_val_ptr, new_rhs.m_val_ptr);
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    return new_lhs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
inline typename std::enable_if<std::is_integral<T>::value>::type
operator_bitwise_apply(MhdScriptKind op,
                       std::valarray<T>& lhs)
{
    /// Apply an UNARY BITWISE operator for integer value array. 
    switch (op) {
        case MhdScriptKind::OP_NOT_BW: 
            lhs = std::move(~lhs);
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_bitwise(MhdScriptKind op,
                               const MhdScriptVal& lhs)
{
    /// Apply an UNARY BITWISE operator.
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
            break;
        default:
            throw MhdScriptInvalidOp(lhs);
    }
    MhdScriptVal new_lhs;
    if (lhs.m_type != MhdScriptVal::Type::INT) {
        new_lhs = MhdScriptVal::operator_cast(MhdScriptVal::Type::INT, lhs);
    } else {
        new_lhs = lhs;
    }
    operator_bitwise_apply(op, new_lhs.m_val_int);
    return new_lhs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
inline typename std::enable_if<std::is_integral<T>::value>::type
operator_bitwise_apply(MhdScriptKind op,
                       std::valarray<T>& lhs, const std::valarray<T>& rhs)
{
    /// Apply a BINARY LOGICAL operator for integer value arrays. 
    switch (op) {
        case MhdScriptKind::OP_AND_BW: 
            lhs &= rhs;
            break;
        case MhdScriptKind::OP_OR_BW: 
            lhs |= rhs;
            break;
        case MhdScriptKind::OP_XOR_BW: 
            lhs ^= rhs;
            break;
        case MhdScriptKind::OP_LSHIFT: 
            lhs <<= rhs;
            break;
        case MhdScriptKind::OP_RSHIFT: 
            lhs >>= rhs;
            break;
        default:
            throw MhdScriptInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_bitwise(MhdScriptKind op,
                               const MhdScriptVal& lhs, const MhdScriptVal& rhs)
{
    /// Apply a BINARY BITWISE operator.
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
            break;
        default:
            throw MhdScriptInvalidOp(lhs);
    }
    switch (rhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
            break;
        default:
            throw MhdScriptInvalidOp(rhs);
    }
    MhdScriptVal new_lhs;
    MhdScriptVal new_rhs;
    if (lhs.m_type != MhdScriptVal::Type::INT) {
        new_lhs = MhdScriptVal::operator_cast(MhdScriptVal::Type::INT, lhs);
    } else {
        new_lhs = lhs;
    }
    if (rhs.m_type != MhdScriptVal::Type::INT) {
        new_rhs = MhdScriptVal::operator_cast(MhdScriptVal::Type::INT, rhs);
    } else {
        new_rhs = rhs;
    }
    operator_bitwise_apply(op, new_lhs.m_val_int, new_rhs.m_val_int);
    return new_lhs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptRef
MhdScriptVal::operator[](const MhdScriptVal& index)
{
    /// Apply an INDEX OPERATOR.
    /// @TODO Implement me correctly.
    std::size_t idx;
    MhdScriptRef ref;
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            idx = static_cast<size_t>((int) index);
            if (m_val_lgc.size() <= idx) {
                m_val_lgc.resize(idx + 1);
            }
            ref = std::move(MhdScriptRef(&m_val_lgc[idx]));
            break;
        case MhdScriptVal::Type::INT:
            idx = static_cast<size_t>((int) index);
            if (m_val_int.size() <= idx) {
                m_val_int.resize(idx + 1);
            }
            ref = std::move(MhdScriptRef(&m_val_int[idx]));
            break;
        case MhdScriptVal::Type::DBL:
            idx = static_cast<size_t>((int) index);
            if (m_val_dbl.size() <= idx) {
                m_val_dbl.resize(idx + 1);
            }
            ref = std::move(MhdScriptRef(&m_val_dbl[idx]));
            break;
        case MhdScriptVal::Type::MAP:
            ref = MhdScriptRef(&(*m_val_map)[index]);
            break;
        default:
            throw MhdScriptInvalidOp(*this);
    }
    return ref;
}
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator[](const MhdScriptVal& index) const
{
    /// Apply an INDEX operator.
    return MhdScriptVal(const_cast<MhdScriptVal&>(*this)[index]);
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptRef
MhdScriptVal::operator[](const std::vector<MhdScriptVal>& indices)
{
    /// Apply an INDEX operator.
    /// @TODO Implement me correctly.
    return (*this)[indices[0]];
}
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator[](const std::vector<MhdScriptVal>& indices) const
{
    /// Apply an INDEX operator.
    return MhdScriptVal(const_cast<MhdScriptVal&>(*this)[indices]);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal 
MhdScriptVal::operator()(const std::vector<MhdScriptVal>& args) const
{
    /// Apply a CALL operator.
    if (m_type == MhdScriptVal::Type::FUN) {
        return (*m_val_fun)(args);
    } else {
        throw MhdScriptInvalidOp(*this);
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename X, typename T, typename U>
inline typename std::enable_if<std::is_convertible<T, X>::value>::type
operator_cast_apply(U& val,
                    const std::valarray<T>& lhs)
{
    /// Apply a CAST operator for convertible value arrays. 
    std::valarray<X> rhs_cast(lhs.size());
    for (std::size_t i = 0; i < lhs.size(); ++i) {
        rhs_cast[i] = static_cast<X>(lhs[i]);
    }
    val = std::move(rhs_cast);
}
template<typename X, typename T, typename U>
inline typename std::enable_if<std::is_convertible<T, X>::value>::type
operator_cast_apply(U& val,
                    const T& lhs)
{
    /// Apply a CAST operator for convertible arbitrary objects. 
    X rhs_cast{std::move(static_cast<X>(lhs))};
    val = std::move(rhs_cast);
}
template<typename X, typename T, typename U>
inline void
operator_cast_apply(U& val,
                    const std::basic_string<T>& lhs)
{
    /// Apply a CAST operator from string to value. 
    X rhs_cast{};
    std::basic_istringstream<T>(lhs) >> rhs_cast;
    val = std::move(rhs_cast);
}
//--------------------------------------------------------------------------------------------------------
template<typename X, typename T, typename U>
inline void
operator_cast_apply_str(U& val,
                        const std::valarray<T>& lhs)
{
    /// Cast to STRING operator for value arrays. 
    std::basic_ostringstream<X> rhs_cast;
    rhs_cast.setf(std::ios::showpoint);
    rhs_cast << "(/";
    if (lhs.size() > 0) {
        rhs_cast << lhs[0];
        for (size_t i = 1; i < lhs.size(); ++i) {
            rhs_cast << ", " << lhs[i];
        }
    }
    rhs_cast << "/)";
    val = std::move(rhs_cast.str());
}
template<typename X, typename T, typename S, typename U>
inline void
operator_cast_apply_str(U& val,
                        const std::map<T, S>& lhs)
{
    /// Cast to STRING operator for maps. 
    std::basic_ostringstream<X> rhs_cast;
    rhs_cast.setf(std::ios::showpoint);
    rhs_cast << "{/";
    if (lhs.size() > 0) {
        auto lhs_iter = lhs.begin();
        rhs_cast << static_cast<std::string>(lhs_iter->first ) << ":"
                 << static_cast<std::string>(lhs_iter->second);
        for (++lhs_iter; lhs_iter != lhs.end(); ++lhs_iter) {
            rhs_cast << ", "
                     << static_cast<std::string>(lhs_iter->first ) << ":"
                     << static_cast<std::string>(lhs_iter->second);
        }
    }
    rhs_cast << "/}";
    val = std::move(rhs_cast.str());
}
template<typename X, typename T, typename U>
inline void
operator_cast_apply_str(U& val,
                        const T& lhs)
{
    /// Cast to STRING operator for arbitrary objects. 
    std::basic_ostringstream<X> rhs_cast;
    rhs_cast << lhs;
    val = std::move(rhs_cast.str());
}
//--------------------------------------------------------------------------------------------------------
template<typename T, typename U>
inline void
operator_cast_apply(MhdScriptVal::Type tp, U& val,
                    const T& lhs)
{
    /// Apply a CAST operator for arbitrary objects.
    switch (tp) {
        case MhdScriptVal::Type::LGC:
            operator_cast_apply<bool>(val, lhs);
            break;
        case MhdScriptVal::Type::INT:
            operator_cast_apply<int>(val, lhs);
            break;
        case MhdScriptVal::Type::DBL:
            operator_cast_apply<double>(val, lhs);
            break;
        case MhdScriptVal::Type::STR:
            operator_cast_apply_str<char>(val, lhs);
            break;
        default:
            throw MhdScriptInvalidOp(lhs);
    }
}
template<typename T, typename S, typename U>
inline void
operator_cast_apply(MhdScriptVal::Type tp, U& val,
                    const std::map<T, S>& lhs)
{
    /// Apply a CAST operator for arbitrary maps.
    switch (tp) {
        case MhdScriptVal::Type::STR:
            operator_cast_apply_str<char>(val, lhs);
            break;
        default:
            throw MhdScriptInvalidOp(lhs);
    }
}
template<typename U>
inline void
operator_cast_apply(MhdScriptVal::Type tp, U& val,
                    void* lhs)
{
    /// Apply a CAST operator for pointers.
    switch (tp) {
        case MhdScriptVal::Type::LGC:
            operator_cast_apply<bool>(val, lhs != nullptr);
            break;
        case MhdScriptVal::Type::STR:
            operator_cast_apply_str<char>(val, lhs);
            break;
        default:
            throw MhdScriptInvalidOp(lhs);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_cast(MhdScriptVal::Type tp,
                            const MhdScriptVal& lhs)
{
    /// Apply a CAST operator.
    if (lhs.m_type == tp) {
        return lhs;
    }
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
        case MhdScriptVal::Type::DBL:
        case MhdScriptVal::Type::STR:
        case MhdScriptVal::Type::PTR:
        case MhdScriptVal::Type::MAP:
            break;
        default:
            throw MhdScriptInvalidOp(lhs);
    }
    MhdScriptVal new_lhs = lhs;
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
            operator_cast_apply(tp, new_lhs, new_lhs.m_val_lgc);
            break;
        case MhdScriptVal::Type::INT:
            operator_cast_apply(tp, new_lhs, new_lhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_cast_apply(tp, new_lhs, new_lhs.m_val_dbl);
            break;
        case MhdScriptVal::Type::STR:
            operator_cast_apply(tp, new_lhs, new_lhs.m_val_str);
            break;
        case MhdScriptVal::Type::PTR:
            operator_cast_apply(tp, new_lhs, new_lhs.m_val_ptr);
            break;
        case MhdScriptVal::Type::MAP:
            operator_cast_apply(tp, new_lhs, *new_lhs.m_val_map.get());
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    return new_lhs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal::operator bool() const 
{
    /// Cast to BOOL operator.
    bool val;
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            val = m_val_lgc.size() > 0 && bool(m_val_lgc[0]);
            break;
        case MhdScriptVal::Type::INT:
            val = m_val_int.size() > 0 && bool(m_val_int[0]);
            break;
        case MhdScriptVal::Type::DBL:
            val = m_val_dbl.size() > 0 && bool(m_val_dbl[0]);
            break;
        case MhdScriptVal::Type::STR:
            val = !m_val_str.empty();
            break;
        case MhdScriptVal::Type::PTR:
            val = m_val_ptr != nullptr;
            break;
        default:
            throw MhdScriptInvalidOp(*this);
    }
    return val;
}
MHD_INTERFACE
MhdScriptVal::operator int() const 
{
    /// Cast to INT operator.
    int val{};
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            val = m_val_lgc.size() > 0 ? int(m_val_lgc[0]) : val;
            break;
        case MhdScriptVal::Type::INT:
            val = m_val_int.size() > 0 ? int(m_val_int[0]) : val;
            break;
        case MhdScriptVal::Type::DBL:
            val = m_val_dbl.size() > 0 ? int(m_val_dbl[0]) : val;
            break;
        default:
            throw MhdScriptInvalidOp(*this);
    }
    return val;
}
MHD_INTERFACE
MhdScriptVal::operator double() const 
{
    /// Cast to DOUBLE operator.
    double val{};
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            val = m_val_lgc.size() > 0 ? double(m_val_lgc[0]) : val;
            break;
        case MhdScriptVal::Type::INT:
            val = m_val_int.size() > 0 ? double(m_val_int[0]) : val;
            break;
        case MhdScriptVal::Type::DBL:
            val = m_val_dbl.size() > 0 ? double(m_val_dbl[0]) : val;
            break;
        default:
            throw MhdScriptInvalidOp(*this);
    }
    return val;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal::operator std::string() const 
{
    /// Cast to STRING operator.
    std::string val;
    if (m_type == MhdScriptVal::Type::STR) {
        val = m_val_str;
    } else {
        val = operator_cast(MhdScriptVal::Type::STR, *this).m_val_str;
    }
    return val;
}
MHD_INTERFACE
MhdScriptVal::operator void*() const 
{
    /// Cast to POINTER operator.
    void* val;
    if (m_type == MhdScriptVal::Type::PTR) {
        val = m_val_ptr;
    } else {
        throw MhdScriptInvalidOp(*this);
    }
    return val;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################


