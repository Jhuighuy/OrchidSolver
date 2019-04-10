// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptValue.hpp"

#include <type_traits>
#include <algorithm>
#include <sstream>

//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal::~MhdScriptVal()
{
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            delete m_val_lgc;
            break;
        case MhdScriptVal::Type::INT:
            delete m_val_int;
            break;
        case MhdScriptVal::Type::DBL:
            delete m_val_dbl;
            break;
        case MhdScriptVal::Type::STR:
            delete m_val_str;
            break;
        case MhdScriptVal::Type::PTR:
            break;
        case MhdScriptVal::Type::FUN:
            delete m_val_fun;
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    m_type = Type::PTR;
    m_val_ptr = nullptr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal::MhdScriptVal(MhdScriptVal&& other) noexcept
    : m_type(Type::PTR)
    , m_val_ptr(nullptr)
{
    *this = std::forward<MhdScriptVal>(other);
}
MHD_INTERFACE
MhdScriptVal::MhdScriptVal(const MhdScriptVal& other)
    : m_type(Type::PTR)
    , m_val_ptr(nullptr)
{
    *this = other;
}
MHD_INTERFACE
MhdScriptVal::MhdScriptVal(const MhdScriptRef& other)
    : m_type(Type::PTR)
    , m_val_ptr(nullptr)
{
    *this = other;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal& 
MhdScriptVal::operator=(MhdScriptVal&& other) noexcept
{
    // Assign VALUE of a VALUE.
    if (this != &other) {
        this->~MhdScriptVal();
        std::swap(m_type, other.m_type);
        std::swap(m_val_ptr, other.m_val_ptr);
    }
    return *this;
}
MHD_INTERFACE
MhdScriptVal& 
MhdScriptVal::operator=(const MhdScriptVal& other)
{
    // Assign VALUE of a VALUE.
    if (this != &other) {
        this->~MhdScriptVal();
        m_type = other.m_type;
        switch (m_type) {
            case MhdScriptVal::Type::LGC:
                m_val_lgc = new std::valarray<bool  >(*other.m_val_lgc);
                break;
            case MhdScriptVal::Type::INT:
                m_val_int = new std::valarray<int   >(*other.m_val_int);
                break;
            case MhdScriptVal::Type::DBL:
                m_val_dbl = new std::valarray<double>(*other.m_val_dbl);
                break;
            case MhdScriptVal::Type::STR:
                m_val_str = new std::string(*other.m_val_str);
                break;
            case MhdScriptVal::Type::PTR:
                m_val_ptr = other.m_val_ptr;
                break;
            case MhdScriptVal::Type::FUN:
                m_val_fun = new std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>(
                    *other.m_val_fun);
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
MhdScriptVal::operator=(const MhdScriptRef& other)
{
    // Assign VALUE of a REFERENCE.
    if (this != other.m_ref) {
        this->~MhdScriptVal();
        m_type = other.m_type;
        switch (m_type) {
            case MhdScriptVal::Type::LGC:
                m_val_lgc = new std::valarray<bool  >(other.m_ref_lgc, 1);
                break;
            case MhdScriptVal::Type::INT:
                m_val_int = new std::valarray<int   >(other.m_ref_int, 1);
                break;
            case MhdScriptVal::Type::DBL:
                m_val_dbl = new std::valarray<double>(other.m_ref_dbl, 1);
                break;
            case MhdScriptVal::Type::MAP:
                *this = *other.m_ref;
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
typename std::enable_if<std::is_arithmetic<T>::value>::type 
operator_arithmetic_apply(MhdScriptToken::Kind op,
                          std::valarray<T>& lhs)
{
    // Apply an UNARY ARITHMETIC operator for arithmetic value arrays. 
    switch (op) {
        case MhdScriptToken::Kind::OP_ADD: 
            lhs = std::move(+lhs);
            break;
        case MhdScriptToken::Kind::OP_SUB:
            lhs = std::move(-lhs);
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_arithmetic(MhdScriptToken::Kind op,
                                  const MhdScriptVal& lhs)
{
    // Apply an UNARY ARITHMETIC operator.
    MhdScriptVal::Type tp;
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
            tp = MhdScriptVal::Type::INT;
            break;
        case MhdScriptVal::Type::DBL:
            tp = MhdScriptVal::Type::DBL;
            break;
        default:
            throw MhdInvalidOp(lhs);
    }
    MhdScriptVal new_lhs;
    if (lhs.m_type != tp) {
        new_lhs = MhdScriptVal::operator_cast(tp, lhs);
    } else {
        new_lhs = lhs;
    }
    switch (tp) {
        case MhdScriptVal::Type::INT:
            operator_arithmetic_apply(op, *new_lhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_arithmetic_apply(op, *new_lhs.m_val_dbl);
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
typename std::enable_if<std::is_integral<T>::value>::type 
operator_arithmetic_apply(MhdScriptToken::Kind op,
                          std::valarray<T>& lhs, const std::valarray<T>& rhs)
{
    // Apply a BINARY ARITHMETIC operator for integer value arrays. 
    switch (op) {
        case MhdScriptToken::Kind::OP_ADD: 
            lhs += rhs;
            break;
        case MhdScriptToken::Kind::OP_SUB:
            lhs -= rhs;
            break;
        case MhdScriptToken::Kind::OP_MUL: 
            lhs *= rhs;
            break;
        case MhdScriptToken::Kind::OP_DIV: 
            lhs /= rhs;
            break;
        case MhdScriptToken::Kind::OP_MOD: 
            lhs %= rhs;
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
template<typename T>
typename std::enable_if<std::is_floating_point<T>::value>::type
operator_arithmetic_apply(MhdScriptToken::Kind op,
                          std::valarray<T>& lhs, const std::valarray<T>& rhs)
{
    // Apply a BINARY ARITHMETIC operator for floating value arrays. 
    switch (op) {
        case MhdScriptToken::Kind::OP_ADD: 
            lhs += rhs;
            break;
        case MhdScriptToken::Kind::OP_SUB:
            lhs -= rhs;
            break;
        case MhdScriptToken::Kind::OP_MUL: 
            lhs *= rhs;
            break;
        case MhdScriptToken::Kind::OP_DIV: 
            lhs /= rhs;
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
template<typename T>
void
operator_arithmetic_apply(MhdScriptToken::Kind op,
                          std::basic_string<T>& lhs, const std::basic_string<T>& rhs)
{
    // Apply a BINARY ARITHMETIC operator for strings. 
    switch (op) {
        case MhdScriptToken::Kind::OP_ADD: 
            lhs += rhs;
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_arithmetic(MhdScriptToken::Kind op,
                                  const MhdScriptVal& lhs, const MhdScriptVal& rhs)
{
    // Apply a BINARY ARITHMETIC operator.
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
                throw MhdInvalidOp(rhs);
            }
            break;
        default:
            throw MhdInvalidOp(lhs);
    }
    switch (rhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
        case MhdScriptVal::Type::DBL:
            tp = std::max(tp, rhs.m_type);
            break;
        case MhdScriptVal::Type::STR:
            break;
        default:
            throw MhdInvalidOp(rhs);
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
        case MhdScriptVal::Type::INT:
            operator_arithmetic_apply(op, *new_lhs.m_val_int, *new_rhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_arithmetic_apply(op, *new_lhs.m_val_dbl, *new_rhs.m_val_dbl);
            break;
        case MhdScriptVal::Type::STR:
            operator_arithmetic_apply(op, *new_lhs.m_val_str, *new_rhs.m_val_str);
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
typename std::enable_if<std::is_arithmetic<T>::value>::type 
operator_logical_apply(MhdScriptToken::Kind op,
                       U& val, const std::valarray<T>& lhs)
{
    // Apply an UNARY LOGICAL operator for arithmetic value array. 
    switch (op) {
        case MhdScriptToken::Kind::OP_EQ: 
            val = std::move(!lhs);
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
template<typename T, typename U>
void
operator_logical_apply(MhdScriptToken::Kind op,
                       U& val, const T& lhs)
{
    // Apply an UNARY LOGICAL operator for arbitrary objects. 
    switch (op) {
        case MhdScriptToken::Kind::OP_NOT: 
            val = std::move(!lhs);
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_logical(MhdScriptToken::Kind op,
                               const MhdScriptVal& lhs)
{
    // Apply an UNARY LOGICAL operator.
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
        case MhdScriptVal::Type::DBL:
        case MhdScriptVal::Type::PTR:
            break;
        default:
            throw MhdInvalidOp(lhs);
    }
    MhdScriptVal new_lhs = lhs;
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
            operator_logical_apply(op, new_lhs, *new_lhs.m_val_lgc);
            break;
        case MhdScriptVal::Type::INT:
            operator_logical_apply(op, new_lhs, *new_lhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_logical_apply(op, new_lhs, *new_lhs.m_val_dbl);
            break;
        case MhdScriptVal::Type::PTR:
            operator_logical_apply(op, new_lhs,  new_lhs.m_val_ptr);
            break;
        default:
            ORCHID_ASSERT(0); 
            break;
    }
    throw 0;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T, typename U>
typename std::enable_if<std::is_arithmetic<T>::value>::type 
operator_logical_apply(MhdScriptToken::Kind op, U& val, 
                       const std::valarray<T>& lhs, const std::valarray<T>& rhs)
{ 
    // Apply a BINARY LOGICAL operator for arithmetic value arrays. 
    switch (op) {
        case MhdScriptToken::Kind::OP_EQ: 
            val = std::move(lhs == rhs);
            break;
        case MhdScriptToken::Kind::OP_NEQ: 
            val = std::move(lhs != rhs);
            break;
        case MhdScriptToken::Kind::OP_LT: 
            val = std::move(lhs <  rhs);
            break;
        case MhdScriptToken::Kind::OP_LTE: 
            val = std::move(lhs <= rhs);
            break;
        case MhdScriptToken::Kind::OP_GT: 
            val = std::move(lhs >  rhs);
            break;
        case MhdScriptToken::Kind::OP_GTE: 
            val = std::move(lhs >= rhs);
            break;
        case MhdScriptToken::Kind::OP_AND: 
            val = std::move(lhs && rhs);
            break;
        case MhdScriptToken::Kind::OP_OR: 
            val = std::move(lhs || rhs);
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
template<typename T, typename U>
void
operator_logical_apply(MhdScriptToken::Kind op, U& val, 
                       const std::basic_string<T>& lhs, const std::basic_string<T>& rhs)
{
    // Apply a BINARY LOGICAL operator for strings. 
    switch (op) {
        case MhdScriptToken::Kind::OP_EQ: 
            val = std::move(lhs == rhs);
            break;
        case MhdScriptToken::Kind::OP_NEQ: 
            val = std::move(lhs != rhs);
            break;
        case MhdScriptToken::Kind::OP_LT: 
            val = std::move(lhs <  rhs);
            break;
        case MhdScriptToken::Kind::OP_LTE: 
            val = std::move(lhs <= rhs);
            break;
        case MhdScriptToken::Kind::OP_GT: 
            val = std::move(lhs >  rhs);
            break;
        case MhdScriptToken::Kind::OP_GTE: 
            val = std::move(lhs >= rhs);
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
template<typename T, typename U>
void
operator_logical_apply(MhdScriptToken::Kind op, U& val, 
                       const T& lhs, const T& rhs)
{
    // Apply a BINARY LOGICAL operator for arbitrary objects. 
    switch (op) {
        case MhdScriptToken::Kind::OP_EQ: 
            val = std::move(lhs == rhs);
            break;
        case MhdScriptToken::Kind::OP_NEQ: 
            val = std::move(lhs != rhs);
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_logical(MhdScriptToken::Kind op,
                               const MhdScriptVal& lhs, const MhdScriptVal& rhs)
{
    // Apply a BINARY LOGICAL operator.
    MhdScriptVal::Type tp;
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
        case MhdScriptVal::Type::DBL:
        case MhdScriptVal::Type::STR:
        case MhdScriptVal::Type::PTR:
            tp = lhs.m_type;
            if (tp != rhs.m_type) {
                throw MhdInvalidOp(rhs);
            }
            break;
        default:
            throw MhdInvalidOp(lhs);
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
            throw MhdInvalidOp(rhs);
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
            operator_logical_apply(op, new_lhs, *new_lhs.m_val_lgc, *new_rhs.m_val_lgc);
            break;
        case MhdScriptVal::Type::INT:
            operator_logical_apply(op, new_lhs, *new_lhs.m_val_int, *new_rhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_logical_apply(op, new_lhs, *new_lhs.m_val_dbl, *new_rhs.m_val_dbl);
            break;
        case MhdScriptVal::Type::STR:
            operator_logical_apply(op, new_lhs, *new_lhs.m_val_str, *new_rhs.m_val_str);
            break;
        case MhdScriptVal::Type::PTR:
            operator_logical_apply(op, new_lhs,  new_lhs.m_val_ptr,  new_rhs.m_val_ptr);
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
typename std::enable_if<std::is_integral<T>::value>::type 
operator_bitwise_apply(MhdScriptToken::Kind op,
                       std::valarray<T>& lhs)
{
    // Apply an UNARY BITWISE operator for integer value array. 
    switch (op) {
        case MhdScriptToken::Kind::OP_NOT_BW: 
            lhs = std::move(~lhs);
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_bitwise(MhdScriptToken::Kind op,
                               const MhdScriptVal& lhs)
{
    // Apply an UNARY BITWISE operator.
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
            break;
        default:
            throw MhdInvalidOp(lhs);
    }
    MhdScriptVal new_lhs;
    if (lhs.m_type != MhdScriptVal::Type::INT) {
        new_lhs = MhdScriptVal::operator_cast(MhdScriptVal::Type::INT, lhs);
    } else {
        new_lhs = lhs;
    }
    operator_bitwise_apply(op, *new_lhs.m_val_int);
    return new_lhs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
typename std::enable_if<std::is_integral<T>::value>::type 
operator_bitwise_apply(MhdScriptToken::Kind op,
                       std::valarray<T>& lhs, const std::valarray<T>& rhs)
{
    // Apply a BINARY LOGICAL operator for integer value arrays. 
    switch (op) {
        case MhdScriptToken::Kind::OP_AND_BW: 
            lhs &= rhs;
            break;
        case MhdScriptToken::Kind::OP_OR_BW: 
            lhs |= rhs;
            break;
        case MhdScriptToken::Kind::OP_XOR_BW: 
            lhs ^= rhs;
            break;
        case MhdScriptToken::Kind::OP_LSHIFT: 
            lhs <<= rhs;
            break;
        case MhdScriptToken::Kind::OP_RSHIFT: 
            lhs >>= rhs;
            break;
        default:
            throw MhdInvalidOp(op);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_bitwise(MhdScriptToken::Kind op,
                               const MhdScriptVal& lhs, const MhdScriptVal& rhs)
{
    // Apply a BINARY BITWISE operator.
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
            break;
        default:
            throw MhdInvalidOp(lhs);
    }
    switch (rhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
            break;
        default:
            throw MhdInvalidOp(rhs);
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
    operator_bitwise_apply(op, *new_lhs.m_val_int, *new_rhs.m_val_int);
    return new_lhs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptRef
MhdScriptVal::operator[](const MhdScriptVal& index) const
{
    // Apply an INDEX OPERATOR.
    /// @TODO Implement me correctly.
    std::size_t idx;
    MhdScriptRef ref;
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            idx = static_cast<size_t>((int) index);
            if (m_val_lgc->size() <= idx) {
                m_val_lgc->resize(idx + 1);
            }
            ref = std::move(MhdScriptRef(&(*m_val_lgc)[idx]));
            break;
        case MhdScriptVal::Type::INT:
            idx = static_cast<size_t>((int) index);
            if (m_val_int->size() <= idx) {
                m_val_int->resize(idx + 1);
            }
            ref = std::move(MhdScriptRef(&(*m_val_int)[idx]));
            break;
        case MhdScriptVal::Type::DBL:
            idx = static_cast<size_t>((int) index);
            if (m_val_dbl->size() <= idx) {
                m_val_dbl->resize(idx + 1);
            }
            ref = std::move(MhdScriptRef(&(*m_val_dbl)[idx]));
            break;
        case MhdScriptVal::Type::MAP:
            ref = MhdScriptRef(&(*m_val_map)[index]);
            break;
        default:
            throw MhdInvalidOp(*this);
    }
    return ref;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptRef
MhdScriptVal::operator[](const std::vector<MhdScriptVal>& args) const
{
    // Apply an INDEX operator.
    /// @TODO Implement me correctly.
    return (*this)[args[0]];
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal 
MhdScriptVal::operator()(const std::vector<MhdScriptVal>& args) const
{
    // Apply a CALL operator.
    if (m_type == MhdScriptVal::Type::FUN) {
        return (*m_val_fun)(args);
    } else {
        throw MhdInvalidOp(*this);
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename X, typename T, typename U>
typename std::enable_if<std::is_convertible<T, X>::value>::type 
operator_cast_apply(U& val,
                    const std::valarray<T>& lhs)
{
    // Apply a CAST operator for convertible value arrays. 
    std::valarray<X> rhs_cast(lhs.size());
    for (std::size_t i = 0; i < lhs.size(); ++i) {
        rhs_cast[i] = static_cast<X>(lhs[i]);
    }
    val = std::move(rhs_cast);
}
template<typename X, typename T, typename U>
void
operator_cast_apply(U& val,
                    const std::basic_string<T>& lhs)
{
    // Apply a CAST operator from string to value. 
    X rhs_cast{};
    std::basic_istringstream<T>(lhs) >> rhs_cast;
    val = std::move(rhs_cast);
}
template<typename X, typename T, typename U>
void
operator_cast_apply(U& val,
                    const T& lhs)
{
    // Apply a CAST operator for arbitrary objects. 
    X rhs_cast{std::move(*reinterpret_cast<const X*>(&lhs))};
    val = std::move(rhs_cast);
}
//--------------------------------------------------------------------------------------------------------
template<typename X, typename T, typename U>
void
operator_cast_apply_string(U& val,
                           const std::valarray<T>& lhs)
{
    // Apply a CAST TO STRING operator for value arrays. 
    std::basic_ostringstream<X> rhs_cast;
    rhs_cast << "[";
    if (lhs.size() > 0) {
        rhs_cast << lhs[0];
        for (size_t i = 1; i < lhs.size(); ++i) {
            rhs_cast << ", " << lhs[i];
        }
    }
    rhs_cast << "]";
    val = std::move(rhs_cast.str());
}
template<typename X, typename T, typename U>
void 
operator_cast_apply_string(U& val,
                           const T& lhs)
{
    // Apply a CAST TO STRING operator for arbitrary objects. 
    std::basic_ostringstream<X> rhs_cast;
    rhs_cast << lhs;
    val = std::move(rhs_cast.str());
}
//--------------------------------------------------------------------------------------------------------
template<typename T, typename U>
void
operator_cast_apply(MhdScriptVal::Type tp, U& val,
                    const T& lhs)
{
    // Apply a CAST operator for arbitrary objects.
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
            operator_cast_apply_string<char>(val, lhs);
            break;
        default:
            throw MhdInvalidOp(lhs);
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVal::operator_cast(MhdScriptVal::Type tp,
                            const MhdScriptVal& lhs)
{
    // Apply a CAST operator.
    if (lhs.m_type == tp) {
        return lhs;
    }
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
        case MhdScriptVal::Type::INT:
        case MhdScriptVal::Type::DBL:
        case MhdScriptVal::Type::STR:
        case MhdScriptVal::Type::PTR:
            break;
        default:
            throw MhdInvalidOp(lhs);
    }
    MhdScriptVal new_lhs = lhs;
    switch (lhs.m_type) {
        case MhdScriptVal::Type::LGC:
            operator_cast_apply(tp, new_lhs, *new_lhs.m_val_lgc);
            break;
        case MhdScriptVal::Type::INT:
            operator_cast_apply(tp, new_lhs, *new_lhs.m_val_int);
            break;
        case MhdScriptVal::Type::DBL:
            operator_cast_apply(tp, new_lhs, *new_lhs.m_val_dbl);
            break;
        case MhdScriptVal::Type::STR:
            operator_cast_apply(tp, new_lhs, *new_lhs.m_val_str);
            break;
        case MhdScriptVal::Type::PTR:
            operator_cast_apply(tp, new_lhs,  new_lhs.m_val_ptr);
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
    // Apply a CAST TO BOOL operator.
    bool val;
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            val = m_val_lgc->size() > 0 && bool((*m_val_lgc)[0]);
            break;
        case MhdScriptVal::Type::INT:
            val = m_val_int->size() > 0 && bool((*m_val_int)[0]);
            break;
        case MhdScriptVal::Type::DBL:
            val = m_val_dbl->size() > 0 && bool((*m_val_dbl)[0]);
            break;
        case MhdScriptVal::Type::STR:
            val = !m_val_str->empty();
            break;
        case MhdScriptVal::Type::PTR:
            val = m_val_ptr != nullptr;
            break;
        default:
            throw MhdInvalidOp(*this);
    }
    return val;
}
MHD_INTERFACE
MhdScriptVal::operator int() const 
{
    // Apply a CAST TO INT operator.
    int val{};
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            val = m_val_lgc->size() > 0 ? int((*m_val_lgc)[0]) : val;
            break;
        case MhdScriptVal::Type::INT:
            val = m_val_int->size() > 0 ? int((*m_val_int)[0]) : val;
            break;
        case MhdScriptVal::Type::DBL:
            val = m_val_dbl->size() > 0 ? int((*m_val_dbl)[0]) : val;
            break;
        default:
            throw MhdInvalidOp(*this);
    }
    return val;
}
MHD_INTERFACE
MhdScriptVal::operator double() const 
{
    // Apply a CAST TO DOUBLE operator.
    double val{};
    switch (m_type) {
        case MhdScriptVal::Type::LGC:
            val = m_val_lgc->size() > 0 ? double((*m_val_lgc)[0]) : val;
            break;
        case MhdScriptVal::Type::INT:
            val = m_val_int->size() > 0 ? double((*m_val_int)[0]) : val;
            break;
        case MhdScriptVal::Type::DBL:
            val = m_val_dbl->size() > 0 ? double((*m_val_dbl)[0]) : val;
            break;
        default:
            throw MhdInvalidOp(*this);
    }
    return val;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal::operator std::string() const 
{
    std::string val;
    if (m_type == MhdScriptVal::Type::STR) {
        val = *m_val_str;
    } else {
        val = *operator_cast(MhdScriptVal::Type::STR, *this).m_val_str;
    }
    return val;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################


