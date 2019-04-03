// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once
#include <string>
#include <vector>
#include <memory>
#include <functional>

struct MhdInvalidOp
{
};
struct MhdDynamic;
template<typename>
using MhdDynamicU = MhdDynamic;
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename = void>
struct MhdDynamicIntT;
template<typename = void>
struct MhdDynamicDblT;
template<typename = void>
struct MhdDynamicStrT;
template<int, typename = void>
struct MhdDynamicFuncT;
//--------------------------------------------------------------------------------------------------------
struct MhdDynamicT
{
public:
    virtual ~MhdDynamicT() {}
public:
    virtual MhdDynamicT* copy() const
    { 
        return new MhdDynamicT(); 
    }
public:
    virtual MhdDynamicT* operator+() const 
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator-() const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator+(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator-(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator*(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator/(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator%(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
public:
    virtual MhdDynamicT* operator!() const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator==(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator!=(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator<(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator<=(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator>(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator>=(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator&&(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator||(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
public:
    virtual MhdDynamicT* operator~() const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator|(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator&(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator^(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator<<(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator>>(const MhdDynamicT*) const
    { 
        throw MhdInvalidOp(); 
    }
public:
    virtual MhdDynamicT* operator()(const std::vector<MhdDynamicT*>&) const
    { 
        throw MhdInvalidOp(); 
    }
    virtual MhdDynamicT* operator[](const std::vector<MhdDynamicT*>&) const
    { 
        throw MhdInvalidOp(); 
    }
public:
    virtual operator int() const
    { 
        throw MhdInvalidOp(); 
    }
    virtual operator bool() const
    { 
        throw MhdInvalidOp(); 
    }
    virtual operator double() const
    { 
        throw MhdInvalidOp(); 
    }
    virtual operator std::string() const
    { 
        throw MhdInvalidOp(); 
    }
};  // struct MhdDynamicT
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
struct MhdDynamicIntT final : public MhdDynamicT
{
public:
    int m_value;
public:
    explicit MhdDynamicIntT(int value)
        : m_value(value) {}
public:
    MhdDynamicT* copy() const override
    { 
        return new MhdDynamicIntT<T>(m_value); 
    }
public:
    MhdDynamicT* operator+() const override
    { 
        return new MhdDynamicIntT<T>(+m_value);
    }
    MhdDynamicT* operator-() const override
    { 
        return new MhdDynamicIntT<T>(-m_value);
    }
    MhdDynamicT* operator+(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicDblT<T>(m_value + dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value + dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator-(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicDblT<T>(m_value - dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value - dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator*(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicDblT<T>(m_value * dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value * dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator/(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicDblT<T>(m_value / dyn_dbl->m_value);
        } else if (dyn_int != nullptr &&
                   dyn_int->m_value != 0) {
            return new MhdDynamicIntT<T>(m_value / dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator%(const MhdDynamicT* impl) const override
    { 
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_int != nullptr &&
            dyn_int->m_value != 0) {
            return new MhdDynamicIntT<T>(m_value % dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
public:
    MhdDynamicT* operator!() const override
    { 
        return new MhdDynamicIntT<T>(!m_value);
    }
    MhdDynamicT* operator==(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value == dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value == dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator!=(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value != dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value != dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator<(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value < dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value < dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator<=(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value <= dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value <= dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator>(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value > dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value > dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator>=(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value >= dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value >= dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
public:
    MhdDynamicT* operator~() const override
    { 
        return new MhdDynamicIntT(~m_value);
    }
    MhdDynamicT* operator|(const MhdDynamicT* impl) const override
    { 
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value | dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator^(const MhdDynamicT* impl) const override
    { 
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value ^ dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator&(const MhdDynamicT* impl) const override
    { 
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value & dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator<<(const MhdDynamicT* impl) const override
    { 
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value << dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator>>(const MhdDynamicT* impl) const override
    { 
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value >> dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
public:
    operator int() const override
    { 
        return m_value; 
    }
    operator bool() const override
    { 
        return m_value != 0; 
    }
    operator double() const override
    { 
        return static_cast<double>(m_value); 
    }
    operator std::string() const override
    { 
        return std::to_string(m_value); 
    }
};  // struct MhdDynamicIntT
//--------------------------------------------------------------------------------------------------------
template<typename T>
struct MhdDynamicDblT final : public MhdDynamicT
{
public:
    double m_value;
public:
    explicit MhdDynamicDblT(double value)
        : m_value(value) {}
public:
    MhdDynamicT* copy() const override
    { 
        return new MhdDynamicDblT<T>(m_value); 
    }
public:
    MhdDynamicT* operator+() const override
    { 
        return new MhdDynamicDblT<T>(+m_value);
    }
    MhdDynamicT* operator-() const override
    { 
        return new MhdDynamicDblT<T>(-m_value);
    }
    MhdDynamicT* operator+(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicDblT<T>(m_value + dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicDblT<T>(m_value + dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator-(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicDblT<T>(m_value - dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicDblT<T>(m_value - dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator*(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicDblT<T>(m_value * dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicDblT<T>(m_value * dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator/(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicDblT<T>(m_value / dyn_dbl->m_value);
        } else if (dyn_int != nullptr &&
                   dyn_int->m_value != 0) {
            return new MhdDynamicDblT<T>(m_value / dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
public:
    MhdDynamicT* operator!() const override
    { 
        return new MhdDynamicIntT<T>(!m_value);
    }
    MhdDynamicT* operator==(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value == dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value == dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator!=(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value != dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value != dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator<(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value < dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value < dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator<=(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value <= dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value <= dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator>(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value > dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value > dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator>=(const MhdDynamicT* impl) const override
    { 
        const auto dyn_dbl = dynamic_cast<const MhdDynamicDblT<T>*>(impl);
        const auto dyn_int = dynamic_cast<const MhdDynamicIntT<T>*>(impl);
        if (dyn_dbl != nullptr) {
            return new MhdDynamicIntT<T>(m_value >= dyn_dbl->m_value);
        } else if (dyn_int != nullptr) {
            return new MhdDynamicIntT<T>(m_value >= dyn_int->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
public:
    operator int() const override
    { 
        return static_cast<int>(m_value); 
    }
    operator bool() const override
    { 
        return m_value != 0.0; 
    }
    operator double() const override
    { 
        return m_value; 
    }
    operator std::string() const override
    { 
        return std::to_string(m_value); 
    }
};  // struct MhdDynamicDblT
//--------------------------------------------------------------------------------------------------------
template<typename T>
struct MhdDynamicStrT final : public MhdDynamicT
{
public:
    std::string m_value;
public:
    explicit MhdDynamicStrT(const std::string& value)
        : m_value(value) {}
public:
    MhdDynamicT* copy() const override
    { 
        return new MhdDynamicStrT<T>(m_value); 
    }
public:
    MhdDynamicT* operator+(const MhdDynamicT* impl) const override
    { 
        const auto dyn_str = dynamic_cast<const MhdDynamicStrT<T>*>(impl);
        if (dyn_str != nullptr) {
            return new MhdDynamicStrT<T>(m_value + dyn_str->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
public:
    MhdDynamicT* operator==(const MhdDynamicT* impl) const override
    { 
        const auto dyn_str = dynamic_cast<const MhdDynamicStrT<T>*>(impl);
        if (dyn_str != nullptr) {
            return new MhdDynamicIntT<T>(m_value == dyn_str->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator!=(const MhdDynamicT* impl) const override
    { 
        const auto dyn_str = dynamic_cast<const MhdDynamicStrT<T>*>(impl);
        if (dyn_str != nullptr) {
            return new MhdDynamicIntT<T>(m_value != dyn_str->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator<(const MhdDynamicT* impl) const override
    { 
        const auto dyn_str = dynamic_cast<const MhdDynamicStrT<T>*>(impl);
        if (dyn_str != nullptr) {
            return new MhdDynamicIntT<T>(m_value < dyn_str->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator<=(const MhdDynamicT* impl) const override
    { 
        const auto dyn_str = dynamic_cast<const MhdDynamicStrT<T>*>(impl);
        if (dyn_str != nullptr) {
            return new MhdDynamicIntT<T>(m_value <= dyn_str->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator>(const MhdDynamicT* impl) const override
    { 
        const auto dyn_str = dynamic_cast<const MhdDynamicStrT<T>*>(impl);
        if (dyn_str != nullptr) {
            return new MhdDynamicIntT<T>(m_value > dyn_str->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
    MhdDynamicT* operator>=(const MhdDynamicT* impl) const override
    { 
        const auto dyn_str = dynamic_cast<const MhdDynamicStrT<T>*>(impl);
        if (dyn_str != nullptr) {
            return new MhdDynamicIntT<T>(m_value >= dyn_str->m_value);
        } else {
            throw MhdInvalidOp(); 
        }
    }
public:
    operator std::string() const override
    { 
        return m_value; 
    }
};  // struct MhdDynamicStrT
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdDynamic
{
public:
    std::unique_ptr<MhdDynamicT> m_impl;
public:
    MhdDynamic(int value = 0)
        : m_impl(new MhdDynamicIntT<>(value)) {}
    MhdDynamic(double value)
        : m_impl(new MhdDynamicDblT<>(value)) {}
    MhdDynamic(const std::string& value)
        : m_impl(new MhdDynamicStrT<>(value)) {}
    template<typename... U>
    MhdDynamic(const std::function<void(U...)>& value)
        : m_impl(new MhdDynamicFuncT<sizeof...(U)>(
            [value](MhdDynamicU<U>... u)
            { 
                return value(u...), MhdDynamic();
            })) {}
    template<typename T, typename... U>
    MhdDynamic(const std::function<T(U...)>& value)
        : m_impl(new MhdDynamicFuncT<sizeof...(U)>(
            [value](MhdDynamicU<U>... u)
            { 
                return MhdDynamic(value(u...));
            })) {}
public:
    MhdDynamic(MhdDynamicT* impl)
        : m_impl(impl) {}
public:
    MhdDynamic(MhdDynamic&& dyn) noexcept
        : m_impl(std::forward<std::unique_ptr<MhdDynamicT>>(dyn.m_impl)) {}
    MhdDynamic(const MhdDynamic& dyn)
        : m_impl(dyn.m_impl != nullptr ? dyn.m_impl->copy() : nullptr) {}
    MhdDynamic& operator= (MhdDynamic&& dyn) noexcept
    {
        if (this != &dyn) {
            m_impl = std::move(dyn.m_impl);
        }
        return *this;
    }
    MhdDynamic& operator= (const MhdDynamic& dyn)
    {
        if (this != &dyn) {
            m_impl.reset(dyn.m_impl != nullptr ? dyn.m_impl->copy() : nullptr);
        }
        return *this;
    }
public:
    MhdDynamic operator+() const
    {
        return MhdDynamic(m_impl->operator+());
    }
    MhdDynamic operator-() const
    {
        return MhdDynamic(m_impl->operator-());
    }
    MhdDynamic operator+(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator+(dyn.m_impl.get()));
    }
    MhdDynamic operator-(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator-(dyn.m_impl.get()));
    }
    MhdDynamic operator*(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator*(dyn.m_impl.get()));
    }
    MhdDynamic operator/(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator/(dyn.m_impl.get()));
    }
    MhdDynamic operator%(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator%(dyn.m_impl.get()));
    }
    MhdDynamic& operator+=(const MhdDynamic& dyn)
    {
        return *this = *this + dyn;
    }
    MhdDynamic& operator-=(const MhdDynamic& dyn)
    {
        return *this = *this - dyn;
    }
    MhdDynamic& operator*=(const MhdDynamic& dyn)
    {
        return *this = *this * dyn;
    }
    MhdDynamic& operator/=(const MhdDynamic& dyn)
    {
        return *this = *this / dyn;
    }
    MhdDynamic& operator%=(const MhdDynamic& dyn)
    {
        return *this = *this % dyn;
    }
public:
    MhdDynamic operator!() const
    {
        return MhdDynamic(m_impl->operator!());
    }
    MhdDynamic operator==(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator==(dyn.m_impl.get()));
    }
    MhdDynamic operator!=(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator!=(dyn.m_impl.get()));
    }
    MhdDynamic operator<(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator<(dyn.m_impl.get()));
    }
    MhdDynamic operator<=(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator<=(dyn.m_impl.get()));
    }
    MhdDynamic operator>(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator>(dyn.m_impl.get()));
    }
    MhdDynamic operator>=(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator>=(dyn.m_impl.get()));
    }
    MhdDynamic operator||(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator||(dyn.m_impl.get()));
    }
    MhdDynamic operator&&(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator&&(dyn.m_impl.get()));
    }
public:
    MhdDynamic operator~() const
    {
        return MhdDynamic(m_impl->operator~());
    }
    MhdDynamic operator&(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator&(dyn.m_impl.get()));
    }
    MhdDynamic operator|(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator|(dyn.m_impl.get()));
    }
    MhdDynamic operator^(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator^(dyn.m_impl.get()));
    }
    MhdDynamic operator<<(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator<<(dyn.m_impl.get()));
    }
    MhdDynamic operator>>(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator>>(dyn.m_impl.get()));
    }
    MhdDynamic& operator&=(const MhdDynamic& dyn)
    {
        return *this = *this & dyn;
    }
    MhdDynamic& operator|=(const MhdDynamic& dyn)
    {
        return *this = *this | dyn;
    }
    MhdDynamic& operator^=(const MhdDynamic& dyn)
    {
        return *this = *this ^ dyn;
    }
    MhdDynamic& operator<<=(const MhdDynamic& dyn)
    {
        return *this = *this << dyn;
    }
    MhdDynamic& operator>>=(const MhdDynamic& dyn)
    {
        return *this = *this >> dyn;
    }
public:
    MhdDynamic operator()(const std::vector<MhdDynamic>& dyns) const
    {
        std::vector<MhdDynamicT*> dyn_impls;
        for (const MhdDynamic& dyn : dyns) {
            dyn_impls.push_back(dyn.m_impl.get());
        }
        return m_impl->operator()(dyn_impls);
    }
    MhdDynamic operator[](const std::vector<MhdDynamic>& dyns) const
    {
        std::vector<MhdDynamicT*> dyn_impls;
        for (const MhdDynamic& dyn : dyns) {
            dyn_impls.push_back(dyn.m_impl.get());
        }
        return m_impl->operator()(dyn_impls);
    }
public:
    operator int() const
    {
        return m_impl->operator int();
    }
    operator bool() const
    {
        return m_impl->operator bool();
    }
    operator double() const
    {
        return m_impl->operator double();
    }
    operator std::string() const
    {
        return m_impl->operator std::string();
    }
};  // struct MhdDynamic
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
struct MhdDynamicFuncT<0, T> final : public MhdDynamicT
{
public:
    std::function<MhdDynamic()> m_value;
public:
    explicit MhdDynamicFuncT(const std::function<MhdDynamic()>& value)
        : m_value(value) {}
public:
    MhdDynamicT* copy() const override
    { 
        return new MhdDynamicFuncT<0, T>(m_value); 
    }
public:
    MhdDynamicT* operator()(const std::vector<MhdDynamicT*>& impls) const override
    { 
        if (impls.size() == 0) {
            MhdDynamic ret = m_value();
            return ret.m_impl.release();
        } else {
            throw MhdInvalidOp(); 
        }
    }
};  // struct MhdDynamicFuncT
//--------------------------------------------------------------------------------------------------------
template<typename T>
struct MhdDynamicFuncT<1, T> final : public MhdDynamicT
{
public:
    std::function<MhdDynamic(MhdDynamic)> m_value;
public:
    explicit MhdDynamicFuncT(const std::function<MhdDynamic(MhdDynamic)>& value)
        : m_value(value) {}
public:
    MhdDynamicT* copy() const override
    { 
        return new MhdDynamicFuncT<1, T>(m_value); 
    }
public:
    MhdDynamicT* operator()(const std::vector<MhdDynamicT*>& impls) const override
    { 
        if (impls.size() == 1) {
            MhdDynamic ret = m_value(MhdDynamic(impls[0]->copy()));
            return ret.m_impl.release();
        } else {
            throw MhdInvalidOp(); 
        }
    }
};  // struct MhdDynamicFuncT
//--------------------------------------------------------------------------------------------------------
template<typename T>
struct MhdDynamicFuncT<2, T> final : public MhdDynamicT
{
public:
    std::function<MhdDynamic(MhdDynamic, MhdDynamic)> m_value;
public:
    explicit MhdDynamicFuncT(const std::function<MhdDynamic(MhdDynamic, MhdDynamic)>& value)
        : m_value(value) {}
public:
    MhdDynamicT* copy() const override
    { 
        return new MhdDynamicFuncT<2, T>(m_value); 
    }
public:
    MhdDynamicT* operator()(const std::vector<MhdDynamicT*>& impls) const override
    { 
        if (impls.size() == 2) {
            MhdDynamic ret = m_value(MhdDynamic(impls[0]->copy()),
                                     MhdDynamic(impls[1]->copy()));
            return ret.m_impl.release();
        } else {
            throw MhdInvalidOp(); 
        }
    }
};  // struct MhdDynamicFuncT
//########################################################################################################
//########################################################################################################
//########################################################################################################
