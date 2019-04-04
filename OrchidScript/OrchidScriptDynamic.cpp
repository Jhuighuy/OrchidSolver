// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptDynamic.hpp"
#include "OrchidScriptParserSyntax.hpp"

#include <string>
#include <vector>
#include <memory>
#include <valarray>

//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename = void>
struct MhdDynamicNilT;
template<typename = void>
struct MhdDynamicIntT;
template<typename = void>
struct MhdDynamicDblT;
template<typename = void>
struct MhdDynamicStrT;
template<typename = void>
struct MhdDynamicMapT;
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
ORCHID_INTERFACE
MhdDynamic::MhdDynamic(int value)
    : m_impl(new MhdDynamicIntT<>(value)) {}
//########################################################################################################
//########################################################################################################
//########################################################################################################
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
ORCHID_INTERFACE
MhdDynamic::MhdDynamic(double value)
    : m_impl(new MhdDynamicDblT<>(value)) {}
//########################################################################################################
//########################################################################################################
//########################################################################################################
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
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERFACE
MhdDynamic::MhdDynamic(const std::string& value)
    : m_impl(new MhdDynamicStrT<>(value)) {}
//########################################################################################################
//########################################################################################################
//########################################################################################################
template<typename T>
struct MhdDynamicFuncT<-1, T> final : public MhdDynamicT
{
public:
    std::function<MhdDynamic(const std::vector<MhdDynamic>&)> m_value;
public:
    explicit MhdDynamicFuncT(const std::function<MhdDynamic(const std::vector<MhdDynamic>&)>& value)
        : m_value(value) {}
public:
    MhdDynamicT* copy() const override
    { 
        return new MhdDynamicFuncT<-1, T>(m_value); 
    }
public:
    MhdDynamicT* operator()(const std::vector<MhdDynamicT*>& impls) const override
    { 
        std::vector<MhdDynamic> dyns;
        for (const MhdDynamicT* impl : impls) {
            dyns.push_back(MhdDynamic(impl->copy()));
        }
        return m_value(dyns).m_impl->copy();
    }
};  // struct MhdDynamicFuncT
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERFACE
MhdDynamic::MhdDynamic(const std::function<MhdDynamic(const std::vector<MhdDynamic>&)>& value)
    : m_impl(new MhdDynamicFuncT<-1>(value)) {}
//########################################################################################################
//########################################################################################################
//########################################################################################################
