// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once
#include <string>
#include <vector>
#include <memory>

//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdDynamicImpl
{
public:
    virtual ~MhdDynamicImpl() {}
    virtual MhdDynamicImpl* copy() { return new MhdDynamicImpl(); }
public:
    virtual MhdDynamicImpl* operator+ () const { return nullptr; }
    virtual MhdDynamicImpl* operator+ (const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator- () const { return nullptr; }
    virtual MhdDynamicImpl* operator- (const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator* (const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator/ (const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator% (const MhdDynamicImpl*) const { return nullptr; }
public:
    virtual MhdDynamicImpl* operator! () const { return nullptr; }
    virtual MhdDynamicImpl* operator==(const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator!=(const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator< (const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator<=(const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator> (const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator>=(const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator& (const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator&&(const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator| (const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator||(const MhdDynamicImpl*) const { return nullptr; }
    virtual MhdDynamicImpl* operator^ (const MhdDynamicImpl*) const { return nullptr; }
public:
    virtual MhdDynamicImpl* operator()(const std::vector<MhdDynamicImpl*>&) const { return nullptr; }
    virtual MhdDynamicImpl* operator[](const std::vector<MhdDynamicImpl*>&) const { return nullptr; }
public:
    virtual operator int        () const { return int(); }
    virtual operator bool       () const { return bool(); }
    virtual operator double     () const { return double(); }
    virtual operator std::string() const { return std::string(); }
};  // struct MhdDynamicImpl
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdDynamic
{
    struct InvalidOp {};
private:
    std::unique_ptr<MhdDynamicImpl> m_impl;
public:
    MhdDynamic(MhdDynamicImpl* impl = nullptr)
        : m_impl(impl) {}
    MhdDynamic(const MhdDynamic& dyn)
        : m_impl(dyn.m_impl != nullptr ? dyn.m_impl->copy() : nullptr) {}
public:
    MhdDynamic& operator= (const MhdDynamic& dyn)
    {
        if (this != &dyn) {
            m_impl.reset(dyn.m_impl != nullptr ? dyn.m_impl->copy() : nullptr);
        }
        return *this;
    }
public:
    MhdDynamic  operator+ () const
    {
        return MhdDynamic(m_impl->operator+());
    }
    MhdDynamic  operator+ (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator+(dyn.m_impl.get()));
    }
    MhdDynamic& operator+=(const MhdDynamic& dyn)
    {
        return *this = *this + dyn;
    }
    MhdDynamic  operator- () const
    {
        return MhdDynamic(m_impl->operator-());
    }
    MhdDynamic  operator- (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator-(dyn.m_impl.get()));
    }
    MhdDynamic& operator-=(const MhdDynamic& dyn)
    {
        return *this = *this - dyn;
    }
    MhdDynamic  operator* (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator*(dyn.m_impl.get()));
    }
    MhdDynamic& operator*=(const MhdDynamic& dyn)
    {
        return *this = *this * dyn;
    }
    MhdDynamic  operator/ (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator/(dyn.m_impl.get()));
    }
    MhdDynamic& operator/=(const MhdDynamic& dyn)
    {
        return *this = *this / dyn;
    }
    MhdDynamic  operator% (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator%(dyn.m_impl.get()));
    }
    MhdDynamic& operator%=(const MhdDynamic& dyn)
    {
        return *this = *this % dyn;
    }
public:
    MhdDynamic  operator! () const
    {
        return MhdDynamic(m_impl->operator!());
    }
    MhdDynamic  operator==(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator==(dyn.m_impl.get()));
    }
    MhdDynamic  operator!=(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator!=(dyn.m_impl.get()));
    }
    MhdDynamic  operator< (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator<(dyn.m_impl.get()));
    }
    MhdDynamic  operator<=(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator<=(dyn.m_impl.get()));
    }
    MhdDynamic  operator> (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator>(dyn.m_impl.get()));
    }
    MhdDynamic  operator>=(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator>=(dyn.m_impl.get()));
    }
    MhdDynamic  operator& (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator&(dyn.m_impl.get()));
    }
    MhdDynamic& operator&=(const MhdDynamic& dyn)
    {
        return *this = *this & dyn;
    }
    MhdDynamic  operator&&(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator&&(dyn.m_impl.get()));
    }
    MhdDynamic  operator| (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator|(dyn.m_impl.get()));
    }
    MhdDynamic& operator|=(const MhdDynamic& dyn)
    {
        return *this = *this | dyn;
    }
    MhdDynamic  operator||(const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator||(dyn.m_impl.get()));
    }
    MhdDynamic  operator^ (const MhdDynamic& dyn) const
    {
        return MhdDynamic(m_impl->operator^(dyn.m_impl.get()));
    }
    MhdDynamic& operator^=(const MhdDynamic& dyn)
    {
        return *this = *this % dyn;
    }
public:
    MhdDynamic operator()(const std::vector<MhdDynamic>& dyns) const
    {
        std::vector<MhdDynamicImpl*> dyn_impls;
        for (const MhdDynamic& dyn : dyns) {
            dyn_impls.push_back(dyn.m_impl.get());
        }
        return m_impl->operator()(dyn_impls);
    }
    MhdDynamic operator[](const std::vector<MhdDynamic>& dyns) const
    {
        std::vector<MhdDynamicImpl*> dyn_impls;
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
