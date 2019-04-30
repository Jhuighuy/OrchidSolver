// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"
#include "OrchidScriptValue.hpp"

#include <utility>
#include <vector>
#include <memory>

struct MhdRuntimeException {};

//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExpr : public std::enable_shared_from_this<MhdScriptExpr>
{
public:
    typedef std::shared_ptr<MhdScriptExpr> Ptr;
    typedef std::vector<Ptr> Vec;
    typedef std::vector<std::pair<Ptr, Ptr>> Map;
public:
    explicit MhdScriptExpr() noexcept {}
    virtual ~MhdScriptExpr() noexcept {}
public:
    virtual MhdScriptVal eval() const 
    { 
        throw MhdScriptInvalidOp();
    } 
    virtual MhdScriptRef eval_ref() const 
    { 
        throw MhdScriptInvalidOp();
    }
};  // struct MhdScriptExpr
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprEmpty : public MhdScriptExpr
{
public:
    MhdScriptExprEmpty() {}
};  // struct MhdScriptExprEmpty
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprConst : public MhdScriptExpr
{
public:
    MhdScriptVal m_value;
public:
    template<typename T>
    MhdScriptExprConst(const T& value) noexcept
        : m_value(MhdScriptVal(value)) {}
    MhdScriptExprConst(MhdScriptVal value) noexcept
        : m_value(value) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprConst
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprConstFunc final : public MhdScriptExpr
{
public:
    std::vector<std::string> m_args;
    MhdScriptExpr::Ptr m_body;
public:
    MhdScriptExprConstFunc(const std::vector<std::string>& args, 
                           MhdScriptExpr::Ptr body) noexcept
        : m_args(args)
        , m_body(body) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprConstFunc
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprConstArray final : public MhdScriptExpr
{
public:
    MhdScriptExpr::Vec m_array;
public:
    MhdScriptExprConstArray(const MhdScriptExpr::Vec& array)
        : m_array(array) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprConstArray
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprIdent final : public MhdScriptExpr
{
public:
    std::string m_id;
    bool m_let;
public:
    MhdScriptExprIdent(const std::string& id, bool let = false)
        : m_id(id), m_let(let) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
    MHD_INTERFACE
    MhdScriptRef eval_ref() const override final;
};  // struct MhdScriptExprIdent
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprCall final : public MhdScriptExpr
{
public:
    MhdScriptExpr::Ptr m_func;
    MhdScriptExpr::Vec m_args;
public:
    MhdScriptExprCall(MhdScriptExpr::Ptr func, const MhdScriptExpr::Vec& args) noexcept
        : m_func(func), m_args(args) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprCall
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprIndex final : public MhdScriptExpr
{
public:
    MhdScriptExpr::Ptr m_array;
    MhdScriptExpr::Vec m_index;
public:
    MhdScriptExprIndex(MhdScriptExpr::Ptr array, const MhdScriptExpr::Vec& index) noexcept
        : m_array(array), m_index(index) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
    MHD_INTERFACE
    MhdScriptRef eval_ref() const override final;
};  // struct MhdScriptExprIndex
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprUnary : public MhdScriptExpr
{
public:
    MhdScriptKind m_op;
    MhdScriptExpr::Ptr m_expr;
public:
    MhdScriptExprUnary(MhdScriptKind op, MhdScriptExpr::Ptr expr) noexcept
        : m_op(op), m_expr(expr) { }
};	// struct MhdScriptExprUnary
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprNot final : public MhdScriptExprUnary
{
public:
    template<typename... T>
    MhdScriptExprNot(T... t)
        : MhdScriptExprUnary(t...) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprNot
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprNegate final : public MhdScriptExprUnary
{
public:
    template<typename... T>
    MhdScriptExprNegate(T... t)
        : MhdScriptExprUnary(t...) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprNegate
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprBinary : public MhdScriptExpr
{
public:
    MhdScriptKind m_op;
    MhdScriptExpr::Ptr m_lhs;
    MhdScriptExpr::Ptr m_rhs;
public:
    MhdScriptExprBinary(MhdScriptKind op, 
                        MhdScriptExpr::Ptr lhs, MhdScriptExpr::Ptr rhs) noexcept
        : m_op(op)
        , m_lhs(lhs), m_rhs(rhs) { }
};	// struct MhdScriptExprBinary
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprAssignment final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprAssignment(T... t) noexcept
        : MhdScriptExprBinary(t...) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
    MHD_INTERFACE
    MhdScriptRef eval_ref() const override final;
};  // struct MhdScriptExprAssignment
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprLogical final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprLogical(T... t) noexcept
        : MhdScriptExprBinary(t...) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprLogical
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprBitwise final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprBitwise(T... t) noexcept
        : MhdScriptExprBinary(t...) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprBitwise
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprArithmetic final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprArithmetic(T... t) noexcept
        : MhdScriptExprBinary(t...) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprArithmetic
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprCompound final : public MhdScriptExpr
{
public:
    MhdScriptExpr::Vec m_exprs;
public:
    MhdScriptExprCompound(const MhdScriptExpr::Vec& exprs) noexcept
        : m_exprs(exprs) {}
    template<typename... T>
    MhdScriptExprCompound(T... exprs) noexcept
        : m_exprs({ exprs... }) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprCompound
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprNamespace final : public MhdScriptExpr
{
public:
    std::string m_id;
    MhdScriptExpr::Vec m_exprs;
public:
    MhdScriptExprNamespace(const std::string& id,
                           const MhdScriptExpr::Vec& exprs) noexcept
        : m_id(id)
        , m_exprs(exprs) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprNamespace
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprCond : public MhdScriptExpr
{
public:
    MhdScriptExprCond() {}
};  // struct MhdScriptExprCond
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprIf final : public MhdScriptExprCond
{
public:
    MhdScriptExpr::Ptr m_cond;
    MhdScriptExpr::Ptr m_then_branch;
    MhdScriptExpr::Ptr m_else_branch;
public:
    MhdScriptExprIf(MhdScriptExpr::Ptr cond, 
                    MhdScriptExpr::Ptr then_branch, MhdScriptExpr::Ptr else_branch) noexcept
        : m_cond(cond)
        , m_then_branch(then_branch), m_else_branch(else_branch) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprIf
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprSwitch final : public MhdScriptExprCond
{
public:
    MhdScriptExpr::Ptr m_cond;
    MhdScriptExpr::Map m_cases;
    MhdScriptExpr::Ptr m_case_default;
public:
    MhdScriptExprSwitch(MhdScriptExpr::Ptr cond, const MhdScriptExpr::Map& cases, 
                        MhdScriptExpr::Ptr case_default) noexcept
        : m_cond(cond)
        , m_cases(cases), m_case_default(case_default) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprSwitch
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprLoop : public MhdScriptExpr
{
public:
    MhdScriptExprLoop() {}
};  // struct MhdScriptExprLoop
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprWhile final : public MhdScriptExprLoop
{
public:
    MhdScriptExpr::Ptr m_cond;
    MhdScriptExpr::Ptr m_body;
public:
    MhdScriptExprWhile(MhdScriptExpr::Ptr cond, 
                       MhdScriptExpr::Ptr body)
        : m_cond(cond)
        , m_body(body) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override;
};  // struct MhdScriptExprWhile
struct MhdScriptExprDoWhile final : public MhdScriptExprLoop
{
public:
    MhdScriptExpr::Ptr m_cond;
    MhdScriptExpr::Ptr m_body;
public:
    MhdScriptExprDoWhile(MhdScriptExpr::Ptr cond, MhdScriptExpr::Ptr body)
        : m_cond(cond)
        , m_body(body) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprDoWhile
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprFor final : public MhdScriptExprLoop
{
public:
    MhdScriptExpr::Ptr m_init;
    MhdScriptExpr::Ptr m_cond;
    MhdScriptExpr::Ptr m_iter;
    MhdScriptExpr::Ptr m_body;
public:
    MhdScriptExprFor(MhdScriptExpr::Ptr init, MhdScriptExpr::Ptr cond, MhdScriptExpr::Ptr iter, 
                     MhdScriptExpr::Ptr body) 
        : m_init(init), m_cond(cond), m_iter(iter)
        , m_body(body) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprFor
struct MhdScriptExprForEach final : public MhdScriptExprLoop
{
public:
    std::string m_id;
    MhdScriptExpr::Ptr m_cont;
    MhdScriptExpr::Ptr m_body;
public:
    MhdScriptExprForEach(const std::string& id, MhdScriptExpr::Ptr cont, 
                         MhdScriptExpr::Ptr body) 
        : m_id(id), m_cont(cont)
        , m_body(body) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprForEach
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprTryCatch final : public MhdScriptExpr
{
public:
    MhdScriptExpr::Ptr m_try_block;
    MhdScriptExpr::Ptr m_catch_block;
    std::string m_catch_arg;
public:
    MhdScriptExprTryCatch(MhdScriptExpr::Ptr try_block, 
                          MhdScriptExpr::Ptr catch_block, const std::string& catch_arg)
        : m_try_block(try_block)
        , m_catch_block(catch_block), m_catch_arg(catch_arg) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprTryCatch
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprJump : public MhdScriptExpr
{
public:
    MhdScriptExprJump() {}
};  // struct MhdScriptExprLoop
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprBreak final : public MhdScriptExprJump
{
public:
    MhdScriptExpr::Ptr m_expr;
public:
    MhdScriptExprBreak(MhdScriptExpr::Ptr expr)
        : m_expr(expr) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprBreak
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprContinue final : public MhdScriptExprJump
{
public:
    MhdScriptExprContinue() {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprContinue
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprReturn final : public MhdScriptExprJump
{
public:
    MhdScriptExpr::Ptr m_expr;
public:
    MhdScriptExprReturn(MhdScriptExpr::Ptr expr)
        : m_expr(expr) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprReturn
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprThrow final : public MhdScriptExprJump
{
public:
    MhdScriptExpr::Ptr m_expr;
public:
    MhdScriptExprThrow(MhdScriptExpr::Ptr expr)
        : m_expr(expr) {}
public:
    MHD_INTERFACE
    MhdScriptVal eval() const override final;
};  // struct MhdScriptExprReturn
//########################################################################################################
//########################################################################################################
//########################################################################################################
