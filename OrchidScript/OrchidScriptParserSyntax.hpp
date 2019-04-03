// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScriptScanner.hpp"
#include "OrchidScriptDynamic.hpp"
//typedef int MhdDynamic;
#include <utility>
#include <memory>
#include <vector>
#include <map>

#include <cassert>
#define ORCHID_ASSERT assert
#define ORCHID_INTERNAL
#define ORCHID_INTERFACE

struct MhdRuntimeException {};

//########################################################################################################
//########################################################################################################
//########################################################################################################
enum MhdExprBaseType
{
    ORCHID_EXPR_TYPE_LGC,
    ORCHID_EXPR_TYPE_INT,
    ORCHID_EXPR_TYPE_DBL,
};	// enum MhdExprBaseType
typedef MhdExprBaseType MhdExprType;
extern std::map<std::string, MhdDynamic> g_vars;
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdJump 
{
public:
    virtual ~MhdJump() {}
};  // struct MhdJump
//--------------------------------------------------------------------------------------------------------
struct MhdJumpBreak final : public MhdJump
{ 
public:
    MhdDynamic m_val; 
public:
    MhdJumpBreak() : m_val() {}
    MhdJumpBreak(MhdDynamic val) : m_val(val) {}
};  // struct MhdJumpBreak
//--------------------------------------------------------------------------------------------------------
struct MhdJumpContinue final : public MhdJump 
{
};  // struct MhdJumpContinue
//--------------------------------------------------------------------------------------------------------
struct MhdJumpReturn final : public MhdJump
{ 
public:
    MhdDynamic m_val; 
public: 
    MhdJumpReturn() : m_val() {}
    MhdJumpReturn(MhdDynamic val) : m_val(val) {}
};  // struct MhdJumpReturn
//--------------------------------------------------------------------------------------------------------
struct MhdJumpThrow final : public MhdJump
{ 
public:
    MhdDynamic m_val; 
public: 
    MhdJumpThrow() : m_val() {}
    MhdJumpThrow(MhdDynamic val) : m_val(val) {}
};  // struct MhdJumpThrow
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExpr : std::enable_shared_from_this<MhdExpr>
{
public:
    typedef std::shared_ptr<MhdExpr> Ptr;
    typedef std::vector<Ptr> Vec;
    typedef std::vector<std::pair<Ptr, Ptr>> Map;
    MhdExprType m_type;
public:
    virtual ~MhdExpr() {}
    MhdExpr(...): m_type(ORCHID_EXPR_TYPE_LGC) {}
public:
    virtual MhdDynamic updt(MhdDynamic val) const { abort(); }
    virtual MhdDynamic eval() const { abort(); } 
};  // struct MhdExpr
//--------------------------------------------------------------------------------------------------------
struct MhdExprEmpty : public MhdExpr
{
public:
    MhdExprEmpty() {}
};  // struct MhdExprEmpty
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExprConst : public MhdExpr
{
public:
    MhdDynamic m_value;
public:
    MhdExprConst(MhdDynamic value)
        : m_value(value) {}
public:
    MhdDynamic eval() const override
    {
        return m_value;
    }
};  // struct MhdExprConst
//--------------------------------------------------------------------------------------------------------
struct MhdExprIdent : public MhdExpr
{
public:
    std::string m_id;
public:
    MhdExprIdent(const std::string& id)
        : m_id(id) {}
public:
    MhdDynamic updt(MhdDynamic val) const override
    {
        return g_vars[m_id] = val;
    }
    MhdDynamic eval() const override
    {
        return g_vars[m_id];
    }
};  // struct MhdExprIdent
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExprFactorCall final : public MhdExpr
{
public:
    MhdExpr::Ptr m_func;
    MhdExpr::Vec m_args;
public:
    MhdExprFactorCall(MhdExpr::Ptr func, const MhdExpr::Vec& args)
        : m_func(func), m_args(args) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic func = m_func->eval();
        std::vector<MhdDynamic> args;
        for (MhdExpr::Ptr arg : m_args) {
            args.push_back(arg->eval());
        }
        try {
            return func(args);
        } catch (const MhdJumpReturn& return_jump) {
            return return_jump.m_val;
        }
    }
};  // struct MhdExprFactorCall
//--------------------------------------------------------------------------------------------------------
struct MhdExprFactorIndex final : public MhdExpr
{
public:
    MhdExpr::Ptr m_array;
    MhdExpr::Vec m_index;
public:
    MhdExprFactorIndex(MhdExpr::Ptr array, const MhdExpr::Vec& index)
        : m_array(array), m_index(index) {}
public:
    MhdDynamic updt(MhdDynamic val) const override
    {
        // @todo Implement me.
        abort();
    }
    MhdDynamic eval() const override
    {
        const MhdDynamic array = m_array->eval();
        std::vector<MhdDynamic> index;
        for (MhdExpr::Ptr arg : m_index) {
            index.push_back(arg->eval());
        }
        return array[index];
    }
};  // struct MhdExprFactorIndex
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExprUnary : public MhdExpr
{
public:
    MhdToken::Kind m_op;
    MhdExpr::Ptr m_expr;
public:
    MhdExprUnary(MhdToken::Kind op, MhdExpr::Ptr expr)
        : m_op(op), m_expr(expr) { }
};	// struct MhdExprUnary
//--------------------------------------------------------------------------------------------------------
struct MhdExprUnaryNot final : public MhdExprUnary
{
public:
    template<typename... T>
    MhdExprUnaryNot(T... t)
        : MhdExprUnary(t...) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic expr = m_expr->eval();
        switch (m_op) {
            case MhdToken::Kind::OP_NOT:    return !expr;
            case MhdToken::Kind::OP_NOT_BW: return ~expr;
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdExprUnaryNot
//--------------------------------------------------------------------------------------------------------
struct MhdExprUnaryNegate final : public MhdExprUnary
{
public:
    template<typename... T>
    MhdExprUnaryNegate(T... t)
        : MhdExprUnary(t...) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic expr = m_expr->eval();
        switch (m_op) {
            case MhdToken::Kind::OP_ADD: return +expr;
            case MhdToken::Kind::OP_SUB: return -expr;
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdExprUnaryNegate
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExprBinary : public MhdExpr
{
public:
    MhdToken::Kind m_op;
    MhdExpr::Ptr m_lhs;
    MhdExpr::Ptr m_rhs;
public:
    MhdExprBinary(MhdToken::Kind op, MhdExpr::Ptr lhs, MhdExpr::Ptr rhs)
        : m_op(op), m_lhs(lhs), m_rhs(rhs) { }
};	// struct MhdExprBinary
//--------------------------------------------------------------------------------------------------------
struct MhdExprBinaryAssignment final : public MhdExprBinary
{
public:
    template<typename... T>
    MhdExprBinaryAssignment(T... t)
        : MhdExprBinary(t...) {}
    MhdDynamic eval() const override
    {
        const MhdDynamic lhs = m_lhs->eval();
        const MhdDynamic rhs = m_rhs->eval();
        switch (m_op) {
            case MhdToken::Kind::OP_ASG:        return m_lhs->updt(rhs);
            case MhdToken::Kind::OP_OR_BW_ASG:  return m_lhs->updt(lhs | rhs);
            case MhdToken::Kind::OP_XOR_BW_ASG: return m_lhs->updt(lhs ^ rhs);
            case MhdToken::Kind::OP_AND_BW_ASG: return m_lhs->updt(rhs & rhs);
            case MhdToken::Kind::OP_LSHIFT_ASG: return m_lhs->updt(rhs << rhs);
            case MhdToken::Kind::OP_RSHIFT_ASG: return m_lhs->updt(rhs >> rhs);
            case MhdToken::Kind::OP_ADD_ASG: return m_lhs->updt(lhs + rhs);
            case MhdToken::Kind::OP_SUB_ASG: return m_lhs->updt(lhs - rhs);
            case MhdToken::Kind::OP_MUL_ASG: return m_lhs->updt(lhs * rhs);
            case MhdToken::Kind::OP_DIV_ASG: return m_lhs->updt(lhs / rhs);
            case MhdToken::Kind::OP_MOD_ASG: return m_lhs->updt(lhs % rhs);
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdExprBinaryAssignment
//--------------------------------------------------------------------------------------------------------
struct MhdExprBinaryLogical final : public MhdExprBinary
{
public:
    template<typename... T>
    MhdExprBinaryLogical(T... t)
        : MhdExprBinary(t...) {}
public:
    MhdDynamic eval() const override
    {
        if (m_op == MhdToken::Kind::OP_AND) {
            if (m_lhs->eval()) {
                return m_rhs->eval();
            } else {
                return MhdDynamic(false);
            }
        } else if (m_op == MhdToken::Kind::OP_OR) {
            if (m_lhs->eval()) {
                return MhdDynamic(true);
            } else {
                return m_rhs->eval();
            }
        } else {
            const MhdDynamic lhs = m_lhs->eval();
            const MhdDynamic rhs = m_rhs->eval();
            switch (m_op) {
                case MhdToken::Kind::OP_EQ:  return lhs == rhs;
                case MhdToken::Kind::OP_NEQ: return lhs != rhs;
                case MhdToken::Kind::OP_LT:  return lhs <  rhs;
                case MhdToken::Kind::OP_LTE: return lhs <= rhs;
                case MhdToken::Kind::OP_GT:  return lhs >  rhs;
                case MhdToken::Kind::OP_GTE: return lhs >= rhs;
                default: 
                    ORCHID_ASSERT(0); 
                    return MhdDynamic();
            }
        }
    }
};  // struct MhdExprBinaryLogical
//--------------------------------------------------------------------------------------------------------
struct MhdExprBinaryBitwise final : public MhdExprBinary
{
public:
    template<typename... T>
    MhdExprBinaryBitwise(T... t)
        : MhdExprBinary(t...) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic lhs = m_lhs->eval();
        const MhdDynamic rhs = m_rhs->eval();
        switch (m_op) {
            case MhdToken::Kind::OP_OR_BW:  return lhs | rhs;
            case MhdToken::Kind::OP_XOR_BW: return lhs ^ rhs;
            case MhdToken::Kind::OP_AND_BW: return lhs & rhs;
            case MhdToken::Kind::OP_LSHIFT: return lhs << rhs;
            case MhdToken::Kind::OP_RSHIFT: return lhs >> rhs;
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdExprBinaryBitwise
//--------------------------------------------------------------------------------------------------------
struct MhdExprBinaryArithmetic final : public MhdExprBinary
{
public:
    template<typename... T>
    MhdExprBinaryArithmetic(T... t)
        : MhdExprBinary(t...) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic lhs = m_lhs->eval();
        const MhdDynamic rhs = m_rhs->eval();
        switch (m_op) {
            case MhdToken::Kind::OP_ADD: return lhs + rhs;
            case MhdToken::Kind::OP_SUB: return lhs - rhs;
            case MhdToken::Kind::OP_MUL: return lhs * rhs;
            case MhdToken::Kind::OP_DIV: return lhs / rhs;
            case MhdToken::Kind::OP_MOD: return lhs % rhs;
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdExprBinaryArithmetic
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExprCompound final : public MhdExpr
{
public:
    MhdExpr::Vec m_exprs;
public:
    MhdExprCompound(const MhdExpr::Vec& exprs)
        : m_exprs(exprs) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        for (const MhdExpr::Ptr& comp_expr : m_exprs) {
            expr = comp_expr->eval();
        }
        return expr;
    }
};  // struct MhdExprCompound
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExprCond : public MhdExpr
{
public:
    MhdExprCond() {}
};  // struct MhdExprCond
//--------------------------------------------------------------------------------------------------------
struct MhdExprCondIf final : public MhdExprCond
{
public:
    MhdExpr::Ptr m_cond;
    MhdExpr::Ptr m_then_branch;
    MhdExpr::Ptr m_else_branch;
public:
    MhdExprCondIf(MhdExpr::Ptr cond, MhdExpr::Ptr then_branch, MhdExpr::Ptr else_branch)
        : m_cond(cond)
        , m_then_branch(then_branch), m_else_branch(else_branch) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        const MhdDynamic cond = m_cond->eval();
        if (cond) {
            expr = m_then_branch->eval();
        } else {
            if (m_else_branch != nullptr) {
                expr = m_else_branch->eval();
            }
        }
        return expr;
    }
};  // struct MhdExprCondIf
//--------------------------------------------------------------------------------------------------------
struct MhdExprCondSwitch final : public MhdExprCond
{
public:
    MhdExpr::Ptr m_cond;
    MhdExpr::Map m_cases;
    MhdExpr::Ptr m_case_default;
public:
    MhdExprCondSwitch(MhdExpr::Ptr cond, const MhdExpr::Map& cases, MhdExpr::Ptr case_default)
        : m_cond(cond)
        , m_cases(cases), m_case_default(case_default) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        const MhdDynamic cond = m_cond->eval();
        try {
            for (const std::pair<MhdExpr::Ptr, MhdExpr::Ptr>& case_expr : m_cases) {
                const MhdExpr::Ptr& case_value = case_expr.first;
                const MhdExpr::Ptr& case_branch = case_expr.second;
                if (case_value != nullptr &&
                    case_value->eval() == cond) {
                    expr = case_branch->eval();
                    break;
                }
            }
            if (m_case_default != nullptr) {
                expr = m_case_default->eval();
            }
        } catch (const MhdJumpBreak& break_jump) {
            expr = break_jump.m_val;
        }
        return expr;
    }
};  // struct MhdExprCondSwitch
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExprLoop : public MhdExpr
{
public:
    MhdExprLoop() {}
};  // struct MhdExprLoop
//--------------------------------------------------------------------------------------------------------
struct MhdExprLoopWhile final : public MhdExprLoop
{
public:
    MhdExpr::Ptr m_cond;
    MhdExpr::Ptr m_body;
public:
    MhdExprLoopWhile(MhdExpr::Ptr cond, MhdExpr::Ptr body)
        : m_cond(cond)
        , m_body(body) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        try {
            while (m_cond->eval()) {
                try {
                    expr = m_body->eval();
                } catch (const MhdJumpContinue&) {}
            }
        } catch (const MhdJumpBreak& break_jump) {
            expr = break_jump.m_val;
        }
        return expr;
    }
};  // struct MhdExprLoopWhile
//--------------------------------------------------------------------------------------------------------
struct MhdExprLoopDoWhile final : public MhdExprLoop
{
public:
    MhdExpr::Ptr m_cond;
    MhdExpr::Ptr m_body;
public:
    MhdExprLoopDoWhile(MhdExpr::Ptr cond, MhdExpr::Ptr body)
        : m_cond(cond)
        , m_body(body) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        try {
            do {
                try {
                    expr = m_body->eval();
                } catch (const MhdJumpContinue&) {}
            } while (m_cond->eval());
        } catch (const MhdJumpBreak& break_jump) {
            expr = break_jump.m_val;
        }
        return expr;
    }
};  // struct MhdExprLoopDoWhile
//--------------------------------------------------------------------------------------------------------
struct MhdExprLoopFor final : public MhdExprLoop
{
public:
    MhdExpr::Ptr m_init;
    MhdExpr::Ptr m_cond;
    MhdExpr::Ptr m_iter;
    MhdExpr::Ptr m_body;
public:
    MhdExprLoopFor(MhdExpr::Ptr init, MhdExpr::Ptr cond, MhdExpr::Ptr iter, MhdExpr::Ptr body) 
        : m_init(init), m_cond(cond), m_iter(iter)
        , m_body(body) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        try {
            for (m_init == nullptr || m_init->eval(); 
                 m_cond == nullptr || m_cond->eval(); 
                 m_iter == nullptr || m_iter->eval()) {
                try {
                    expr = m_body->eval();
                } catch (const MhdJumpContinue&) {}
            }
        } catch (const MhdJumpBreak& break_jump) {
            expr = break_jump.m_val;
        }
        return expr;
    }
};  // struct MhdExprLoopFor
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExprTryCatch : public MhdExpr
{
public:
    MhdExpr::Ptr m_try_block;
    MhdExpr::Ptr m_catch_block;
public:
    MhdExprTryCatch(MhdExpr::Ptr try_block, MhdExpr::Ptr catch_block)
        : m_try_block(try_block)
        , m_catch_block(catch_block) {}
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        try {
            expr = m_try_block->eval();
        } catch (const MhdJumpThrow& throw_jump) {
            expr = throw_jump.m_val;
        }
        return expr;
    }
};  // struct MhdExprTryCatch
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdExprJump : public MhdExpr
{
public:
    MhdExprJump() {}
};  // struct MhdExprLoop
//--------------------------------------------------------------------------------------------------------
struct MhdExprJumpBreak final : public MhdExprJump
{
public:
    MhdExpr::Ptr m_expr;
public:
    MhdExprJumpBreak(MhdExpr::Ptr expr)
        : m_expr(expr) {}
public:
    MhdDynamic eval() const override
    {
        if (m_expr != nullptr) {
            throw MhdJumpBreak(m_expr->eval());
        } else {
            throw MhdJumpBreak();
        }
    }
};  // struct MhdExprJumpBreak
//--------------------------------------------------------------------------------------------------------
struct MhdExprJumpContinue final : public MhdExprJump
{
public:
    MhdExprJumpContinue() {}
public:
    MhdDynamic eval() const override
    {
        throw MhdJumpContinue();
    }
};  // struct MhdExprJumpContinue
//--------------------------------------------------------------------------------------------------------
struct MhdExprJumpReturn final : public MhdExprJump
{
public:
    MhdExpr::Ptr m_expr;
public:
    MhdExprJumpReturn(MhdExpr::Ptr expr)
        : m_expr(expr) {}
public:
    MhdDynamic eval() const override
    {
        if (m_expr != nullptr) {
            throw MhdJumpReturn(m_expr->eval());
        } else {
            throw MhdJumpReturn();
        }
    }
};  // struct MhdExprJumpReturn
//--------------------------------------------------------------------------------------------------------
struct MhdExprJumpThrow final : public MhdExprJump
{
public:
    MhdExpr::Ptr m_expr;
public:
    MhdExprJumpThrow(MhdExpr::Ptr expr)
        : m_expr(expr) {}
public:
    MhdDynamic eval() const override
    {
        if (m_expr != nullptr) {
            throw MhdJumpThrow(m_expr->eval());
        } else {
            throw MhdJumpThrow();
        }
    }
};  // struct MhdExprJumpReturn
//########################################################################################################
//########################################################################################################
//########################################################################################################


