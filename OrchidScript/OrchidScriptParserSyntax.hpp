// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScriptScanner.hpp"
#include "OrchidScriptDynamic.hpp"

#include <memory>
#include <vector>
#include <set>
#include <map>

#include <cassert>
#define ORCHID_ASSERT assert
#define MHD_INTERNAL
#define MhdScriptVal MhdDynamic
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
//########################################################################################################
//########################################################################################################
//########################################################################################################
extern std::map<std::string, MhdDynamic> g_vars;
struct MhdVariableScope 
{
private:
    std::map<std::string, MhdDynamic> m_vars;
public:
    MhdVariableScope()
    {
        m_vars = g_vars;
    }
    ~MhdVariableScope()
    {
        std::map<std::string, MhdDynamic> vars;
        for (const auto& var : g_vars) {
            vars[var.first] = m_vars[var.first];
        }
        g_vars = vars;
    }
};  // struct MhdVariableScope
struct MhdVariable 
{
public:
    MhdVariable(const std::string& name, const MhdDynamic& value)
    {
        g_vars[name] = value;
    }
};  // struct MhdVariable
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
struct MhdScriptExpr : std::enable_shared_from_this<MhdScriptExpr>
{
public:
    typedef std::shared_ptr<MhdScriptExpr> Ptr;
    typedef std::vector<Ptr> Vec;
    typedef std::vector<std::pair<Ptr, Ptr>> Map;
public:
    explicit MhdScriptExpr() {}
    virtual ~MhdScriptExpr() {}
public:
    virtual MhdDynamic updt(MhdDynamic val) const { abort(); }
    virtual MhdDynamic eval() const { abort(); } 
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
    MhdDynamic m_value;
public:
    MhdScriptExprConst(MhdDynamic value)
        : m_value(value) {}
public:
    MhdDynamic eval() const override
    {
        return m_value;
    }
};  // struct MhdScriptExprConst
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprConstFunc : public MhdScriptExpr
{
public:
    std::vector<std::string> m_args;
    MhdScriptExpr::Ptr m_body;
public:
    MhdScriptExprConstFunc(const std::vector<std::string>& args, MhdScriptExpr::Ptr body)
        : m_args(args)
        , m_body(body) {}
public:
    MhdDynamic eval() const override
    {
        const std::function<MhdDynamic(const std::vector<MhdDynamic>&)> func =
            [args_name=m_args, body=m_body](const std::vector<MhdDynamic>& args) {
                MhdVariableScope scope{};
                if (args.size() != args_name.size()) {
                    throw MhdInvalidOp(); 
                }
                for (std::size_t i = 0; i < args.size(); ++i) {
                    MhdVariable var(args_name[i], args[i]);
                }
                return body->eval();
            };
        return MhdDynamic(func);
    }
};  // struct MhdScriptExprConstFunc
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprIdent : public MhdScriptExpr
{
public:
    std::string m_id;
public:
    MhdScriptExprIdent(const std::string& id)
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
    MhdScriptExprCall(MhdScriptExpr::Ptr func, const MhdScriptExpr::Vec& args)
        : m_func(func), m_args(args) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic func = m_func->eval();
        std::vector<MhdDynamic> args;
        for (MhdScriptExpr::Ptr arg : m_args) {
            args.push_back(arg->eval());
        }
        try {
            return func(args);
        } catch (const MhdJumpReturn& return_jump) {
            return return_jump.m_val;
        }
    }
};  // struct MhdScriptExprCall
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprIndex final : public MhdScriptExpr
{
public:
    MhdScriptExpr::Ptr m_array;
    MhdScriptExpr::Vec m_index;
public:
    MhdScriptExprIndex(MhdScriptExpr::Ptr array, const MhdScriptExpr::Vec& index)
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
        for (MhdScriptExpr::Ptr arg : m_index) {
            index.push_back(arg->eval());
        }
        return array[index];
    }
};  // struct MhdScriptExprIndex
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprUnary : public MhdScriptExpr
{
public:
    MhdScriptToken::Kind m_op;
    MhdScriptExpr::Ptr m_expr;
public:
    MhdScriptExprUnary(MhdScriptToken::Kind op, MhdScriptExpr::Ptr expr)
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
    MhdDynamic eval() const override
    {
        const MhdDynamic expr = m_expr->eval();
        switch (m_op) {
            case MhdScriptToken::Kind::OP_NOT:    return !expr;
            case MhdScriptToken::Kind::OP_NOT_BW: return ~expr;
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdScriptExprNot
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprNegate final : public MhdScriptExprUnary
{
public:
    template<typename... T>
    MhdScriptExprNegate(T... t)
        : MhdScriptExprUnary(t...) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic expr = m_expr->eval();
        switch (m_op) {
            case MhdScriptToken::Kind::OP_ADD: 
                return +expr;
            case MhdScriptToken::Kind::OP_SUB: 
                return -expr;
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdScriptExprNegate
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprBinary : public MhdScriptExpr
{
public:
    MhdScriptToken::Kind m_op;
    MhdScriptExpr::Ptr m_lhs;
    MhdScriptExpr::Ptr m_rhs;
public:
    MhdScriptExprBinary(MhdScriptToken::Kind op, MhdScriptExpr::Ptr lhs, MhdScriptExpr::Ptr rhs)
        : m_op(op)
        , m_lhs(lhs), m_rhs(rhs) { }
};	// struct MhdScriptExprBinary
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprAssignment final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprAssignment(T... t)
        : MhdScriptExprBinary(t...) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic lhs = m_lhs->eval();
        const MhdDynamic rhs = m_rhs->eval();
        switch (m_op) {
            case MhdScriptToken::Kind::OP_ASG:        
                return m_lhs->updt(rhs);
            case MhdScriptToken::Kind::OP_OR_BW_ASG:  
                return m_lhs->updt(lhs | rhs);
            case MhdScriptToken::Kind::OP_XOR_BW_ASG: 
                return m_lhs->updt(lhs ^ rhs);
            case MhdScriptToken::Kind::OP_AND_BW_ASG: 
                return m_lhs->updt(rhs & rhs);
            case MhdScriptToken::Kind::OP_LSHIFT_ASG: 
                return m_lhs->updt(rhs << rhs);
            case MhdScriptToken::Kind::OP_RSHIFT_ASG: 
                return m_lhs->updt(rhs >> rhs);
            case MhdScriptToken::Kind::OP_ADD_ASG: 
                return m_lhs->updt(lhs + rhs);
            case MhdScriptToken::Kind::OP_SUB_ASG: 
                return m_lhs->updt(lhs - rhs);
            case MhdScriptToken::Kind::OP_MUL_ASG: 
                return m_lhs->updt(lhs * rhs);
            case MhdScriptToken::Kind::OP_DIV_ASG: 
                return m_lhs->updt(lhs / rhs);
            case MhdScriptToken::Kind::OP_MOD_ASG: 
                return m_lhs->updt(lhs % rhs);
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdScriptExprAssignment
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprLogical final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprLogical(T... t)
        : MhdScriptExprBinary(t...) {}
public:
    MhdDynamic eval() const override
    {
        if (m_op == MhdScriptToken::Kind::OP_AND) {
            if (m_lhs->eval()) {
                return m_rhs->eval();
            } else {
                return MhdDynamic(false);
            }
        } else if (m_op == MhdScriptToken::Kind::OP_OR) {
            if (m_lhs->eval()) {
                return MhdDynamic(true);
            } else {
                return m_rhs->eval();
            }
        } else {
            const MhdDynamic lhs = m_lhs->eval();
            const MhdDynamic rhs = m_rhs->eval();
            switch (m_op) {
                case MhdScriptToken::Kind::OP_EQ:  
                    return lhs == rhs;
                case MhdScriptToken::Kind::OP_NEQ: 
                    return lhs != rhs;
                case MhdScriptToken::Kind::OP_LT:  
                    return lhs <  rhs;
                case MhdScriptToken::Kind::OP_LTE: 
                    return lhs <= rhs;
                case MhdScriptToken::Kind::OP_GT:  
                    return lhs >  rhs;
                case MhdScriptToken::Kind::OP_GTE: 
                    return lhs >= rhs;
                default: 
                    ORCHID_ASSERT(0); 
                    return MhdDynamic();
            }
        }
    }
};  // struct MhdScriptExprLogical
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprBitwise final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprBitwise(T... t)
        : MhdScriptExprBinary(t...) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic lhs = m_lhs->eval();
        const MhdDynamic rhs = m_rhs->eval();
        switch (m_op) {
            case MhdScriptToken::Kind::OP_OR_BW:  
                return lhs | rhs;
            case MhdScriptToken::Kind::OP_XOR_BW: 
                return lhs ^ rhs;
            case MhdScriptToken::Kind::OP_AND_BW: 
                return lhs & rhs;
            case MhdScriptToken::Kind::OP_LSHIFT: 
                return lhs << rhs;
            case MhdScriptToken::Kind::OP_RSHIFT: 
                return lhs >> rhs;
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdScriptExprBitwise
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprArithmetic final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprArithmetic(T... t)
        : MhdScriptExprBinary(t...) {}
public:
    MhdDynamic eval() const override
    {
        const MhdDynamic lhs = m_lhs->eval();
        const MhdDynamic rhs = m_rhs->eval();
        switch (m_op) {
            case MhdScriptToken::Kind::OP_ADD: 
                return lhs + rhs;
            case MhdScriptToken::Kind::OP_SUB: 
                return lhs - rhs;
            case MhdScriptToken::Kind::OP_MUL: 
                return lhs * rhs;
            case MhdScriptToken::Kind::OP_DIV: 
                return lhs / rhs;
            case MhdScriptToken::Kind::OP_MOD: 
                return lhs % rhs;
            default: 
                ORCHID_ASSERT(0); 
                return MhdDynamic();
        }
    }
};  // struct MhdScriptExprArithmetic
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprCompound final : public MhdScriptExpr
{
public:
    MhdScriptExpr::Vec m_exprs;
public:
    MhdScriptExprCompound(const MhdScriptExpr::Vec& exprs)
        : m_exprs(exprs) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        MhdVariableScope scope{};
        for (const MhdScriptExpr::Ptr& comp_expr : m_exprs) {
            expr = comp_expr->eval();
        }
        return expr;
    }
};  // struct MhdScriptExprCompound
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
    MhdScriptExprIf(MhdScriptExpr::Ptr cond, MhdScriptExpr::Ptr then_branch, MhdScriptExpr::Ptr else_branch)
        : m_cond(cond)
        , m_then_branch(then_branch), m_else_branch(else_branch) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        MhdVariableScope scope{};
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
};  // struct MhdScriptExprIf
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprSwitch final : public MhdScriptExprCond
{
public:
    MhdScriptExpr::Ptr m_cond;
    MhdScriptExpr::Map m_cases;
    MhdScriptExpr::Ptr m_case_default;
public:
    MhdScriptExprSwitch(MhdScriptExpr::Ptr cond, const MhdScriptExpr::Map& cases, MhdScriptExpr::Ptr case_default)
        : m_cond(cond)
        , m_cases(cases), m_case_default(case_default) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        MhdVariableScope scope{};
        const MhdDynamic cond = m_cond->eval();
        try {
            for (const std::pair<MhdScriptExpr::Ptr, MhdScriptExpr::Ptr>& case_expr : m_cases) {
                const MhdScriptExpr::Ptr& case_value = case_expr.first;
                const MhdScriptExpr::Ptr& case_branch = case_expr.second;
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
    MhdScriptExprWhile(MhdScriptExpr::Ptr cond, MhdScriptExpr::Ptr body)
        : m_cond(cond)
        , m_body(body) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        MhdVariableScope scope{};
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
};  // struct MhdScriptExprWhile
//--------------------------------------------------------------------------------------------------------
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
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        MhdVariableScope scope{};
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
    MhdScriptExprFor(MhdScriptExpr::Ptr init, MhdScriptExpr::Ptr cond, MhdScriptExpr::Ptr iter, MhdScriptExpr::Ptr body) 
        : m_init(init), m_cond(cond), m_iter(iter)
        , m_body(body) {}
public:
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        MhdVariableScope scope{};
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
};  // struct MhdScriptExprFor
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptExprTryCatch : public MhdScriptExpr
{
public:
    MhdScriptExpr::Ptr m_try_block;
    MhdScriptExpr::Ptr m_catch_block;
    std::string m_catch_arg;
public:
    MhdScriptExprTryCatch(MhdScriptExpr::Ptr try_block, MhdScriptExpr::Ptr catch_block, const std::string& catch_arg)
        : m_try_block(try_block)
        , m_catch_block(catch_block), m_catch_arg(catch_arg) {}
    MhdDynamic eval() const override
    {
        MhdDynamic expr;
        MhdVariableScope scope{};
        try {
            expr = m_try_block->eval();
        } catch (const MhdJumpThrow& throw_jump) {
            MhdVariable(m_catch_arg, throw_jump.m_val);
            expr = m_catch_block->eval();
        }
        return expr;
    }
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
    MhdDynamic eval() const override
    {
        if (m_expr != nullptr) {
            throw MhdJumpBreak(m_expr->eval());
        } else {
            throw MhdJumpBreak();
        }
    }
};  // struct MhdScriptExprBreak
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprContinue final : public MhdScriptExprJump
{
public:
    MhdScriptExprContinue() {}
public:
    MhdDynamic eval() const override
    {
        throw MhdJumpContinue();
    }
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
    MhdDynamic eval() const override
    {
        if (m_expr != nullptr) {
            throw MhdJumpReturn(m_expr->eval());
        } else {
            throw MhdJumpReturn();
        }
    }
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
    MhdDynamic eval() const override
    {
        if (m_expr != nullptr) {
            throw MhdJumpThrow(m_expr->eval());
        } else {
            throw MhdJumpThrow();
        }
    }
};  // struct MhdScriptExprReturn
//########################################################################################################
//########################################################################################################
//########################################################################################################


