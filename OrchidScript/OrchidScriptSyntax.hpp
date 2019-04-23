// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScriptScanner.hpp"
#include "OrchidScriptValue.hpp"
#include "OrchidScriptVar.hpp"

#include <algorithm>
#include <memory>
#include <vector>
#include <set>
#include <map>

struct MhdRuntimeException {};

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
    MhdScriptVal m_val; 
public:
    MhdJumpBreak() : m_val() {}
    MhdJumpBreak(MhdScriptVal val) : m_val(val) {}
};  // struct MhdJumpBreak
//--------------------------------------------------------------------------------------------------------
struct MhdJumpContinue final : public MhdJump 
{
};  // struct MhdJumpContinue
//--------------------------------------------------------------------------------------------------------
struct MhdJumpReturn final : public MhdJump
{ 
public:
    MhdScriptVal m_val; 
public: 
    MhdJumpReturn() : m_val() {}
    MhdJumpReturn(MhdScriptVal val) : m_val(val) {}
};  // struct MhdJumpReturn
//--------------------------------------------------------------------------------------------------------
struct MhdJumpThrow final : public MhdJump
{ 
public:
    MhdScriptVal m_val; 
public: 
    MhdJumpThrow() : m_val() {}
    MhdJumpThrow(MhdScriptVal val) : m_val(val) {}
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
    MhdScriptVal eval() const override
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
    MhdScriptExprConstFunc(const std::vector<std::string>& args, MhdScriptExpr::Ptr body) noexcept
        : m_args(args)
        , m_body(body) {}
public:
    MhdScriptVal eval() const override
    {
        const std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)> func =
            [args_name=m_args, body=m_body](const std::vector<MhdScriptVal>& args) {
                MhdScriptVarScope scope{true};
                if (args.size() != args_name.size()) {
                    throw MhdScriptInvalidOp(); 
                }
                for (std::size_t i = 0; i < args.size(); ++i) {
                    MhdScriptVarScope::var(args_name[i]) = args[i];
                }
                return body->eval();
            };
        return MhdScriptVal(func);
    }
};  // struct MhdScriptExprConstFunc
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprConstArray : public MhdScriptExpr
{
public:
    MhdScriptExpr::Vec m_array;
public:
    MhdScriptExprConstArray(const MhdScriptExpr::Vec& array)
        : m_array(array) {}
public:
    MhdScriptVal eval() const override
    {
        std::vector<MhdScriptVal> array;
        for (MhdScriptExpr::Ptr value : m_array) {
            array.push_back(value->eval());
        }
        return MhdScriptVal(array);
    }
};  // struct MhdScriptExprConstArray
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprIdent : public MhdScriptExpr
{
public:
    std::string m_id;
    bool m_let;
public:
    MhdScriptExprIdent(const std::string& id, bool let = false)
        : m_id(id), m_let(let) {}
public:
    MhdScriptVal eval() const override
    {
        if (m_let) {
            return MhdScriptVarScope::let(m_id);
        } else {
            return MhdScriptVarScope::var(m_id);
        }
    }
    MhdScriptRef eval_ref() const override
    { 
        if (m_let) {
            return MhdScriptRef(&MhdScriptVarScope::let(m_id));
        } else {
            return MhdScriptRef(&MhdScriptVarScope::var(m_id));
        }
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
    MhdScriptExprCall(MhdScriptExpr::Ptr func, const MhdScriptExpr::Vec& args) noexcept
        : m_func(func), m_args(args) {}
public:
    MhdScriptVal eval() const override
    {
        const MhdScriptVal func = m_func->eval();
        std::vector<MhdScriptVal> args;
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
    MhdScriptExprIndex(MhdScriptExpr::Ptr array, const MhdScriptExpr::Vec& index) noexcept
        : m_array(array), m_index(index) {}
public:
    MhdScriptVal eval() const override
    {
        const MhdScriptVal array = m_array->eval();
        std::vector<MhdScriptVal> index;
        for (MhdScriptExpr::Ptr arg : m_index) {
            index.push_back(arg->eval());
        }
        return array[index];
    }
    MhdScriptRef eval_ref() const override
    { 
        std::vector<MhdScriptVal> index;
        for (MhdScriptExpr::Ptr arg : m_index) {
            index.push_back(arg->eval());
        }
        return m_array->eval_ref()[index];
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
    MhdScriptExprUnary(MhdScriptToken::Kind op, MhdScriptExpr::Ptr expr) noexcept
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
    MhdScriptVal eval() const override
    {
        const MhdScriptVal expr = m_expr->eval();
        switch (m_op) {
            case MhdScriptToken::Kind::OP_NOT:    
                return !expr;
            case MhdScriptToken::Kind::OP_NOT_BW: 
                return ~expr;
            default: 
                ORCHID_ASSERT(0); 
                return MhdScriptVal();
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
    MhdScriptVal eval() const override
    {
        const MhdScriptVal expr = m_expr->eval();
        switch (m_op) {
            case MhdScriptToken::Kind::OP_ADD: 
                return +expr;
            case MhdScriptToken::Kind::OP_SUB: 
                return -expr;
            default: 
                ORCHID_ASSERT(0); 
                return MhdScriptVal();
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
    MhdScriptExprBinary(MhdScriptToken::Kind op, 
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
    MhdScriptVal eval() const override
    {
        MhdScriptRef lhs = eval_ref();
        return lhs;
    }
    MhdScriptRef eval_ref() const override
    {
        MhdScriptRef lhs = m_lhs->eval_ref();
        const MhdScriptVal rhs = m_rhs->eval();
        switch (m_op) {
            case MhdScriptToken::Kind::OP_ASG:        
                return lhs = rhs;
            case MhdScriptToken::Kind::OP_ADD_ASG: 
                return lhs = lhs + rhs;
            case MhdScriptToken::Kind::OP_SUB_ASG: 
                return lhs = lhs - rhs;
            case MhdScriptToken::Kind::OP_MUL_ASG: 
                return lhs = lhs * rhs;
            case MhdScriptToken::Kind::OP_DIV_ASG: 
                return lhs = lhs / rhs;
            case MhdScriptToken::Kind::OP_MOD_ASG: 
                return lhs = lhs % rhs;
            case MhdScriptToken::Kind::OP_OR_BW_ASG:  
                return lhs = lhs | rhs;
            case MhdScriptToken::Kind::OP_XOR_BW_ASG: 
                return lhs = lhs ^ rhs;
            case MhdScriptToken::Kind::OP_AND_BW_ASG: 
                return lhs = rhs & rhs;
            case MhdScriptToken::Kind::OP_LSHIFT_ASG: 
                return lhs = rhs << rhs;
            case MhdScriptToken::Kind::OP_RSHIFT_ASG: 
                return lhs = rhs >> rhs;
            default: 
                ORCHID_ASSERT(0); 
                return MhdScriptRef();
        }
    }
};  // struct MhdScriptExprAssignment
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprLogical final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprLogical(T... t) noexcept
        : MhdScriptExprBinary(t...) {}
public:
    MhdScriptVal eval() const override
    {
        if (m_op == MhdScriptToken::Kind::OP_AND) {
            if (m_lhs->eval()) {
                return m_rhs->eval();
            } else {
                return MhdScriptVal(false);
            }
        } else if (m_op == MhdScriptToken::Kind::OP_OR) {
            if (m_lhs->eval()) {
                return MhdScriptVal(true);
            } else {
                return m_rhs->eval();
            }
        } else {
            const MhdScriptVal lhs = m_lhs->eval();
            const MhdScriptVal rhs = m_rhs->eval();
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
                    return MhdScriptVal();
            }
        }
    }
};  // struct MhdScriptExprLogical
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprBitwise final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprBitwise(T... t) noexcept
        : MhdScriptExprBinary(t...) {}
public:
    MhdScriptVal eval() const override
    {
        const MhdScriptVal lhs = m_lhs->eval();
        const MhdScriptVal rhs = m_rhs->eval();
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
                return MhdScriptVal();
        }
    }
};  // struct MhdScriptExprBitwise
//--------------------------------------------------------------------------------------------------------
struct MhdScriptExprArithmetic final : public MhdScriptExprBinary
{
public:
    template<typename... T>
    MhdScriptExprArithmetic(T... t) noexcept
        : MhdScriptExprBinary(t...) {}
public:
    MhdScriptVal eval() const override
    {
        const MhdScriptVal lhs = m_lhs->eval();
        const MhdScriptVal rhs = m_rhs->eval();
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
                return MhdScriptVal();
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
    MhdScriptExprCompound(MhdScriptExpr::Ptr expr1) noexcept
        : m_exprs({ expr1 }) {}
    MhdScriptExprCompound(MhdScriptExpr::Ptr expr1, MhdScriptExpr::Ptr expr2) noexcept
        : m_exprs({ expr1, expr2 }) {}
    MhdScriptExprCompound(const MhdScriptExpr::Vec& exprs) noexcept
        : m_exprs(exprs) {}
public:
    MhdScriptVal eval() const override
    {
        MhdScriptVal expr;
        MhdScriptVarScope scope{};
        for (const MhdScriptExpr::Ptr& comp_expr : m_exprs) {
            expr = comp_expr->eval();
        }
        return expr;
    }
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
        , m_exprs(exprs)
    {}
public:
    MhdScriptVal eval() const override
    {
        return MhdScriptVarScope::let(m_id) = [&](){
            const MhdScriptVal& prev = MhdScriptVarScope::let(m_id);
            MhdScriptVarScope scope{};
            if (prev.m_type == MhdScriptVal::Type::MAP) {
                MhdScriptVarScope::load(*prev.m_val_map.get());
            }
            for (const MhdScriptExpr::Ptr& comp_expr : m_exprs) {
                comp_expr->eval();
            }
            return MhdScriptVarScope::dump();
        }();
    }
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
    MhdScriptVal eval() const override
    {
        MhdScriptVal expr;
        MhdScriptVarScope scope{};
        const MhdScriptVal cond = m_cond->eval();
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
    MhdScriptExprSwitch(MhdScriptExpr::Ptr cond, const MhdScriptExpr::Map& cases, 
                        MhdScriptExpr::Ptr case_default) noexcept
        : m_cond(cond)
        , m_cases(cases), m_case_default(case_default) {}
public:
    MhdScriptVal eval() const override
    {
        MhdScriptVal expr;
        MhdScriptVarScope scope{};
        const MhdScriptVal cond = m_cond->eval();
        try {
            const auto iter = std::find_if(m_cases.begin(), m_cases.end(), 
                [&](const std::pair<MhdScriptExpr::Ptr, MhdScriptExpr::Ptr>& case_expr) {
                    const MhdScriptExpr::Ptr& case_value = case_expr.first;
                    return case_value->eval() == cond;
                });
            if (iter != m_cases.end()) {
                const MhdScriptExpr::Ptr& case_branch = iter->second;
                expr = case_branch->eval();
            } else {
                if (m_case_default != nullptr) {
                    expr = m_case_default->eval();
                }
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
    MhdScriptExprWhile(MhdScriptExpr::Ptr cond, 
                       MhdScriptExpr::Ptr body)
        : m_cond(cond)
        , m_body(body) {}
public:
    MhdScriptVal eval() const override
    {
        MhdScriptVal expr;
        MhdScriptVarScope scope{};
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
    MhdScriptVal eval() const override
    {
        MhdScriptVal expr;
        MhdScriptVarScope scope{};
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
    MhdScriptExprFor(MhdScriptExpr::Ptr init, MhdScriptExpr::Ptr cond, MhdScriptExpr::Ptr iter, 
                     MhdScriptExpr::Ptr body) 
        : m_init(init), m_cond(cond), m_iter(iter)
        , m_body(body) {}
public:
    MhdScriptVal eval() const override
    {
        MhdScriptVal expr;
        MhdScriptVarScope scope{};
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
    MhdScriptExprTryCatch(MhdScriptExpr::Ptr try_block, 
                          MhdScriptExpr::Ptr catch_block, const std::string& catch_arg)
        : m_try_block(try_block)
        , m_catch_block(catch_block), m_catch_arg(catch_arg) {}
    MhdScriptVal eval() const override
    {
        MhdScriptVal expr;
        MhdScriptVarScope scope{};
        try {
            expr = m_try_block->eval();
        } catch (const MhdJumpThrow& throw_jump) {
            MhdScriptVarScope::var(m_catch_arg) = throw_jump.m_val;
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
    MhdScriptVal eval() const override
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
    MhdScriptVal eval() const override
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
    MhdScriptVal eval() const override
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
    MhdScriptVal eval() const override
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


