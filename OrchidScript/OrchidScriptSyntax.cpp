// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptSyntax.hpp"
#include "OrchidScriptVar.hpp"

#include <algorithm>
#include <set>
#include <map>

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
public: 
    MhdJumpContinue() {}
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
MHD_INTERFACE
MhdScriptVal
MhdScriptExprConst::eval() const
{
    /// Evaluate CONSTANT expression.
    return m_value;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprConstFunc::eval() const
{
    /// Evaluate FUNCTION CONSTANT expression.
    const auto func = [args_name=m_args, body=m_body](const std::vector<MhdScriptVal>& args) {
        MhdScriptVarScope scope{};
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
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprConstArray::eval() const
{
    /// Evaluate LIST CONSTANT expression.
    std::vector<MhdScriptVal> array;
    for (MhdScriptExpr::Ptr value : m_array) {
        array.push_back(value->eval());
    }
    return MhdScriptVal(array);
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprIdent::eval() const
{
    /// Evaluate IDENTIFIER expression.
    if (m_let) {
        return MhdScriptVarScope::let(m_id);
    } else {
        return MhdScriptVarScope::var(m_id);
    }
}
MHD_INTERFACE
MhdScriptRef
MhdScriptExprIdent::eval_ref() const
{
    /// Evaluate IDENTIFIER expression.
    if (m_let) {
        return MhdScriptRef(&MhdScriptVarScope::let(m_id));
    } else {
        return MhdScriptRef(&MhdScriptVarScope::var(m_id));
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal
MhdScriptExprCall::eval() const
{
    /// Evaluate CALL expression.
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
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprIndex::eval() const
{
    /// Evaluate INDEX expression.
    std::vector<MhdScriptVal> index;
    for (MhdScriptExpr::Ptr arg : m_index) {
        index.push_back(arg->eval());
    }
    const MhdScriptVal array = m_array->eval();
    return array[index];
}
MHD_INTERFACE
MhdScriptRef
MhdScriptExprIndex::eval_ref() const
{
    /// Evaluate INDEX expression.
    std::vector<MhdScriptVal> index;
    for (MhdScriptExpr::Ptr arg : m_index) {
        index.push_back(arg->eval());
    }
    MhdScriptRef array = m_array->eval_ref();
    return array[index];
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal
MhdScriptExprNot::eval() const
{
    /// Evaluate NOT expression.
    const MhdScriptVal expr = m_expr->eval();
    switch (m_op) {
        case MhdScriptKind::OP_NOT:    
            return !expr;
        case MhdScriptKind::OP_NOT_BW: 
            return ~expr;
        default: 
            ORCHID_ASSERT(0); 
            return MhdScriptVal();
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprNegate::eval() const
{
    /// Evaluate NEGATE expression.
    const MhdScriptVal expr = m_expr->eval();
    switch (m_op) {
        case MhdScriptKind::OP_ADD: 
            return +expr;
        case MhdScriptKind::OP_SUB: 
            return -expr;
        default: 
            ORCHID_ASSERT(0); 
            return MhdScriptVal();
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal
MhdScriptExprAssignment::eval() const
{
    /// Evaluate ASSIGNMENT expression.
    MhdScriptRef lhs = eval_ref();
    return lhs;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptRef
MhdScriptExprAssignment::eval_ref() const
{
    /// Evaluate ASSIGNMENT expression.
    MhdScriptRef lhs = m_lhs->eval_ref();
    const MhdScriptVal rhs = m_rhs->eval();
    switch (m_op) {
        case MhdScriptKind::OP_ASG:        
            return lhs = rhs;
        case MhdScriptKind::OP_ADD_ASG: 
            return lhs = lhs + rhs;
        case MhdScriptKind::OP_SUB_ASG: 
            return lhs = lhs - rhs;
        case MhdScriptKind::OP_MUL_ASG: 
            return lhs = lhs * rhs;
        case MhdScriptKind::OP_DIV_ASG: 
            return lhs = lhs / rhs;
        case MhdScriptKind::OP_MOD_ASG: 
            return lhs = lhs % rhs;
        case MhdScriptKind::OP_OR_BW_ASG:  
            return lhs = lhs | rhs;
        case MhdScriptKind::OP_XOR_BW_ASG: 
            return lhs = lhs ^ rhs;
        case MhdScriptKind::OP_AND_BW_ASG: 
            return lhs = rhs & rhs;
        case MhdScriptKind::OP_LSHIFT_ASG: 
            return lhs = rhs << rhs;
        case MhdScriptKind::OP_RSHIFT_ASG: 
            return lhs = rhs >> rhs;
        default: 
            ORCHID_ASSERT(0); 
            return MhdScriptRef();
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal
MhdScriptExprLogical::eval() const
{
    /// Evaluate LOGICAL expression.
    if (m_op == MhdScriptKind::OP_AND) {
        if (m_lhs->eval()) {
            return m_rhs->eval();
        } else {
            return MhdScriptVal(false);
        }
    } else if (m_op == MhdScriptKind::OP_OR) {
        if (m_lhs->eval()) {
            return MhdScriptVal(true);
        } else {
            return m_rhs->eval();
        }
    } else {
        const MhdScriptVal lhs = m_lhs->eval();
        const MhdScriptVal rhs = m_rhs->eval();
        switch (m_op) {
            case MhdScriptKind::OP_EQ:  
                return lhs == rhs;
            case MhdScriptKind::OP_NEQ: 
                return lhs != rhs;
            case MhdScriptKind::OP_LT:  
                return lhs <  rhs;
            case MhdScriptKind::OP_LTE: 
                return lhs <= rhs;
            case MhdScriptKind::OP_GT:  
                return lhs >  rhs;
            case MhdScriptKind::OP_GTE: 
                return lhs >= rhs;
            default: 
                ORCHID_ASSERT(0); 
                return MhdScriptVal();
        }
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprBitwise::eval() const
{
    /// Evaluate BITWISE expression.
    const MhdScriptVal lhs = m_lhs->eval();
    const MhdScriptVal rhs = m_rhs->eval();
    switch (m_op) {
        case MhdScriptKind::OP_OR_BW:  
            return lhs | rhs;
        case MhdScriptKind::OP_XOR_BW: 
            return lhs ^ rhs;
        case MhdScriptKind::OP_AND_BW: 
            return lhs & rhs;
        case MhdScriptKind::OP_LSHIFT: 
            return lhs << rhs;
        case MhdScriptKind::OP_RSHIFT: 
            return lhs >> rhs;
        default: 
            ORCHID_ASSERT(0); 
            return MhdScriptVal();
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprArithmetic::eval() const
{
    /// Evaluate ARITHMETIC expression.
    const MhdScriptVal lhs = m_lhs->eval();
    const MhdScriptVal rhs = m_rhs->eval();
    switch (m_op) {
        case MhdScriptKind::OP_ADD: 
            return lhs + rhs;
        case MhdScriptKind::OP_SUB: 
            return lhs - rhs;
        case MhdScriptKind::OP_MUL: 
            return lhs * rhs;
        case MhdScriptKind::OP_DIV: 
            return lhs / rhs;
        case MhdScriptKind::OP_MOD: 
            return lhs % rhs;
        default: 
            ORCHID_ASSERT(0); 
            return MhdScriptVal();
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal
MhdScriptExprCompound::eval() const
{
    /// Evaluate COMPOUND expression.
    MhdScriptVal expr;
    MhdScriptVarScope scope{};
    for (const MhdScriptExpr::Ptr& comp_expr : m_exprs) {
        expr = comp_expr->eval();
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprNamespace::eval() const
{
    /// Evaluate NAMESPACE expression.
    MhdScriptVal& expr = MhdScriptVarScope::let(m_id);
    const MhdScriptVal& prev = MhdScriptVarScope::let(m_id);
    MhdScriptVarScope scope{};
    if (prev.m_type == MhdScriptVal::Type::MAP) {
        MhdScriptVarScope::load(*prev.m_val_map.get());
    }
    for (const MhdScriptExpr::Ptr& comp_expr : m_exprs) {
        comp_expr->eval();
    }
    return expr = MhdScriptVarScope::dump();
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal
MhdScriptExprIf::eval() const
{
    /// Evaluate IF expression.
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
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprSwitch::eval() const
{
    /// Evaluate SWITCH expression.
    MhdScriptVal expr;
    MhdScriptVarScope scope{};
    const MhdScriptVal cond = m_cond->eval();
    const auto iter = std::find_if(m_cases.begin(), m_cases.end(), 
        [&](const std::pair<MhdScriptExpr::Ptr, 
                            MhdScriptExpr::Ptr>& case_expr) {
            const MhdScriptExpr::Ptr& case_value_expr = case_expr.first;
            return case_value_expr->eval() == cond;
        });
    try {
        if (iter != m_cases.end()) {
            const MhdScriptExpr::Ptr& case_branch_expr = iter->second;
            expr = case_branch_expr->eval();
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
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal
MhdScriptExprWhile::eval() const
{
    /// Evaluate WHILE expression.
    MhdScriptVal expr;
    MhdScriptVarScope scope{};
    try {
        while (m_cond->eval()) {
            try {
                expr = m_body->eval();
            } catch (const MhdJumpContinue&) {
            }
        }
    } catch (const MhdJumpBreak& break_jump) {
        expr = break_jump.m_val;
    }
    return expr;
}
MHD_INTERFACE
MhdScriptVal
MhdScriptExprDoWhile::eval() const
{
    /// Evaluate DO WHILE expression.
    MhdScriptVal expr;
    MhdScriptVarScope scope{};
    try {
        do {
            try {
                expr = m_body->eval();
            } catch (const MhdJumpContinue&) {
            }
        } while (m_cond->eval());
    } catch (const MhdJumpBreak& break_jump) {
        expr = break_jump.m_val;
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprFor::eval() const
{
    /// Evaluate FOR LOOP expression.
    MhdScriptVal expr;
    MhdScriptVarScope scope{};
    try {
        for (m_init == nullptr || m_init->eval(); 
             m_cond == nullptr || m_cond->eval(); 
             m_iter == nullptr || m_iter->eval()) {
            try {
                expr = m_body->eval();
            } catch (const MhdJumpContinue&) {
            }
        }
    } catch (const MhdJumpBreak& break_jump) {
        expr = break_jump.m_val;
    }
    return expr;
}
MHD_INTERFACE
MhdScriptVal
MhdScriptExprForEach::eval() const
{
    /// Evaluate FOR-EACH LOOP expression.
    MhdScriptVal expr;
    MhdScriptVarScope scope{};
    try {
        const MhdScriptVal cont = m_cont->eval();
#if 0   /** @todo Implement iterators for MhdScriptVal. */
        for (MhdScriptVal val : cont) {
            try {
                MhdScriptVarScope::let(m_id) = val;
                expr = m_body->eval();
            } catch (const MhdJumpContinue&) {
            }
        }
#endif
        ORCHID_ASSERT(0);
    } catch (const MhdJumpBreak& break_jump) {
        expr = break_jump.m_val;
    }
    ORCHID_ASSERT(0);
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal
MhdScriptExprTryCatch::eval() const
{
    /// Evaluate TRY-CATCH expression.
    MhdScriptVal expr;
    MhdScriptVarScope scope{};
    try {
        expr = m_try_block->eval();
    } catch (const MhdJumpThrow& throw_jump) {
        MhdScriptVarScope scope{};
        if (m_catch_block != nullptr) {
            MhdScriptVarScope::let(m_catch_arg) = throw_jump.m_val;
            expr = m_catch_block->eval();
        }
    }
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal
MhdScriptExprBreak::eval() const
{
    /// Evaluate BREAK expression.
    if (m_expr != nullptr) {
        throw MhdJumpBreak(m_expr->eval());
    } else {
        throw MhdJumpBreak();
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprContinue::eval() const
{
    /// Evaluate CONTINUE expression.
    throw MhdJumpContinue();
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprReturn::eval() const
{
    /// Evaluate RETURN expression.
    if (m_expr != nullptr) {
        throw MhdJumpReturn(m_expr->eval());
    } else {
        throw MhdJumpReturn();
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptExprThrow::eval() const
{
    /// Evaluate THROW expression.
    if (m_expr != nullptr) {
        throw MhdJumpThrow(m_expr->eval());
    } else {
        throw MhdJumpThrow();
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
