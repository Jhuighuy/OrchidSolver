// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScanner.hpp"
#include <memory>

//########################################################################################################
//########################################################################################################
//########################################################################################################
enum MhdExprType
{
    ORCHID_EXPR_TYPE_LGC,
    ORCHID_EXPR_TYPE_INT,
    ORCHID_EXPR_TYPE_DBL,
};	// enum MhdExprType
struct orchid_expr_val
{

};	// struct orchid_expr_val
/** Expression. */
struct MhdExpr : std::enable_shared_from_this<MhdExpr>
{
public:
    MhdExprType m_type;
public:
    virtual ~MhdExpr() {}
    MhdExpr(...): m_type(ORCHID_EXPR_TYPE_LGC) {}
};  // struct MhdExpr
struct MhdExprConst : public MhdExpr
{
public:
    MhdExprConst(...) {}
};  // struct MhdExprConst
//########################################################################################################
//########################################################################################################
//########################################################################################################
/** Unary expression. */
struct MhdExprUnary : public MhdExpr
{
public:
    MhdToken::Kind m_op;
    std::shared_ptr<MhdExpr> m_expr;
public:
    virtual ~MhdExprUnary() {}
    MhdExprUnary(MhdToken::Kind op, std::shared_ptr<MhdExpr> expr)
        : m_op(op), m_expr(expr) { }
};	// struct MhdExprUnary
/** ``Not`` expression. */
typedef MhdExprUnary MhdExprNot;
/** ``Negate`` expression. */
typedef MhdExprUnary MhdExprNegate;
//########################################################################################################
//########################################################################################################
//########################################################################################################
/** Binary expression. */
struct MhdExprBinary : public MhdExpr
{
public:
    MhdToken::Kind m_op;
    std::shared_ptr<MhdExpr> m_lhs;
    std::shared_ptr<MhdExpr> m_rhs;
public:
    virtual ~MhdExprBinary() {}
    MhdExprBinary(MhdToken::Kind op, std::shared_ptr<MhdExpr> lhs, std::shared_ptr<MhdExpr> rhs)
        : m_op(op), m_lhs(lhs), m_rhs(rhs) { }
};	// struct MhdExprBinary
/** ``Logical Binary`` expression. */
typedef MhdExprBinary MhdExprLogical;
/** ``Bitwise Logical Binary`` expression. */
typedef MhdExprBinary MhdExprLogicalBw;
/** ``Arithmetic`` expression. */
typedef MhdExprBinary MhdExprArithmetic;
//########################################################################################################
//########################################################################################################
//########################################################################################################
