// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidParser.hpp"

//########################################################################################################
//########################################################################################################
//########################################################################################################
MhdExpr::Ptr MhdParser::parse_expression_or()
{
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_and();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_OR) {
        peek();
        expr = std::make_shared<MhdExprLogical>(op, expr, parse_expression_and());
    }
    return expr;
}
MhdExpr::Ptr MhdParser::parse_expression_and()
{
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_or_bw();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_AND) {
        peek();
        expr = std::make_shared<MhdExprLogical>(op, expr, parse_expression_or_bw());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MhdExpr::Ptr MhdParser::parse_expression_or_bw()
{
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_xor_bw();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_OR_BW) {
        peek();
        expr = std::make_shared<MhdExprLogicalBw>(op, expr, parse_expression_xor_bw());
    }
    return expr;
}
MhdExpr::Ptr MhdParser::parse_expression_xor_bw()
{
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_and_bw();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_XOR_BW) {
        peek();
        expr = std::make_shared<MhdExprLogicalBw>(op, expr, parse_expression_and_bw());
    }
    return expr;
}
MhdExpr::Ptr MhdParser::parse_expression_and_bw()
{
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_eq_neq();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_XOR_BW) {
        peek();
        expr = std::make_shared<MhdExprLogicalBw>(op, expr, parse_expression_eq_neq());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MhdExpr::Ptr MhdParser::parse_expression_eq_neq()
{
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_lt_lte_gt_gte();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_EQ ||
            op == MhdToken::Kind::OP_NEQ) {
        peek();
        expr = std::make_shared<MhdExprLogical>(op, expr, parse_expression_lt_lte_gt_gte());
    }
    return expr;
}
MhdExpr::Ptr MhdParser::parse_expression_lt_lte_gt_gte()
{
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_add_sub();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_LT || op == MhdToken::Kind::OP_LTE ||
            op == MhdToken::Kind::OP_GT || op == MhdToken::Kind::OP_GTE) {
        peek();
        expr = std::make_shared<MhdExprLogical>(op, expr, parse_expression_add_sub());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MhdExpr::Ptr MhdParser::parse_expression_add_sub()
{
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_mul_div_mod();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_ADD ||
            op == MhdToken::Kind::OP_SUB) {
        peek();
        expr = std::make_shared<MhdExprArithmetic>(op, expr, parse_expression_mul_div_mod());
    }
    return expr;
}
MhdExpr::Ptr MhdParser::parse_expression_mul_div_mod()
{
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_unary();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_MUL ||
            op == MhdToken::Kind::OP_DIV ||
            op == MhdToken::Kind::OP_MOD) {
        peek();
        expr = std::make_shared<MhdExprArithmetic>(op, expr, parse_expression_unary());
    }
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MhdExpr::Ptr MhdParser::parse_expression_unary()
{
    switch (m_token.m_kind) {
    case MhdToken::Kind::OP_NOT:
        //return parse_expression_unary_not();
    case MhdToken::Kind::OP_ADD:
    case MhdToken::Kind::OP_SUB:
        //return parse_expression_unary_negate();
    case MhdToken::Kind::OP_PAREN_OPEN:
        return nullptr;
    default:
        return parse_expression_unary_operand();
    }
}
MhdExpr::Ptr MhdParser::parse_expression_unary_operand()
{
    switch (m_token.m_kind) {
        case MhdToken::Kind::CT_STR:
            peek();
            return std::make_shared<MhdExprConst>(m_token.m_value_str);
        case MhdToken::Kind::CT_INT:
            peek();
            return std::make_shared<MhdExprConst>(m_token.m_value_int);
        case MhdToken::Kind::CT_DBL:
            peek();
            return std::make_shared<MhdExprConst>(m_token.m_value_dbl);
        case MhdToken::Kind::CT_LGC:
            if (m_token.m_value_int != 0) {
                peek();
                return std::make_shared<MhdExprConst>(true);
            } else {
                peek();
                return std::make_shared<MhdExprConst>(false);
            }
        default:
            return nullptr;
    }
}
//--------------------------------------------------------------------------------------------------------
//########################################################################################################
//########################################################################################################
//########################################################################################################


extern "C" void orchid_solver_scanner_test()
{
    MhdParser p("3+2");
    auto e = p.parse_expression_or();
    auto g = e.get();
}

