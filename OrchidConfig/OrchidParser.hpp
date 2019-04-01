// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScanner.hpp"
#include "OrchidAST.hpp"

//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdParser
{
public:
    MhdTokenizer m_tokenizer;
    MhdToken m_token;
public:
    MhdParser(const char* text)
        : m_tokenizer(text) { peek(); }
public:
    MhdExpr::Ptr parse_expression();
public:
    MhdExpr::Ptr parse_expression_asg();
    MhdExpr::Ptr parse_expression_or();
    MhdExpr::Ptr parse_expression_and();
    MhdExpr::Ptr parse_expression_or_bw();
    MhdExpr::Ptr parse_expression_and_bw();
    MhdExpr::Ptr parse_expression_xor_bw();
    MhdExpr::Ptr parse_expression_eq_neq();
    MhdExpr::Ptr parse_expression_lt_lte_gt_gte();
    MhdExpr::Ptr parse_expression_add_sub();
    MhdExpr::Ptr parse_expression_mul_div_mod();
public:
    MhdExpr::Ptr parse_expression_unary();
    MhdExpr::Ptr parse_expression_unary_not();
    MhdExpr::Ptr parse_expression_unary_negate();
    MhdExpr::Ptr parse_expression_unary_operand();
public:
    void peek() { m_tokenizer.scan(m_token); }
};	// struct MhdParser
//########################################################################################################
//########################################################################################################
//########################################################################################################
