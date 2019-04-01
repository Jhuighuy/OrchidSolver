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
    std::shared_ptr<MhdExpr> parse_expression();
public:
    std::shared_ptr<MhdExpr> parse_expression_asg();
    std::shared_ptr<MhdExpr> parse_expression_or();
    std::shared_ptr<MhdExpr> parse_expression_and();
    std::shared_ptr<MhdExpr> parse_expression_or_bw();
    std::shared_ptr<MhdExpr> parse_expression_and_bw();
    std::shared_ptr<MhdExpr> parse_expression_xor_bw();
    std::shared_ptr<MhdExpr> parse_expression_eq_neq();
    std::shared_ptr<MhdExpr> parse_expression_lt_lte_gt_gte();
    std::shared_ptr<MhdExpr> parse_expression_add_sub();
    std::shared_ptr<MhdExpr> parse_expression_mul_div_mod();
public:
    std::shared_ptr<MhdExpr> parse_expression_unary();
    std::shared_ptr<MhdExpr> parse_expression_unary_not();
    std::shared_ptr<MhdExpr> parse_expression_unary_negate();
    std::shared_ptr<MhdExpr> parse_expression_unary_operand();
public:
    void peek() { m_tokenizer.scan(m_token); }
};	// struct MhdParser
//########################################################################################################
//########################################################################################################
//########################################################################################################
