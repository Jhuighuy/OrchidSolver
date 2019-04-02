// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScriptScanner.hpp"
#include "OrchidScriptParserSyntax.hpp"

//########################################################################################################
//########################################################################################################
//########################################################################################################
enum class MhdParserErr 
{
    ERR_UNEXP_TOKEN,
    ERR_UNEXP_DEFAULT,
};
struct MhdParser
{
public:
    MhdTokenizer m_tokenizer;
    MhdToken m_token;
public:
    MhdParser(const char* text)
        : m_tokenizer(text) { peek(); }
public:

public:
    MhdExpr::Ptr parse();
    MhdExpr::Ptr parse_expression();
public:
    MhdExpr::Ptr parse_expression_compound();
public:
    MhdExpr::Ptr parse_expression_cond_if();
    MhdExpr::Ptr parse_expression_cond_switch();
public:
    MhdExpr::Ptr parse_expression_loop_while();
    MhdExpr::Ptr parse_expression_loop_do();
    MhdExpr::Ptr parse_expression_loop_for();
public:
    MhdExpr::Ptr parse_expression_jump_break();
    MhdExpr::Ptr parse_expression_jump_continue();
    MhdExpr::Ptr parse_expression_jump_return();
public:
    MhdExpr::Ptr parse_expression_binary_asg();
    MhdExpr::Ptr parse_expression_binary_or();
    MhdExpr::Ptr parse_expression_binary_and();
    MhdExpr::Ptr parse_expression_binary_or_bw();
    MhdExpr::Ptr parse_expression_binary_and_bw();
    MhdExpr::Ptr parse_expression_binary_xor_bw();
    MhdExpr::Ptr parse_expression_binary_eq_neq();
    MhdExpr::Ptr parse_expression_binary_lt_lte_gt_gte();
    MhdExpr::Ptr parse_expression_binary_add_sub();
    MhdExpr::Ptr parse_expression_binary_mul_div_mod();
public:
    MhdExpr::Ptr parse_expression_unary();
    MhdExpr::Ptr parse_expression_unary_not();
    MhdExpr::Ptr parse_expression_unary_negate();
    MhdExpr::Ptr parse_expression_unary_operand();
    MhdExpr::Ptr parse_expression_unary_factor();
    MhdExpr::Vec parse_expression_unary_factor_call();
    MhdExpr::Vec parse_expression_unary_factor_index();
public:
    void peek() { m_tokenizer.scan(m_token); }
};	// struct MhdParser
//########################################################################################################
//########################################################################################################
//########################################################################################################