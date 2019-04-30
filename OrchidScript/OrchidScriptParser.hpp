// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScriptScanner.hpp"
#include "OrchidScriptSyntax.hpp"

#include <exception>
#include <stdexcept>
#include <string>

//########################################################################################################
//########################################################################################################
//########################################################################################################
enum class MhdScriptError 
{
    ERR_UNEXP_TOKEN,
    ERR_UNEXP_DEFAULT,
    ERR_FUNC_ARG_REDECL,
};
//--------------------------------------------------------------------------------------------------------
struct MhdParseError : public std::runtime_error
{
public:
    MhdParseError(...)
        : std::runtime_error("hui") {}
    MhdParseError(const std::string& what = "<unimplemented>") 
        : std::runtime_error(what) {}
public:
    static std::string make_parse_error(const MhdScriptToken& token)
    {
        (void)token;
        return "PARSE ERROR AT <file>:<line>,<column>: ";
    }
};  // struct MhdParseError
//--------------------------------------------------------------------------------------------------------
struct MhdParseUnexpTokenError : public MhdParseError
{
public:
    template<typename... T>
    MhdParseUnexpTokenError(const MhdScriptToken& token, const T&... expected)
        : MhdParseError(make_error_unexp_token(token, expected...)) {}
private:
    template<typename... T>
    static std::string make_error_unexp_token(const MhdScriptToken& token, const T&... /*expected*/)
    {
        return MhdParseError::make_parse_error(token);
    }
};  // struct MhdParseUnexpTokenError
//--------------------------------------------------------------------------------------------------------
struct MhdParseUnexpDefaultError : public MhdParseError
{
public:
    MhdParseUnexpDefaultError(struct MhdScriptParser*)
        : MhdParseError(std::string("what")) {}
};  // struct MhdParseUnexpDefaultError
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptParser
{
public:
    MhdTokenizer m_tokenizer;
    MhdScriptToken m_token;
public:
    MhdScriptParser(const char* text)
        : m_tokenizer(text) { peek(); }
public:
    MhdScriptExpr::Ptr parse_wrap();
private:
    MhdScriptExpr::Ptr parse();
    MhdScriptExpr::Ptr parse_expression();
private:
    MhdScriptExpr::Ptr parse_expression_compound();
    MhdScriptExpr::Ptr parse_expression_namespace();
private:
    MhdScriptExpr::Ptr parse_expression_cond_if();
    MhdScriptExpr::Ptr parse_expression_cond_switch();
private:
    MhdScriptExpr::Ptr parse_expression_loop_while();
    MhdScriptExpr::Ptr parse_expression_loop_do();
    MhdScriptExpr::Ptr parse_expression_loop_for();
private:
    MhdScriptExpr::Ptr parse_expression_try_catch();
private:
    MhdScriptExpr::Ptr parse_expression_jump_break();
    MhdScriptExpr::Ptr parse_expression_jump_continue();
    MhdScriptExpr::Ptr parse_expression_jump_return();
    MhdScriptExpr::Ptr parse_expression_jump_throw();
private:
    MhdScriptExpr::Ptr parse_expression_comma();
    MhdScriptExpr::Ptr parse_expression_binary_asg();
    MhdScriptExpr::Ptr parse_expression_ternary();
    MhdScriptExpr::Ptr parse_expression_binary_or();
    MhdScriptExpr::Ptr parse_expression_binary_and();
    MhdScriptExpr::Ptr parse_expression_binary_or_bw();
    MhdScriptExpr::Ptr parse_expression_binary_and_bw();
    MhdScriptExpr::Ptr parse_expression_binary_xor_bw();
    MhdScriptExpr::Ptr parse_expression_binary_eq_neq();
    MhdScriptExpr::Ptr parse_expression_binary_lt_lte_gt_gte();
    MhdScriptExpr::Ptr parse_expression_binary_shift();
    MhdScriptExpr::Ptr parse_expression_binary_add_sub();
    MhdScriptExpr::Ptr parse_expression_binary_mul_div_mod();
private:
    MhdScriptExpr::Ptr parse_expression_unary();
    MhdScriptExpr::Ptr parse_expression_unary_not();
    MhdScriptExpr::Ptr parse_expression_unary_negate();
private:
    MhdScriptExpr::Ptr parse_expression_unary_operand();
    MhdScriptExpr::Ptr parse_expression_unary_operand_func();
    MhdScriptExpr::Ptr parse_expression_unary_operand_list();
private:
    MhdScriptExpr::Ptr parse_expression_unary_factor();
    MhdScriptExpr::Ptr parse_expression_unary_factor_call(MhdScriptExpr::Ptr);
    MhdScriptExpr::Ptr parse_expression_unary_factor_index(MhdScriptExpr::Ptr);
    MhdScriptExpr::Ptr parse_expression_unary_factor_subscript(MhdScriptExpr::Ptr);
private:
    void peek() { m_tokenizer.scan(m_token); }
};	// struct MhdScriptParser
//########################################################################################################
//########################################################################################################
//########################################################################################################
