// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"
#include "OrchidScriptScanner.hpp"
#include "OrchidScriptSyntax.hpp"

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
    MhdParseError(const MhdScriptToken& token, const std::string& what)
        : std::runtime_error(make_parse_error(token, what)) {}
public:
    static std::string make_parse_error(const MhdScriptToken& token, 
                                        const std::string& what = "");
};  // struct MhdParseError
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptParser
{
public:
    MhdScriptTokenizer m_tokenizer;
    MhdScriptToken m_token;
public:
    MhdScriptParser(const char* text)
        : m_tokenizer(text) {}
public:
    MHD_INTERFACE
    MhdScriptExpr::Ptr parse();
    MHD_INTERFACE
    MhdScriptExpr::Ptr parse_wrap();
public:
    MHD_INTERFACE
    MhdScriptExpr::Ptr parse_program();
    MHD_INTERFACE
    MhdScriptExpr::Ptr parse_program_wrap();
private:
    MhdScriptExpr::Ptr parse_expression();
private:
    MhdScriptExpr::Ptr parse_expression_compound();
private:
    MhdScriptExpr::Ptr parse_expression_decl_function();
    MhdScriptExpr::Ptr parse_expression_decl_struct();
    MhdScriptExpr::Ptr parse_expression_decl_namespace();
private:
    MhdScriptExpr::Ptr parse_expression_cond_if();
    MhdScriptExpr::Ptr parse_expression_cond_switch();
private:
    MhdScriptExpr::Ptr parse_expression_loop_while();
    MhdScriptExpr::Ptr parse_expression_loop_do();
    MhdScriptExpr::Ptr parse_expression_loop_for();
    MhdScriptExpr::Ptr parse_expression_loop_foreach();
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
    void peek() 
    { 
        /// Peek a next token.
        if (!m_tokenizer.scan(m_token)) {
            throw MhdParseError(m_token, m_token.m_value_str);
        }
    }
};	// struct MhdScriptParser
//########################################################################################################
//########################################################################################################
//########################################################################################################
