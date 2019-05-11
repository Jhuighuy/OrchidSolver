// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"
#include "OrchidScriptScanner.hpp"
#include "OrchidScriptSyntax.hpp"

#include <stdexcept>
#include <string>

#ifndef MHD_SCRIPT_SUGAR
#define MHD_SCRIPT_SUGAR 1
#endif

#ifndef MHD_SCRIPT_BITWISE
#define MHD_SCRIPT_BITWISE 0
#endif

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
                                        const std::string& what = "") {
        return std::to_string(token.m_loc_line) + ":" + std::to_string(token.m_loc_column);
    }
};  // struct MhdParseError
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptParser
{
public:
    MhdScriptTokenizer m_tokenizer;
    MhdScriptToken m_token;
    int m_inside_loop;
    int m_inside_switch;
    int m_inside_func;
    int m_inside_try;
public:
    MhdScriptParser(const char* text)
        : m_tokenizer(text)
        , m_inside_loop(0), m_inside_switch(0)
        , m_inside_func(0), m_inside_try(0) {}
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
    MhdScriptExpr::Ptr parse_expression_decl_class();
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
    MhdScriptExpr::Ptr parse_expression_binary_eq_neq();
    MhdScriptExpr::Ptr parse_expression_binary_lt_lte_gt_gte();
    MhdScriptExpr::Ptr parse_expression_binary_add_sub();
    MhdScriptExpr::Ptr parse_expression_binary_mul_div_mod();
private:
    MhdScriptExpr::Ptr parse_expression_unary();
    MhdScriptExpr::Ptr parse_expression_unary_not();
    MhdScriptExpr::Ptr parse_expression_unary_negate();
private:
    MhdScriptExpr::Ptr parse_expression_operand();
private:
    MhdScriptExpr::Ptr parse_expression_operand_primary();
    MhdScriptExpr::Ptr parse_expression_operand_primary_func();
    MhdScriptExpr::Ptr parse_expression_operand_primary_list();
private:
    MhdScriptExpr::Ptr parse_expression_operand_factor();
    MhdScriptExpr::Ptr parse_expression_operand_factor_call(MhdScriptExpr::Ptr);
    MhdScriptExpr::Ptr parse_expression_operand_factor_index(MhdScriptExpr::Ptr);
    MhdScriptExpr::Ptr parse_expression_operand_factor_subscript(MhdScriptExpr::Ptr);
private:
    const char* parse_operator();
private:
    void advance() 
    { 
        /// Peek a next token.
        if (!m_tokenizer.scan(m_token)) {
            throw MhdParseError(m_token, m_token.m_value_str);
        }
    }
    bool matches(MhdScriptKind kind) const
    {
        return m_token.m_kind == kind;
    }
    template<typename... T>
    bool matches(MhdScriptKind kind, T... kinds) const
    {
        return matches(kind) || matches(kinds...);
    }
    bool matched(MhdScriptKind kind)
    {
        /// Peek a next token on a match.
        if (m_token.m_kind == kind) {
            advance();
            return true;
        } 
        return false;
    }
    template<typename... T>
    bool matched(MhdScriptKind kind, T... kinds)
    {
        /// Peek a next token on a match.
        return matched(kind) || matched(kinds...);
    }
    template<typename... T>
    void expect(MhdScriptKind kind, T... kinds)
    {
        if (!matched(kind, kinds...)) {
            throw MhdParseError(m_token);
        }
    }
    template<typename... T>
    void expects(MhdScriptKind kind, T... kinds)
    {
        if (!matches(kind, kinds...)) {
            throw MhdParseError(m_token);
        }
    }
    void unexpected()
    {
        expects(MhdScriptKind::ERR);
    }
};	// struct MhdScriptParser
//########################################################################################################
//########################################################################################################
//########################################################################################################
