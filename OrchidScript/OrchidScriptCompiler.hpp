// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"
#include "OrchidScriptScanner.hpp"

#include <stdexcept>
#include <string>

#ifndef MHD_SCRIPT_SUGAR
#define MHD_SCRIPT_SUGAR 1
#endif

#ifndef MHD_SCRIPT_BITWISE
#define MHD_SCRIPT_BITWISE 0
#endif

enum struct MhdLangOpcode : unsigned char
{
    NOOP,
    DISCARD_1,
    LOAD_TRUE,
    LOAD_FALSE,
    LOAD_NULLPTR,
    LOAD_UI_0,
    LOAD_UI_1,
    LOAD_UI_2,
    LOAD_UI_3,
    LOAD_UI8,
    LOAD_UI16,
    LOAD_UI32,
    LOAD_FP32,
    LOAD_FP64,
    LOAD_CSTR,
    JUMP,
    JUMP_Z,
    JUMP_NZ,
    OP_ASG,
    OP_NOT,
    OP_PLUS,
    OP_MINUS,
    OP_OR,
    OP_AND,
    OP_EQ,
    OP_NEQ,
    OP_LT,
    OP_GT,
    OP_LTE,
    OP_GTE,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_MOD,
    OP_ADD_ASG,
    OP_SUB_ASG,
    OP_MUL_ASG,
    OP_DIV_ASG,
    OP_MOD_ASG,
};
struct MhdLangByteCodeLabel
{
    MhdLangByteCodeLabel() {}
};
struct MhdLangByteCode
{
    void emit_ui8 (unsigned) {}
    void emit_ui16(unsigned) {}
    void emit_ui32(unsigned) {}
    void emit_fp32(float) {}
    void emit_fp64(double) {}
    void emit_cstr(const char*) {}
    void emit_code(MhdLangOpcode) {}
    void emit_code(MhdLangOpcode, MhdLangByteCodeLabel&) {}
    void emit_addr(MhdLangByteCodeLabel&) {}
    void label(MhdLangByteCodeLabel&) {}
};

//########################################################################################################
//########################################################################################################
//########################################################################################################
enum class MhdLangError
{
    ERR_UNEXP_TOKEN,
    ERR_UNEXP_DEFAULT,
    ERR_FUNC_ARG_REDECL,
};
//--------------------------------------------------------------------------------------------------------
struct MhdCompileError : public std::runtime_error
{
public:
    MhdCompileError(...)
        : std::runtime_error("hui") {}
    MhdCompileError(const std::string& what = "<unimplemented>")
        : std::runtime_error(what) {}
    MhdCompileError(const MhdScriptToken& token, const std::string& what)
        : std::runtime_error(make_compile_error(token, what)) {}
public:
    static std::string make_compile_error(const MhdScriptToken& token,
        const std::string& what = "") {
        return std::to_string(token.m_loc_line) + ":" + std::to_string(token.m_loc_column);
    }
};  // struct MhdParseError
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdLangCompiler
{
public:
    MhdScriptTokenizer m_tokenizer;
    MhdScriptToken m_token;
    int m_inside_loop;
    int m_inside_switch;
    int m_inside_func;
    int m_inside_try;
public:
    MhdLangCompiler(const char* text)
        : m_tokenizer(text)
        , m_inside_loop(0), m_inside_switch(0)
        , m_inside_func(0), m_inside_try(0) {}
public:
    MHD_INTERFACE
    void compile(MhdLangByteCode& bytecode);
    MHD_INTERFACE
    void compile_wrap();
public:
    MHD_INTERFACE
    void compile_program(MhdLangByteCode& bytecode);
    MHD_INTERFACE
    void compile_program_wrap(MhdLangByteCode& bytecode);
private:
    void compile_expression_compound(MhdLangByteCode& bytecode);
private:
    void compile_expression_decl_function(MhdLangByteCode& bytecode);
    void compile_expression_decl_class(MhdLangByteCode& bytecode);
    void compile_expression_decl_namespace(MhdLangByteCode& bytecode);
private:
    void compile_expression_cond_if(MhdLangByteCode& bytecode);
    void compile_expression_cond_switch(MhdLangByteCode& bytecode);
private:
    void compile_expression_loop_do(MhdLangByteCode& bytecode);
    void compile_expression_loop_while(MhdLangByteCode& bytecode);
    void compile_expression_loop_for(MhdLangByteCode& bytecode);
    void compile_expression_loop_foreach(MhdLangByteCode& bytecode);
private:
    void compile_expression_try_catch(MhdLangByteCode& bytecode);
private:
    void compile_expression_jump_break(MhdLangByteCode& bytecode);
    void compile_expression_jump_continue(MhdLangByteCode& bytecode);
    void compile_expression_jump_return(MhdLangByteCode& bytecode);
    void compile_expression_jump_throw(MhdLangByteCode& bytecode);
private:
    void compile_expression(MhdLangByteCode& bytecode);
private:
    void compile_expression_comma(MhdLangByteCode& bytecode);
    void compile_expression_binary_asg(MhdLangByteCode& bytecode);
    void compile_expression_ternary(MhdLangByteCode& bytecode);
    void compile_expression_binary_or(MhdLangByteCode& bytecode);
    void compile_expression_binary_and(MhdLangByteCode& bytecode);
    void compile_expression_binary_eq_neq(MhdLangByteCode& bytecode);
    void compile_expression_binary_lt_lte_gt_gte(MhdLangByteCode& bytecode);
    void compile_expression_binary_add_sub(MhdLangByteCode& bytecode);
    void compile_expression_binary_mul_div_mod(MhdLangByteCode& bytecode);
private:
    void compile_expression_unary(MhdLangByteCode& bytecode);
    void compile_expression_unary_not(MhdLangByteCode& bytecode);
    void compile_expression_unary_plus_minus(MhdLangByteCode& bytecode);
private:
    void compile_expression_operand(MhdLangByteCode& bytecode);
private:
    void compile_expression_operand_primary(MhdLangByteCode& bytecode);
    void compile_expression_operand_primary_func();
    void compile_expression_operand_primary_list();
private:
    void compile_expression_operand_factor_call(MhdLangByteCode& bytecode);
    void compile_expression_operand_factor_index(MhdLangByteCode& bytecode);
    void compile_expression_operand_factor_subscript(MhdLangByteCode& bytecode);
private:
    const char* compile_operator();
private:
    void advance()
    {
        /// Peek a next token.
        if (!m_tokenizer.scan(m_token)) {
            throw MhdCompileError(m_token, m_token.m_value_str);
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
            throw MhdCompileError(m_token);
        }
    }
    template<typename... T>
    void expects(MhdScriptKind kind, T... kinds)
    {
        if (!matches(kind, kinds...)) {
            throw MhdCompileError(m_token);
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
