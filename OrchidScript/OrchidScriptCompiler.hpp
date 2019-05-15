// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"
#include "OrchidScriptScanner.hpp"

#include <stdexcept>
#include <string>
#include <vector>

#ifndef MHD_SCRIPT_SUGAR
#define MHD_SCRIPT_SUGAR 1
#endif

#ifndef MHD_SCRIPT_BITWISE
#define MHD_SCRIPT_BITWISE 0
#endif

enum struct MhdLangOpcode : unsigned char
{
    /** Do nothing. */
    NOOP = 0x00,
    /** Discard values on stack. */
    DISCARD_1,
    DISCARD_2,
    DISCARD_3,
    DISCARD_4,
    /** Load constants onto stack. */
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
    LOAD_LIST,
    LOAD_MAP,
    /** Duplicate values on top of the stack. */
    DUP_1X1,
    DUP_1X2,
    DUP_2X1,
    DUP_2X2,
    /** Perform relative jumps. */
    JUMP,
    JUMP_Z,
    JUMP_NZ,
    /** Calls and Indexing. */
    CALL_0,
    CALL_1,
    CALL_2,
    CALL_3,
    CALL_N,
    INDEX_1,
    INDEX_2,
    INDEX_3,
    INDEX_N,
    INDEX_ASG_1,
    INDEX_ASG_2,
    INDEX_ASG_3,
    INDEX_ASG_N,
    INDEX_CALL_1_0,
    INDEX_CALL_1_1,
    INDEX_CALL_1_2,
    INDEX_CALL_1_3,
    INDEX_CALL_1_N,
    INDEX_CALL_2_0,
    INDEX_CALL_2_1,
    INDEX_CALL_2_2,
    INDEX_CALL_2_3,
    INDEX_CALL_2_N,
    INDEX_CALL_M_N,
    /** Reference variables to separate stack. */
    REF_CSTR,
    /** Operators. */
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
    /** Assignment operators, L-value is assumed to be on reference stack. */
    OPREF_ASG,
    OPREF_ADD_ASG,
    OPREF_SUB_ASG,
    OPREF_MUL_ASG,
    OPREF_DIV_ASG,
    OPREF_MOD_ASG,
    /** Extra opcodes. */
    OP_EXTRA,
};  // enum struct MhdLangOpcode
static_assert(static_cast<int>(MhdLangOpcode::OP_EXTRA) <= 0xFF,
    "Opcode overflow.");

struct MhdLangByteCodeLabel
{
    std::vector<std::uint32_t*> references;
    std::uint32_t value = 0;
    MhdLangByteCodeLabel() {}
    ~MhdLangByteCodeLabel()
    {
        for (auto ref : references) {
            *ref = value;
        }
    }
};
struct MhdLangByteCode : public std::vector<std::uint8_t>
{
    MhdLangByteCode()
    {
        reserve(1000000);
    }
    void emit_ui8 (std::uint8_t v) 
    {
        this->push_back(v);
    }
    void emit_ui16(std::uint16_t v) 
    {
        insert(end(), reinterpret_cast<std::uint8_t*>(&v),
                      reinterpret_cast<std::uint8_t*>(&v) + sizeof(v));
    }
    void emit_ui32(std::uint32_t v) 
    {
        insert(end(), reinterpret_cast<std::uint8_t*>(&v),
                      reinterpret_cast<std::uint8_t*>(&v) + sizeof(v));
    }
    void emit_fp32(float v) 
    {
        insert(end(), reinterpret_cast<std::uint8_t*>(&v),
                      reinterpret_cast<std::uint8_t*>(&v) + sizeof(v));
    }
    void emit_fp64(double v) 
    {
        insert(end(), reinterpret_cast<std::uint8_t*>(&v),
                      reinterpret_cast<std::uint8_t*>(&v) + sizeof(v));
    }
    void emit_cstr(const char* v) 
    {
        insert(end(), v, v + strlen(v) + 1);
    }
    void emit_code(MhdLangOpcode v) 
    {
        emit_ui8(static_cast<std::uint8_t>(v));
    }
    void emit_addr(MhdLangByteCodeLabel& label) 
    {
        label.references.push_back(reinterpret_cast<std::uint32_t*>(&back() + 1));
        emit_ui32(0);
    }
public:
    void label(MhdLangByteCodeLabel& label)
    {
        label.value = size();
    }
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
    void compile_wrap(MhdLangByteCode& bytecode);
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
    void compile_expression_operand_primary_list(MhdLangByteCode& bytecode);
    void compile_expression_operand_primary_map(MhdLangByteCode& bytecode);
    void compile_expression_operand_primary_func(MhdLangByteCode& bytecode);
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
private:
    bool matches(MhdScriptKind kind) const
    {
        return m_token.m_kind == kind;
    }
    template<typename... T>
    bool matches(MhdScriptKind kind, T... kinds) const
    {
        return matches(kind) || matches(kinds...);
    }
private:
    template<typename... T>
    bool matched(MhdScriptKind kind, T... kinds)
    {
        /// Peek a next token on a match.
        if (matches(kind, kinds...)) {
            advance();
            return true;
        }
        return false;;
    }
private:
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
