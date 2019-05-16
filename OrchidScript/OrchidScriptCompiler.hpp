// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"
#include "OrchidScriptScanner.hpp"

#include <stdexcept>
#include <cstring>
#include <string>
#include <vector>

#ifndef MHD_SCRIPT_SUGAR
#define MHD_SCRIPT_SUGAR 1
#endif

#ifndef MHD_SCRIPT_BITWISE
#define MHD_SCRIPT_BITWISE 0
#endif

//########################################################################################################
//########################################################################################################
//########################################################################################################
#define MHD_LANG_OPCODES \
    /** Do nothing. */ \
    OP(NOOP) \
    /** Perform relative jumps. */ \
    OP(JUMP) \
    OP(JUMP_Z) \
    OP(JUMP_NZ) \
    OP(RET) \
    /** Discard values on stack. */ \
    OP(DISCARD_1) \
    OP(DISCARD_2) \
    OP(DISCARD_3) \
    OP(DISCARD_4) \
    /** Duplicate values on top of the stack. */ \
    OP(DUP_1X1) \
    OP(DUP_1X2) \
    OP(DUP_2X1) \
    OP(DUP_2X2) \
    /** Load constants onto stack. */ \
    OP(LOAD_TRUE) \
    OP(LOAD_FALSE) \
    OP(LOAD_NULLPTR) \
    OP(LOAD_UI_0) \
    OP(LOAD_UI_1) \
    OP(LOAD_UI_2) \
    OP(LOAD_UI_3) \
    OP(LOAD_UI8) \
    OP(LOAD_UI16) \
    OP(LOAD_UI32) \
    OP(LOAD_FP32) \
    OP(LOAD_FP64) \
    OP(LOAD_CSTR) \
    OP(LOAD_LIST) \
    OP(LOAD_MAP) \
    OP(LOAD_FUNC) \
    OP(LOAD_REF) \
    OP(LOAD_VAR_CSTR) \
    /** Calls and Indexing. */ \
    OP(CALL_0) \
    OP(CALL_1) \
    OP(CALL_2) \
    OP(CALL_3) \
    OP(CALL_N) \
    OP(INDEX_1) \
    OP(INDEX_2) \
    OP(INDEX_3) \
    OP(INDEX_N) \
    OP(INDEXCALL_0) \
    OP(INDEXCALL_1) \
    OP(INDEXCALL_2) \
    OP(INDEXCALL_3) \
    OP(INDEXCALL_N) \
    OP(INDEX_CALL_0) \
    OP(INDEX_CALL_1) \
    OP(INDEX_CALL_2) \
    OP(INDEX_CALL_3) \
    OP(INDEX_CALL_N) \
    /** Store variables. */ \
    OP(STORE_VAR_CSTR) \
    OP(STORE_INDEX_1) \
    OP(STORE_INDEX_2) \
    OP(STORE_INDEX_3) \
    OP(STORE_INDEX_4) \
    /** Reference variables to separate stack. */ \
    OP(REF_CSTR) \
    OP(REF_DISCARD) \
    /** Operators. */ \
    OP(OP_NOT) \
    OP(OP_PLUS) \
    OP(OP_MINUS) \
    OP(OP_OR) \
    OP(OP_AND) \
    OP(OP_EQ) \
    OP(OP_NEQ) \
    OP(OP_LT) \
    OP(OP_GT) \
    OP(OP_LTE) \
    OP(OP_GTE) \
    OP(OP_ADD) \
    OP(OP_SUB) \
    OP(OP_MUL) \
    OP(OP_DIV) \
    OP(OP_MOD) \
    OP(OP_ADD_ASG) \
    OP(OP_SUB_ASG) \
    OP(OP_MUL_ASG) \
    OP(OP_DIV_ASG) \
    OP(OP_MOD_ASG) \
    /** Assignment operators, L-value is assumed to be on reference stack. */ \
    OP(OPREF_ASG) \
    OP(OPREF_ADD_ASG) \
    OP(OPREF_SUB_ASG) \
    OP(OPREF_MUL_ASG) \
    OP(OPREF_DIV_ASG) \
    OP(OPREF_MOD_ASG) \
    /** Debug opcodes. */ \
    OP(DEBUG_PRINT) \
    /** Extra opcodes. */ \
    OP(EXTRA)

enum struct MhdLangOpcode : unsigned char
{
#define OP(opcode) opcode,
    MHD_LANG_OPCODES
#undef OP
};  // enum struct MhdLangOpcode
static_assert(static_cast<int>(MhdLangOpcode::EXTRA) <= 0xFF,
    "Opcode overflow.");

//########################################################################################################
//########################################################################################################
//########################################################################################################

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
public:
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
    void compile_expression_decl_var(MhdLangByteCode& bytecode);
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
    void compile_expression_comma(MhdLangByteCode& bytecode);
private:
    void compile_expression_binary_asg(MhdLangByteCode& bytecode);
    bool compile_expression_binary_asg_end(MhdLangByteCode& bytecode, ...);
private:
    void compile_expression_ternary(MhdLangByteCode& bytecode, bool compile_lhs = true);
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
    bool compile_expression_operand(MhdLangByteCode& bytecode, bool asg = true);
private:
    bool compile_expression_operand_primary(MhdLangByteCode& bytecode, bool parse_assignment = false);
    void compile_expression_operand_primary_list(MhdLangByteCode& bytecode);
    void compile_expression_operand_primary_map(MhdLangByteCode& bytecode);
    void compile_expression_operand_primary_func(MhdLangByteCode& bytecode);
private:
    void compile_expression_operand_factor_call(MhdLangByteCode& bytecode);
    bool compile_expression_operand_factor_index(MhdLangByteCode& bytecode, bool assignment);
    bool compile_expression_operand_factor_subscript(MhdLangByteCode& bytecode, bool assignment);
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
    bool matches_op_asg() const
    {
        return matches(MhdLangKind::OP_ADD_ASG, MhdLangKind::OP_SUB_ASG,
                       MhdLangKind::OP_MUL_ASG, MhdLangKind::OP_DIV_ASG,
                       MhdLangKind::OP_MOD_ASG);
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
        return false;
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
