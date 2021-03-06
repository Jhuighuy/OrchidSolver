// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"
#include "OrchidScriptScanner.hpp"

#include <stdexcept>
#include <type_traits>
#include <cstring>
#include <string>
#include <vector>
#include <limits>
#include <cmath>

using MhdLangKind = MhdScriptKind;

#ifndef MHD_SCRIPT_SUGAR
#define MHD_SCRIPT_SUGAR 1
#endif

#ifndef MHD_SCRIPT_BITWISE
#define MHD_SCRIPT_BITWISE 0
#endif

#define MHD_LANG_KINDS \
    KIND(NONE,             ""                  ) \
    KIND(END,              "<end>"             ) \
    KIND(ID,               "<id>"              ) \
    KIND(CT_STR,           "<string-constant>" ) \
    KIND(CT_INT,           "<integer-constant>") \
    KIND(CT_DBL,           "<double-constant>" ) \
    KIND(KW_NULL,          "null"              ) \
    KIND(KW_TRUE,          "true"              ) \
    KIND(KW_FALSE,         "false"             ) \
    KIND(KW_IF,            "if"                ) \
    KIND(KW_ELSE,          "else"              ) \
    KIND(KW_SWITCH,        "switch"            ) \
    KIND(KW_CASE,          "case"              ) \
    KIND(KW_DEFAULT,       "default"           ) \
    KIND(KW_DO,            "do"                ) \
    KIND(KW_WHILE,         "while"             ) \
    KIND(KW_FOR,           "for"               ) \
    KIND(KW_FOREACH,       "foreach"           ) \
    KIND(KW_TRY,           "try"               ) \
    KIND(KW_CATCH,         "catch"             ) \
    KIND(KW_BREAK,         "break"             ) \
    KIND(KW_CONTINUE,      "continue"          ) \
    KIND(KW_RETURN,        "return"            ) \
    KIND(KW_THROW,         "throw"             ) \
    KIND(KW_OPERATOR,      "operator"          ) \
    KIND(KW_NAMESPACE,     "namespace"         ) \
    KIND(KW_FUNCTION,      "function"          ) \
    KIND(KW_STRUCT,        "struct"            ) \
    KIND(KW_CLASS,         "class"             ) \
    KIND(KW_LET,           "let"               ) \
    KIND(KW_NEW,           "new"               ) \
    KIND(KW_DELETE,        "delete"            ) \
    KIND(OP_DOT,           "."                 ) \
    KIND(OP_ELLIPSIS,      "..."               ) \
    KIND(OP_COLON,         ":"                 ) \
    KIND(OP_COMMA,         ","                 ) \
    KIND(OP_SEMICOLON,     ";"                 ) \
    KIND(OP_QUESTION,      "?"                 ) \
    KIND(OP_NOT,           "!"                 ) \
    KIND(OP_EQ,            "=="                ) \
    KIND(OP_NEQ,           "!="                ) \
    KIND(OP_LT,            "<"                 ) \
    KIND(OP_LTE,           "<="                ) \
    KIND(OP_GT,            ">"                 ) \
    KIND(OP_GTE,           ">="                ) \
    KIND(OP_ADD,           "+"                 ) \
    KIND(OP_SUB,           "-"                 ) \
    KIND(OP_MUL,           "*"                 ) \
    KIND(OP_DIV,           "/"                 ) \
    KIND(OP_MOD,           "%"                 ) \
    KIND(OP_ASG,           "="                 ) \
    KIND(OP_ADD_ASG,       "+="                ) \
    KIND(OP_SUB_ASG,       "-="                ) \
    KIND(OP_MUL_ASG,       "*="                ) \
    KIND(OP_DIV_ASG,       "/="                ) \
    KIND(OP_MUL_ASG,       "%="                ) \
    KIND(OP_INC,           "++"                ) \
    KIND(OP_DEC,           "--"                ) \
    KIND(OP_PAREN_OPEN,    "("                 ) \
    KIND(OP_PAREN_CLOSE,   ")"                 ) \
    KIND(OP_BRACE_OPEN,    "{"                 ) \
    KIND(OP_BRACE_CLOSE,   "}"                 ) \
    KIND(OP_BRACKET_OPEN,  "["                 ) \
    KIND(OP_BRACKET_CLOSE, "]"                 ) \

//########################################################################################################
//########################################################################################################
//########################################################################################################
#define MHD_LANG_OPCODES \
    /** Do nothing. */ \
    OP(NOOP) \
    /** Perform relative jumps. */ \
    OP(JUMP) \
    OP(JUMP_IF_TRUE) \
    OP(JUMP_IF_FALSE) \
    OP(RET) \
    /** Discard values on stack. */ \
    OP(DISCARD_1) \
    OP(DISCARD_2) \
    OP(DISCARD_3) \
    OP(DISCARD_4) \
    /** Duplicate values on top of the stack. */ \
    OP(DUP_1X1) \
    OP(DUP_2X1) \
    OP(DUP_3X1) \
    OP(DUP_NX1) \
    OP(DUP_1X2) \
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
    OP(LOAD_FP_0) \
    OP(LOAD_FP_1) \
    OP(LOAD_FPm1) \
    OP(LOAD_FP32) \
    OP(LOAD_FP64) \
    OP(LOAD_CSTR) \
    OP(LOAD_CSTR_EMPTY) \
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
    OP(STORE_INDEX_N) \
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
static MhdLangOpcode operator+(MhdLangOpcode op, std::uint32_t v) {
    return static_cast<MhdLangOpcode>(static_cast<std::uint32_t>(op) + v);
}
static_assert(static_cast<int>(MhdLangOpcode::EXTRA) <= 0xFF,
    "Opcode overflow.");
static const char* MhdLangOpcodeNames[] = {
#define OP(opcode) #opcode,
    MHD_LANG_OPCODES
#undef OP
};

//########################################################################################################
//########################################################################################################
//########################################################################################################

struct MhdLangByteCodeLabel
{
    std::vector<std::uint32_t*> references;
    std::uint32_t value = 0;
    MhdLangByteCodeLabel() {}
    ~MhdLangByteCodeLabel() {
        for (auto ref : references) {
            *ref = value;
        }
    }
};
struct MhdLangByteCode : public std::vector<std::uint8_t>
{
public:
    MhdLangByteCode() {
        reserve(1000000);
    }
public:
    void emit_code(MhdLangOpcode v) {
        emit_ui8(static_cast<std::uint8_t>(v));
    }
public:
    /// Emit various `JUMP*` instructions.
    void emit_jump(MhdLangOpcode op, MhdLangByteCodeLabel& label) {
        ORCHID_ASSERT(op == MhdLangOpcode::JUMP ||
                      op == MhdLangOpcode::JUMP_IF_TRUE ||
                      op == MhdLangOpcode::JUMP_IF_FALSE);
        emit_code(op);
        emit_addr(label);
    }
public:
    /// Emit various `LOAD*` instructions.
    void emit_load(MhdLangOpcode op) {
        ORCHID_ASSERT(op == MhdLangOpcode::LOAD_UI_0 ||
                      op == MhdLangOpcode::LOAD_UI_1 ||
                      op == MhdLangOpcode::LOAD_UI_2 ||
                      op == MhdLangOpcode::LOAD_UI_3 ||
                      op == MhdLangOpcode::LOAD_TRUE ||
                      op == MhdLangOpcode::LOAD_FALSE ||
                      op == MhdLangOpcode::LOAD_NULLPTR);
        emit_code(op);
    }
    void emit_load(MhdLangOpcode op, std::uint32_t value) {
        ORCHID_ASSERT(op == MhdLangOpcode::LOAD_UI32);
        if (value <= 3) {
            emit_code(MhdLangOpcode::LOAD_UI_0 + value);
            return;
        }
        if (value <= std::numeric_limits<std::uint8_t>::max()) {
            emit_code(MhdLangOpcode::LOAD_UI8);
            emit_ui8 (static_cast<std::uint8_t>(value));
            return;
        }
        if (value <= std::numeric_limits<std::uint16_t>::max()) {
            emit_code(MhdLangOpcode::LOAD_UI16);
            emit_ui16(static_cast<std::uint16_t>(value));
            return;
        }
        emit_code(MhdLangOpcode::LOAD_UI32);
        emit_ui32(value);
    }
    void emit_load(MhdLangOpcode op, double value) {
        ORCHID_ASSERT(op == MhdLangOpcode::LOAD_FP64);
        if (value == 0.0 && !std::signbit(value)) {
            emit_code(MhdLangOpcode::LOAD_FP_0);
            return;
        }
        const auto fvalue{ static_cast<float>(value) };
        if (fvalue == value) {
            emit_code(MhdLangOpcode::LOAD_FP32);
            emit_fp32(fvalue);
            return;
        }
        emit_code(MhdLangOpcode::LOAD_FP64);
        emit_fp64(value);
    }
    void emit_load(MhdLangOpcode op, const std::string& value) {
        ORCHID_ASSERT(op == MhdLangOpcode::LOAD_CSTR ||
                      op == MhdLangOpcode::LOAD_VAR_CSTR);
        if (value.empty()) {
            emit_code(MhdLangOpcode::LOAD_CSTR_EMPTY);
            return;
        }
        emit_code(op);
        emit_cstr(value.c_str());
    }
    void emit_load(MhdLangOpcode op, MhdLangByteCodeLabel& label, std::size_t num_args = 0) {
        ORCHID_ASSERT(op == MhdLangOpcode::LOAD_FUNC);
        emit_code(op);
        emit_addr(label);
    }
public:
    void emit_ui8 (std::uint8_t v) {
        push_back(v);
    }
    void emit_ui16(std::uint16_t v) {
        insert(end(), reinterpret_cast<std::uint8_t*>(&v),
                      reinterpret_cast<std::uint8_t*>(&v) + sizeof(v));
    }
    void emit_ui32(std::uint32_t v) {
        insert(end(), reinterpret_cast<std::uint8_t*>(&v),
                      reinterpret_cast<std::uint8_t*>(&v) + sizeof(v));
    }
public:
    void emit_fp32(float v) {
        insert(end(), reinterpret_cast<std::uint8_t*>(&v),
                      reinterpret_cast<std::uint8_t*>(&v) + sizeof(v));
    }
    void emit_fp64(double v) {
        insert(end(), reinterpret_cast<std::uint8_t*>(&v),
                      reinterpret_cast<std::uint8_t*>(&v) + sizeof(v));
    }
public:
    void emit_cstr(const char* v) {
        insert(end(), v, v + strlen(v) + 1);
    }
    void emit_addr(MhdLangByteCodeLabel& label) {
        label.references.push_back(reinterpret_cast<std::uint32_t*>(&back() + 1));
        emit_ui32(0);
    }
public:
    void label(MhdLangByteCodeLabel& label) {
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
private:
    void compile_expression_ternary(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_expression_or(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_expression_and(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_expression_eq_neq(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_expression_lt_lte_gt_gte(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_expression_add_sub(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_expression_mul_div_mod(MhdLangByteCode& bytecode, bool can_assign = false);
private:
    void compile_expression_unary(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_expression_unary_not(MhdLangByteCode& bytecode);
    void compile_expression_unary_plus_minus(MhdLangByteCode& bytecode);
private:
    void compile_operand(MhdLangByteCode& bytecode, bool can_assign = false);
private:
    void compile_operand_primary(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_operand_primary_list(MhdLangByteCode& bytecode);
    void compile_operand_primary_map(MhdLangByteCode& bytecode);
    void compile_operand_primary_func(MhdLangByteCode& bytecode);
private:
    void compile_factor_call(MhdLangByteCode& bytecode, bool index_call = false);
    void compile_factor_index(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_factor_indexdot(MhdLangByteCode& bytecode, bool can_assign = false);
    void compile_factor_index_end(MhdLangByteCode& bytecode, std::uint16_t num_indices = 1, bool can_assign = false);
private:
    const char* compile_operator();
private:
    void advance() {
        /// Peek a next token.
        if (!m_tokenizer.scan(m_token)) {
            throw MhdCompileError(m_token, m_token.m_value_str);
        }
    }
private:
    bool matches(MhdScriptKind kind) const {
        return m_token.m_kind == kind;
    }
    template<typename... T>
    bool matches(MhdScriptKind kind, T... kinds) const {
        return matches(kind) || matches(kinds...);
    }
private:
    template<typename... T>
    bool matched(MhdScriptKind kind, T... kinds) {
        if (matches(kind, kinds...)) {
            advance();
            return true;
        }
        return false;
    }
private:
    void unexpected() {
        throw MhdCompileError(m_token, "unexpected token");
    }
    template<typename... T>
    void expect(MhdScriptKind kind, T... kinds) {
        if (!matched(kind, kinds...)) {
            unexpected();
        }
    }
    template<typename... T>
    void expects(MhdScriptKind kind, T... kinds) {
        if (!matches(kind, kinds...)) {
            unexpected();
        }
    }
};	// struct MhdScriptParser
//########################################################################################################
//########################################################################################################
//########################################################################################################
