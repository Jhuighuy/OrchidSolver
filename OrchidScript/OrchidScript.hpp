// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#define ORCHID_ASSERT(...) do { if(!(__VA_ARGS__)){ \
    fprintf(stderr, "%s(%d): %s\n", __FILE__, __LINE__, #__VA_ARGS__); \
    abort(); } } while (false)
#define MHD_INTERNAL
#define MHD_INTERFACE
#define ORCHID_INTERFACE
//########################################################################################################
//########################################################################################################
//########################################################################################################
enum struct MhdScriptKind : char
{
    NONE,
    ERR,
    ID,
    CT_STR,
    CT_INT,
    CT_DBL,
    CT_LGC,
    CT_NIL,
    KW_TRUE,
    KW_FALSE,
    KW_IF,
    KW_ELSE,
    KW_SWITCH,
    KW_CASE,
    KW_DEFAULT,
    KW_WHILE,
    KW_DO,
    KW_FOR,
    KW_TRY,
    KW_CATCH,
    KW_BREAK,
    KW_CONTINUE,
    KW_RETURN,
    KW_THROW,
    KW_OPERATOR,
    KW_NAMESPACE,
    KW_LET,
    OP_DOT,
    OP_COMMA,
    OP_COLON,
    OP_QUESTION,
    OP_SEMICOLON,
    OP_ASG,
    OP_INC,
    OP_DEC,
    OP_ADD,
    OP_ADD_ASG,
    OP_SUB,
    OP_SUB_ASG,
    OP_MUL,
    OP_MUL_ASG,
    OP_DIV,
    OP_DIV_ASG,
    OP_MOD,
    OP_MOD_ASG,
    OP_POW,
    OP_POW_ASG,
    OP_NOT,
    OP_NOT_BW,
    OP_EQ,
    OP_NEQ,
    OP_LT,
    OP_LTE,
    OP_GT,
    OP_GTE,
    OP_LSHIFT,
    OP_LSHIFT_ASG,
    OP_RSHIFT,
    OP_RSHIFT_ASG,
    OP_AND,
    OP_AND_BW,
    OP_AND_BW_ASG,
    OP_OR,
    OP_OR_BW,
    OP_OR_BW_ASG,
    OP_XOR_BW,
    OP_XOR_BW_ASG,
    OP_PAREN_OPEN,
    OP_PAREN_CLOSE,
    OP_BRACE_OPEN,
    OP_BRACE_CLOSE,
    OP_BRACKET_OPEN,
    OP_BRACKET_CLOSE,
};	// enum struct MhdScriptKind
//########################################################################################################
//########################################################################################################
//########################################################################################################
