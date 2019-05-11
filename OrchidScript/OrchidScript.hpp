// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include <cstdio>

#define ORCHID_ASSERT(...) \
    do { \
        if(!(__VA_ARGS__)) { \
            fprintf(stderr, "%s(%d): %s\n", __FILE__, __LINE__, #__VA_ARGS__); \
            abort(); \
        } \
    } while (false)
#define MHD_INTERNAL
#define MHD_INTERFACE
#define ORCHID_INTERFACE
//########################################################################################################
//########################################################################################################
//########################################################################################################
enum struct MhdScriptKind : char
{
    NONE,
    END,
    ERR,
    ID,
    CT_STR,
    CT_INT,
    CT_DBL,
    KW_NULL,
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
    KW_FOREACH,
    KW_TRY,
    KW_CATCH,
    KW_BREAK,
    KW_CONTINUE,
    KW_RETURN,
    KW_THROW,
    KW_OPERATOR,
    KW_NAMESPACE,
    KW_FUNCTION,
    KW_STRUCT,
    KW_CLASS,
    KW_LET,
    KW_NEW,
    KW_DELETE,
    OP_DOT,
    OP_COMMA,
    OP_COLON,
    OP_SEMICOLON,
    OP_QUESTION,
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
//--------------------------------------------------------------------------------------------------------
namespace MhdScriptOp {
static const char* const OP_INC = "operator++";
static const char* const OP_DEC = "operator--";
static const char* const OP_ADD = "operator+";
static const char* const OP_SUB = "operator-";
static const char* const OP_MUL = "operator*";
static const char* const OP_DIV = "operator/";
static const char* const OP_MOD = "operator%";
static const char* const OP_ADD_ASG = "operator+=";
static const char* const OP_SUB_ASG = "operator-=";
static const char* const OP_MUL_ASG = "operator*=";
static const char* const OP_DIV_ASG = "operator/=";
static const char* const OP_MOD_ASG = "operator%=";
static const char* const OP_NOT = "operator!";
static const char* const OP_NOT_BW = "operator~";
static const char* const OP_EQ = "operator==";
static const char* const OP_NEQ = "operator!=";
static const char* const OP_LT = "operator<";
static const char* const OP_LTE = "operator<=";
static const char* const OP_GT = "operator>";
static const char* const OP_GTE = "operator>=";
static const char* const OP_LSHIFT = "operator<<";
static const char* const OP_LSHIFT_ASG = "operator<<=";
static const char* const OP_RSHIFT = "operator>>";
static const char* const OP_RSHIFT_ASG = "operator>>=";
static const char* const OP_AND = "operator&&";
static const char* const OP_AND_BW = "operator&";
static const char* const OP_AND_BW_ASG = "operator&=";
static const char* const OP_OR = "operator||";
static const char* const OP_OR_BW = "operator|";
static const char* const OP_OR_BW_ASG = "operator|=";
static const char* const OP_XOR_BW = "operator^";
static const char* const OP_XOR_BW_ASG = "operator^=";
static const char* const OP_CALL = "operator()";
static const char* const OP_INDEX = "operator[]";
static const char* const OP_PLUS = "operator{+}";
static const char* const OP_MINUS = "operator{-}";
static const char* const OP_INC_POSTFIX = "operator{++}";
static const char* const OP_DEC_POSTFIX = "operator{--}";
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
