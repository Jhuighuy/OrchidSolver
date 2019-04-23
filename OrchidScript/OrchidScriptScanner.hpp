// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once
#include <cinttypes>
#include <string>


#define ORCHID_ASSERT(...) do { if(!(__VA_ARGS__)){ \
    fprintf(stderr, "%s(%d): %s\n", __FILE__, __LINE__, #__VA_ARGS__); \
    abort(); } } while (false)
#define MHD_INTERNAL
#define MHD_INTERFACE
#define ORCHID_INTERFACE

//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptToken
{
    enum class Kind {
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
        KW_NAMESPACE,
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
    };	// enum class Kind
public:
    Kind m_kind;
    std::int32_t m_value_int;
    std::string  m_value_str;
    double       m_value_dbl;
public:
    /** Initialize an empty token. */
    MhdScriptToken()
        : m_kind(MhdScriptToken::Kind::NONE)
        , m_value_int(0), m_value_dbl(0.0) { }
public:
    bool operator==(MhdScriptToken::Kind kind) const
    {
        return m_kind == kind;
    }
    bool operator!=(MhdScriptToken::Kind kind) const
    {
        return m_kind != kind;
    }
};	// struct MhdScriptToken
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdTokenizer
{
public:
    const char* m_text;
    std::size_t m_text_peeked;
public:
    /** Initialize a tokenizer. */
    MhdTokenizer(const char* text)
        : m_text(text)
        , m_text_peeked(0) { }
public:
    bool scan(MhdScriptToken& token);
private:
    bool scan_str(MhdScriptToken& token);
    bool scan_num(MhdScriptToken& token);
    bool scan_id(MhdScriptToken& token);
private:
    char peek()
    {
        return m_text[m_text_peeked = 0];
    }
    char peek_next()
    {
        return m_text[m_text_peeked += 1];
    }
    void advance()
    {
        m_text += m_text_peeked + 1;
        m_text_peeked = 0;
    }
};	// struct MhdTokenizer
//########################################################################################################
//########################################################################################################
//########################################################################################################
