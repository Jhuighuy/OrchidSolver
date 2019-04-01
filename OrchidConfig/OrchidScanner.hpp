// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once
#include <cinttypes>
#include <string>

//########################################################################################################
//########################################################################################################
//########################################################################################################
/** Token. */
struct MhdToken
{
    enum class Kind
    {
        NONE,
        ERR,
        ID,
        CT_STR,
        CT_INT,
        CT_DBL,
        CT_LGC,
        KW_TRUE,
        KW_FALSE,
        OP_ASG,
        OP_ADD,
        OP_SUB,
        OP_MUL,
        OP_DIV,
        OP_MOD,
        OP_NOT,
        OP_EQ,
        OP_NEQ,
        OP_LT,
        OP_LTE,
        OP_GT,
        OP_GTE,
        OP_AND,
        OP_AND_BW,
        OP_OR,
        OP_OR_BW,
        OP_XOR,
        OP_XOR_BW,
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
    MhdToken()
        : m_kind(MhdToken::Kind::NONE)
        , m_value_int(0), m_value_dbl(0.0) { }
public:
    bool operator==(MhdToken::Kind kind) const
    {
        return m_kind == kind;
    }
    bool operator!=(MhdToken::Kind kind) const
    {
        return m_kind != kind;
    }
};	// struct MhdToken
//########################################################################################################
//########################################################################################################
//########################################################################################################
/** Token scanner. */
struct MhdTokenizer
{
private:
    const char* m_text;
    std::size_t m_text_peeked;
public:
    /** Initialize a tokenizer. */
    MhdTokenizer(const char* text)
        : m_text(text)
        , m_text_peeked(0) { }
public:
    bool scan(MhdToken& token);
private:
    bool scan_str(MhdToken& token);
    bool scan_num(MhdToken& token);
    bool scan_id(MhdToken& token);
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
