// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once
#include "OrchidScript.hpp"

#include <cinttypes>
#include <string>

//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptToken
{
public:
    MhdScriptKind m_kind;
    std::int32_t m_value_int;
    std::string  m_value_str;
    double       m_value_dbl;
    const char*  m_loc_file;
    int          m_loc_line;
    int          m_loc_column;
public:
    /** Initialize an empty token. */
    MhdScriptToken()
        : m_kind(MhdScriptKind::NONE)
        , m_value_int(0), m_value_dbl(0.0)
        , m_loc_file(""), m_loc_line(1), m_loc_column(1) { }
public:
    bool operator==(MhdScriptKind kind) const
    {
        return m_kind == kind;
    }
    bool operator!=(MhdScriptKind kind) const
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
    MhdTokenizer(const char* text)
        : m_text(text)
        , m_text_peeked(0) { }
public:
    MHD_INTERFACE
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
