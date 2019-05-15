// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"

#include <string>

//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptToken
{
public:
    MhdScriptKind m_kind;
    union {
        unsigned int     m_value_uint;
        int     m_value_int;
        double  m_value_dbl;
    };
    std::string m_value_str;
    const char* m_loc_file;
    int         m_loc_line;
    int         m_loc_column;
public:
    MhdScriptToken()
        : m_kind(MhdScriptKind::NONE)
        , m_value_dbl(0.0)
        , m_loc_file("<unknown>"), m_loc_line(1), m_loc_column(0) { }
public:
    void clear()
    {
        m_kind = MhdScriptKind::NONE;
        m_value_int = 0;
        m_value_dbl = 0.0;
        m_value_str.clear();
    }
};	// struct MhdScriptToken
//--------------------------------------------------------------------------------------------------------
struct MhdScriptTokenizer
{
public:
    const char* m_text;
    std::size_t m_text_peeked;
public:
    MhdScriptTokenizer(const char* text)
        : m_text(text), m_text_peeked(0) {}
public:
    MHD_INTERFACE
    bool scan(MhdScriptToken& token);
private:
    bool scan_id(MhdScriptToken& token);
    bool scan_str(MhdScriptToken& token);
    bool scan_num(MhdScriptToken& token);
private:
    void advance(MhdScriptToken& token);
    char peek()
    {
        return m_text[m_text_peeked = 0];
    }
    char peek_next()
    {
        return m_text[m_text_peeked] == '\0' ? '\0' :
               m_text[m_text_peeked += 1];
    }
private:
    bool matches_next(char c)
    {
        return peek_next() == c;
    }
    template<typename T>
    bool matches_next(const T& func)
    {
        return func(peek_next());
    }
    template<typename T, typename... Ts>
    bool matches_next(const T& func, const Ts&... funcs)
    {
        return matches_next(func) && matches_next(funcs...);
    }
    bool matches(char c)
    {
        return peek() == c;
    }
    template<typename T>
    bool matches(const T& func)
    {
        return func(peek());
    }
    template<typename T, typename... Ts>
    bool matches(const T& func, const Ts&... funcs)
    {
        return matches(func) && matches_next(funcs...);
    }
public:
    template<typename... Ts>
    bool matched(MhdScriptToken& token, const Ts&... funcs)
    {
        if (matches(funcs...)) {
            advance(token);
            return true;
        }
        return false;
    }
    template<typename... Ts>
    bool make(MhdScriptToken& token, MhdScriptKind kind, const Ts&... funcs)
    {
        if (matched(token, funcs...)) {
            token.m_kind = kind;
            return true;
        }
        return false;
    }
};	// struct MhdTokenizer
//########################################################################################################
//########################################################################################################
//########################################################################################################
