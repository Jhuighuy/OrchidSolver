// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptScanner.hpp"

#include <cstdlib>
#include <cctype>
#include <unordered_map>

namespace std {
    static inline 
    int isalpha_(int c)
    {
        return c == '_' || isalpha(c);
    }
    static inline
    int isalnum_(int c)
    {
        return c == '_' || isalnum(c);
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE 
bool MhdScriptTokenizer::scan(MhdScriptToken& token)
{
    /// Scan a token.
    token.clear();
    char character;
    /* Skip a trivia. */
    for (;;) {
        if (character = peek(), std::isspace(character)) {
            /* Skip spaces. */
            advance(token);
            while (std::isspace(peek())) {
                advance(token);
            }
        } else if (character == '/') {
            if (character = peek_next(),
                character == '/') {
                /* Skip single-line comment. */
                advance(token);
                while (peek() != '\n') {
                    advance(token);
                }
                advance(token);
            } else if (character == '*') {
                /* Skip multi-line comment. */
                advance(token);
                while (peek() != '*' || peek_next() != '/') {
                    peek();
                    advance(token);
                }
                advance(token);
            } else {
                break;
            }
        } else {
            break;
        }
    }
    /* Scan a token. */
    character = peek();
    switch (character) {
        /* Scan end of stream. */
        case '\0':
            token.m_kind = MhdScriptKind::END;
            return true;
        /* Scan a string constant. */
        case '\'':
        case '\"':
            return scan_str(token);
        /* Scan a numeric constant. */
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            return scan_num(token);
        /* Scan an identifier or keyword. */
        case '_': case '$':
        case 'A': case 'a':
        case 'B': case 'b':
        case 'C': case 'c':
        case 'D': case 'd':
        case 'E': case 'e':
        case 'F': case 'f':
        case 'G': case 'g':
        case 'H': case 'h':
        case 'I': case 'i':
        case 'J': case 'j':
        case 'K': case 'k':
        case 'L': case 'l':
        case 'M': case 'm':
        case 'N': case 'n':
        case 'O': case 'o':
        case 'P': case 'p':
        case 'Q': case 'q':
        case 'R': case 'r':
        case 'S': case 's':
        case 'T': case 't':
        case 'U': case 'u':
        case 'V': case 'v':
        case 'W': case 'w':
        case 'X': case 'x':
        case 'Y': case 'y':
        case 'Z': case 'z':
            return scan_id(token);
        /* Scan operators. */
        case '.':
            advance(token);
            token.m_kind = MhdScriptKind::OP_DOT;
            return true;
        case ',':
            advance(token);
            token.m_kind = MhdScriptKind::OP_COMMA;
            return true;
        case ':':
            advance(token);
            token.m_kind = MhdScriptKind::OP_COLON;
            return true;
        case ';':
            advance(token);
            token.m_kind = MhdScriptKind::OP_SEMICOLON;
            return true;
        case '?':
            advance(token);
            token.m_kind = MhdScriptKind::OP_QUESTION;
            return true;
        case '=':
            advance(token);
            if (peek() == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_EQ;
            } else {
                token.m_kind = MhdScriptKind::OP_ASG;
            }
            return true;
        case '+':
            advance(token);
            if (character = peek(),
                character == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_ADD_ASG;
            } else if (character == '+') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_INC;
            } else {
                token.m_kind = MhdScriptKind::OP_ADD;
            }
            return true;
        case '-':
            advance(token);
            if (character = peek(),
                character == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_SUB_ASG;
            } else if (character == '-') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_DEC;
            } else {
                token.m_kind = MhdScriptKind::OP_SUB;
            }
            return true;
        case '*':
            advance(token);
            if (peek() == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_MUL_ASG;
            } else {
                token.m_kind = MhdScriptKind::OP_MUL;
            }
            return true;
        case '/':
            advance(token);
            if (peek() == '=') {
                token.m_kind = MhdScriptKind::OP_DIV_ASG;
            } else {
                token.m_kind = MhdScriptKind::OP_DIV;
            }
            return true;
        case '%':
            advance(token);
            if (peek() == '=') {
                token.m_kind = MhdScriptKind::OP_MOD_ASG;
            } else {
                token.m_kind = MhdScriptKind::OP_MOD;
            }
            return true;
        case '!':
            advance(token);
            if (peek() == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_NEQ;
            } else {
                token.m_kind = MhdScriptKind::OP_NOT;
            }
            return true;
        case '~':
            advance(token);
            token.m_kind = MhdScriptKind::OP_NOT_BW;
            return true;
        case '<':
            advance(token);
            if (character = peek(),
                character == '<') {
                advance(token);
                if (peek() == '=') {
                    advance(token);
                    token.m_kind = MhdScriptKind::OP_LSHIFT_ASG;
                } else {
                    token.m_kind = MhdScriptKind::OP_LSHIFT;
                }
            } else if (character == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_LTE;
            } else {
                token.m_kind = MhdScriptKind::OP_LT;
            }
            return true;
        case '>':
            advance(token);
            if (character = peek(),
                character == '>') {
                advance(token);
                if (peek() == '=') {
                    advance(token);
                    token.m_kind = MhdScriptKind::OP_RSHIFT_ASG;
                } else {
                    token.m_kind = MhdScriptKind::OP_RSHIFT;
                }
            } else if (character == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_GTE;
            } else {
                token.m_kind = MhdScriptKind::OP_GT;
            }
            return true;
        case '&':
            advance(token);
            if (character = peek(),
                character == '&') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_AND;
            } else if (character == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_AND_BW_ASG;
            } else {
                token.m_kind = MhdScriptKind::OP_AND_BW;
            }
            return true;
        case '|':
            advance(token);
            if (character = peek(),
                character == '|') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_OR;
            } else if (character == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_OR_BW_ASG;
            } else {
                token.m_kind = MhdScriptKind::OP_OR_BW;
            }
            return true;
        case '^':
            advance(token);
            if (peek() == '=') {
                advance(token);
                token.m_kind = MhdScriptKind::OP_XOR_BW_ASG;
            } else {
                token.m_kind = MhdScriptKind::OP_XOR_BW;
            }
            return true;
        case '(':
            advance(token);
            token.m_kind = MhdScriptKind::OP_PAREN_OPEN;
            return true;
        case ')':
            advance(token);
            token.m_kind = MhdScriptKind::OP_PAREN_CLOSE;
            return true;
        case '{':
            advance(token);
            token.m_kind = MhdScriptKind::OP_BRACE_OPEN;
            return true;
        case '}':
            advance(token);
            token.m_kind = MhdScriptKind::OP_BRACE_CLOSE;
            return true;
        case '[':
            advance(token);
            token.m_kind = MhdScriptKind::OP_BRACKET_OPEN;
            return true;
        case ']':
            advance(token);
            token.m_kind = MhdScriptKind::OP_BRACKET_CLOSE;
            return true;
        default:
            token.m_kind = MhdScriptKind::ERR;
            token.m_value_str = "Invalid symbol in the input stream.";
            return false;
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL 
bool 
MhdScriptTokenizer::scan_str(MhdScriptToken& token)
{
    /// Scan a single string token.
    const char end_character = peek();
    advance(token);
    char character;
    while (character = peek(),
           character != end_character) {
        /** @todo Scan escapes! */
        advance(token);
        token.m_value_str.push_back(character);
    } 
    advance(token);
    token.m_kind = MhdScriptKind::CT_STR;
    return true;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL 
bool 
MhdScriptTokenizer::scan_num(MhdScriptToken& token)
{
    /// Scan a single numeric token.
    bool can_be_hex = false;
    bool can_be_oct = false;
    bool has_frc = false;
    bool has_exp = false;
    /* Determine what possible radix is
     * this numeric literal is using. */
    char character = peek();
    if (character == '0') {
        character = peek_next();
        if (character == 'x' || character == 'X') {
            advance(token);
            can_be_hex = true;
        } else {
            can_be_oct = true;
        }
    }
    if (can_be_hex) {
        /* Parse a hexadecimal literal. */
        while (character = peek(), std::isxdigit(character)) {
            advance(token);
            token.m_value_str.push_back(character);
        }
    } else {
        /* Parse a decimal, octal of floating point literal. */
        bool has_oct_chars = true;
        while (character = peek(), std::isdigit(character)) {
            advance(token);
            token.m_value_str.push_back(character);
            has_oct_chars &= character <= '7';
        }
        /* Parse fraction. */
        character = peek();
        if (character == '.') {
            advance(token);
            token.m_value_str.push_back(character);
            if (character = peek(), std::isdigit(character)) {
                advance(token);
                token.m_value_str.push_back(character);
                while (character = peek(), std::isdigit(character)) {
                    advance(token);
                    token.m_value_str.push_back(character);
                }
            } else {
                token.m_kind = MhdScriptKind::ERR;
                token.m_value_str = "Invalid numeric literal fraction.";
                return false;
            }
            has_frc = true;
        }
        /* Parse exponent. */
        character = peek();
        if (character == 'e' || character == 'E' ||
            character == 'd' || character == 'D') {
            advance(token);
            token.m_value_str.push_back('e');
            if ((character = peek()) == '+' || character == '-') {
                advance(token);
                token.m_value_str.push_back(character);
            }
            if (character = peek(), std::isdigit(character)) {
                advance(token);
                token.m_value_str.push_back(character);
                while (character = peek(), std::isdigit(character)) {
                    advance(token);
                    token.m_value_str.push_back(character);
                }
            } else {
                token.m_kind = MhdScriptKind::ERR;
                token.m_value_str = "Invalid numeric literal exponent.";
                return false;
            }
            has_exp = true;
        }
        can_be_oct &= !has_frc && !has_exp;
        if (can_be_oct && !has_oct_chars) {
            token.m_kind = MhdScriptKind::ERR;
            token.m_value_str = "Invalid octal number.";
            return false;
        }
    }
    if (has_frc || has_exp) {
        char* end_ptr = nullptr;
        token.m_kind = MhdScriptKind::CT_DBL;
        token.m_value_dbl = std::strtod(token.m_value_str.c_str(), &end_ptr);
        if (token.m_value_str.c_str() == end_ptr) {
            token.m_kind = MhdScriptKind::ERR;
            token.m_value_str = "Floating point overflow.";
            return false;
        }
    } else {
        char* end_ptr = nullptr;
        token.m_kind = MhdScriptKind::CT_INT;
        token.m_value_int = std::strtol(token.m_value_str.c_str(), &end_ptr,
                                        can_be_hex ? 16 : can_be_oct ? 8 : 10);
        if (token.m_value_str.c_str() == end_ptr) {
            token.m_kind = MhdScriptKind::ERR;
            token.m_value_str = "Integer overflow.";
            return false;
        }
    }
    return true;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL 
bool 
MhdScriptTokenizer::scan_id(MhdScriptToken& token)
{
    /// Scan an idenifier or a keyword.
    char character = peek();
    while (character = peek(), std::isalnum(character) ||
           character == '_' || character == '$') {
        advance(token);
        token.m_value_str.push_back(character);
    }
    static const std::unordered_map<std::string, MhdScriptKind> keywords{
        { "null",      MhdScriptKind::KW_NULL      },
        { "true",      MhdScriptKind::KW_TRUE      },
        { "false",     MhdScriptKind::KW_FALSE     },
        { "if",        MhdScriptKind::KW_IF        },
        { "else",      MhdScriptKind::KW_ELSE      },
        { "switch",    MhdScriptKind::KW_SWITCH    },
        { "case",      MhdScriptKind::KW_CASE      },
        { "default",   MhdScriptKind::KW_DEFAULT   },
        { "while",     MhdScriptKind::KW_WHILE     },
        { "do",        MhdScriptKind::KW_DO        },
        { "for",       MhdScriptKind::KW_FOR       },
        { "foreach",   MhdScriptKind::KW_FOREACH   },
        { "try",       MhdScriptKind::KW_TRY       },
        { "catch",     MhdScriptKind::KW_CATCH     },
        { "break",     MhdScriptKind::KW_BREAK     },
        { "continue",  MhdScriptKind::KW_CONTINUE  },
        { "return",    MhdScriptKind::KW_RETURN    },
        { "throw",     MhdScriptKind::KW_THROW     },
        { "operator",  MhdScriptKind::KW_OPERATOR  },
        { "namespace", MhdScriptKind::KW_NAMESPACE },
        { "function",  MhdScriptKind::KW_FUNCTION  },
        { "struct",    MhdScriptKind::KW_STRUCT    },
        { "class",     MhdScriptKind::KW_CLASS     },
        { "let",       MhdScriptKind::KW_LET       },
        { "new",       MhdScriptKind::KW_NEW       },
        { "delete",    MhdScriptKind::KW_DELETE    },
    };
    const auto keyword_iter = keywords.find(token.m_value_str);
    if (keyword_iter != keywords.cend()) {
        token.m_kind = keyword_iter->second;
    } else {
        token.m_kind = MhdScriptKind::ID;
    }
    return true;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL 
void
MhdScriptTokenizer::advance(MhdScriptToken& token)
{
    /// Advance peeked characters, also advancing token position in text.
    for (std::size_t i = 0; i <= m_text_peeked; ++i) {
        if (m_text[i] == '\n') {
            token.m_loc_line += 1;
            token.m_loc_column = 1;
        } else {
            token.m_loc_column += 1;
        }
    }
    m_text += m_text_peeked + 1;
    m_text_peeked = 0;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
