// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptScanner.hpp"

#include <cstdlib>
#include <cctype>

//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE 
bool 
MhdScriptTokenizer::scan(MhdScriptToken& token)
{
    /// Scan a token.
    char character;
    token.clear();
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
MhdScriptTokenizer::scan_id(MhdScriptToken& token)
{
    /// Scan an idenifier or a keyword.
    if (peek()      == 't' &&
        peek_next() == 'r' &&
        peek_next() == 'u' &&
        peek_next() == 'e') {
        advance(token);
        token.m_kind = MhdScriptKind::CT_LGC;
        token.m_value_int = 1;
        token.m_value_str = "true";
    } else if (peek()      == 'f' &&
               peek_next() == 'a' &&
               peek_next() == 'l' &&
               peek_next() == 's' &&
               peek_next() == 'e') {
        advance(token);
        token.m_kind = MhdScriptKind::CT_LGC;
        token.m_value_int = 0;
        token.m_value_str = "false";
    } else if (peek()      == 'i' &&
               peek_next() == 'f') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_IF;
        token.m_value_str = "if";
    } else if (peek()      == 'e' &&
               peek_next() == 'l' &&
               peek_next() == 's' &&
               peek_next() == 'e') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_ELSE;
        token.m_value_str = "else";
    } else if (peek()      == 's' &&
               peek_next() == 'w' &&
               peek_next() == 'i' &&
               peek_next() == 't' &&
               peek_next() == 'c' &&
               peek_next() == 'h') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_SWITCH;
        token.m_value_str = "switch";
    } else if (peek()      == 'c' &&
               peek_next() == 'a' &&
               peek_next() == 's' &&
               peek_next() == 'e') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_CASE;
        token.m_value_str = "case";
    } else if (peek()      == 'd' &&
               peek_next() == 'e' &&
               peek_next() == 'f' &&
               peek_next() == 'a' &&
               peek_next() == 'u' &&
               peek_next() == 'l' &&
               peek_next() == 't') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_DEFAULT;
        token.m_value_str = "default";
    } else if (peek()      == 'w' &&
               peek_next() == 'h' &&
               peek_next() == 'i' &&
               peek_next() == 'l' &&
               peek_next() == 'e') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_WHILE;
        token.m_value_str = "while";
    } else if (peek()      == 'd' &&
               peek_next() == 'o') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_DO;
        token.m_value_str = "do";
    } else if (peek()      == 'f' &&
               peek_next() == 'o' &&
               peek_next() == 'r') {
        advance(token);
        if (peek()      == 'e' &&
            peek_next() == 'a' &&
            peek_next() == 'c' &&
            peek_next() == 'h') {
            advance(token);
            token.m_kind = MhdScriptKind::KW_FOREACH;
            token.m_value_str = "foreach";
        } else {
            token.m_kind = MhdScriptKind::KW_FOR;
            token.m_value_str = "for";
        }
    } else if (peek()      == 't' &&
               peek_next() == 'r' &&
               peek_next() == 'y') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_TRY;
        token.m_value_str = "try";
    } else if (peek()      == 'c' &&
               peek_next() == 'a' &&
               peek_next() == 't' &&
               peek_next() == 'c' &&
               peek_next() == 'h') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_CATCH;
        token.m_value_str = "catch";
    } else if (peek()      == 'b' &&
               peek_next() == 'r' &&
               peek_next() == 'e' &&
               peek_next() == 'a' &&
               peek_next() == 'k') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_BREAK;
        token.m_value_str = "break";
    } else if (peek()      == 'c' &&
               peek_next() == 'o' &&
               peek_next() == 'n' &&
               peek_next() == 't' &&
               peek_next() == 'i' &&
               peek_next() == 'n' &&
               peek_next() == 'u' &&
               peek_next() == 'e') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_CONTINUE;
        token.m_value_str = "continue";
    } else if (peek()      == 'r' &&
               peek_next() == 'e' &&
               peek_next() == 't' &&
               peek_next() == 'u' &&
               peek_next() == 'r' &&
               peek_next() == 'n') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_RETURN;
        token.m_value_str = "return";
    } else if (peek()      == 't' &&
               peek_next() == 'h' &&
               peek_next() == 'r' &&
               peek_next() == 'o' &&
               peek_next() == 'w') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_THROW;
        token.m_value_str = "throw";
    } else if (peek()      == 'o' &&
               peek_next() == 'p' &&
               peek_next() == 'e' &&
               peek_next() == 'r' &&
               peek_next() == 'a' &&
               peek_next() == 't' &&
               peek_next() == 'o' &&
               peek_next() == 'r') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_OPERATOR;
        token.m_value_str = "operator";
    } else if (peek()      == 'n' &&
               peek_next() == 'a' &&
               peek_next() == 'm' &&
               peek_next() == 'e' &&
               peek_next() == 's' &&
               peek_next() == 'p' &&
               peek_next() == 'a' &&
               peek_next() == 'c' &&
               peek_next() == 'e') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_NAMESPACE;
        token.m_value_str = "namespace";
    } else if (peek()      == 'l' &&
               peek_next() == 'e' &&
               peek_next() == 't') {
        advance(token);
        token.m_kind = MhdScriptKind::KW_LET;
        token.m_value_str = "let";
    }
    char character = peek();
    while (character = peek(), std::isalnum(character) ||
           character == '_' || character == '$') {
        advance(token);
        token.m_kind = MhdScriptKind::ID;
        token.m_value_str.push_back(character);
    }
    return true;
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
