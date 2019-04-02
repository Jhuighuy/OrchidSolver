// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScanner.hpp"
#include "OrchidAST.hpp"

//########################################################################################################
//########################################################################################################
//########################################################################################################
/** 
 * Scan a single token. 
 */
ORCHID_INTERNAL 
bool MhdTokenizer::scan(MhdToken& token)
{
    token = MhdToken();
    char character;
    while (isspace(character = peek())) {
        advance();
    }
    character = peek();
    switch (character) {
        case '\'':
        case '\"':
            return scan_str(token);
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
            if (character == 'i' &&
                peek_next() == 'f') {
                advance();
                token.m_kind = MhdToken::Kind::KW_IF;
                return true;
            }
            if (character == 'e' &&
                peek_next() == 'l' && peek_next() == 's' && peek_next() == 'e') {
                advance();
                token.m_kind = MhdToken::Kind::KW_ELSE;
                return true;
            }
            if (character == 'f' &&
                peek_next() == 'o' && peek_next() == 'r') {
                advance();
                token.m_kind = MhdToken::Kind::KW_FOR;
                return true;
            }
            if (character == 't' &&
                peek_next() == 'r' && peek_next() == 'u' && peek_next() == 'e') {
                advance();
                token.m_kind = MhdToken::Kind::CT_LGC;
                token.m_value_int = 1;
                return true;
            }
            if (character == 'f' &&
                peek_next() == 'a' && peek_next() == 'l' && peek_next() == 's' && peek_next() == 'e') {
                advance();
                token.m_kind = MhdToken::Kind::CT_LGC;
                token.m_value_int = 0;
                return true;
            }
            if (character == 'w' &&
                peek_next() == 'h' && peek_next() == 'i' && peek_next() == 'l' && peek_next() == 'e') {
                advance();
                token.m_kind = MhdToken::Kind::KW_WHILE;
                token.m_value_int = 0;
                return true;
            }
            if (character == 'b' &&
                peek_next() == 'r' && peek_next() == 'e' && peek_next() == 'a' && peek_next() == 'k') {
                advance();
                token.m_kind = MhdToken::Kind::KW_BREAK;
                token.m_value_int = 0;
                return true;
            }
            return scan_id(token);
        case '.':
            advance();
            token.m_kind = MhdToken::Kind::OP_DOT;
            return true;
        case ',':
            advance();
            token.m_kind = MhdToken::Kind::OP_COMMA;
            return true;
        case ';':
            advance();
            token.m_kind = MhdToken::Kind::OP_SEMICOLON;
            return true;
        case '=':
            advance();
            if (peek() == '=') {
                advance();
                token.m_kind = MhdToken::Kind::OP_EQ;
            } else {
                token.m_kind = MhdToken::Kind::OP_ASG;
            }
            return true;
        case '+':
            advance();
            token.m_kind = MhdToken::Kind::OP_ADD;
            return true;
        case '-':
            advance();
            token.m_kind = MhdToken::Kind::OP_SUB;
            return true;
        case '*':
            advance();
            token.m_kind = MhdToken::Kind::OP_MUL;
            return true;
        case '/':
            advance();
            token.m_kind = MhdToken::Kind::OP_DIV;
            return true;
        case '%':
            advance();
            token.m_kind = MhdToken::Kind::OP_MOD;
            return true;
        case '!':
            advance();
            if (peek() == '=') {
                advance();
                token.m_kind = MhdToken::Kind::OP_NEQ;
            } else {
                token.m_kind = MhdToken::Kind::OP_NOT;
            }
            return true;
        case '<':
            advance();
            if (peek() == '=') {
                advance();
                token.m_kind = MhdToken::Kind::OP_LTE;
            } else {
                token.m_kind = MhdToken::Kind::OP_LT;
            }
            return true;
        case '>':
            advance();
            if (peek() == '=') {
                advance();
                token.m_kind = MhdToken::Kind::OP_GTE;
            } else {
                token.m_kind = MhdToken::Kind::OP_GT;
            }
            return true;
        case '&':
            advance();
            if (peek() == '&') {
                advance();
                token.m_kind = MhdToken::Kind::OP_AND;
            } else {
                token.m_kind = MhdToken::Kind::OP_AND_BW;
            }
            return true;
        case '|':
            advance();
            if (peek() == '|') {
                advance();
                token.m_kind = MhdToken::Kind::OP_OR;
            } else {
                token.m_kind = MhdToken::Kind::OP_OR_BW;
            }
            return true;
        case '^':
            advance();
            if (peek() == '^') {
                advance();
                token.m_kind = MhdToken::Kind::OP_XOR;
            } else {
                token.m_kind = MhdToken::Kind::OP_XOR_BW;
            }
            return true;
        case '(':
            advance();
            token.m_kind = MhdToken::Kind::OP_PAREN_OPEN;
            return true;
        case ')':
            advance();
            token.m_kind = MhdToken::Kind::OP_PAREN_CLOSE;
            return true;
        case '{':
            advance();
            token.m_kind = MhdToken::Kind::OP_BRACE_OPEN;
            return true;
        case '}':
            advance();
            token.m_kind = MhdToken::Kind::OP_BRACE_CLOSE;
            return true;
        case '[':
            advance();
            token.m_kind = MhdToken::Kind::OP_BRACKET_OPEN;
            return true;
        case ']':
            advance();
            token.m_kind = MhdToken::Kind::OP_BRACKET_CLOSE;
            return true;
        default:
            token.m_kind = MhdToken::Kind::ERR;
            token.m_value_str = "Invalid symbol in the input stream.";
            return false;
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
/**
 * Scan a single string token.
 */
ORCHID_INTERNAL 
bool MhdTokenizer::scan_str(MhdToken& token)
{
    return false;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
/**
 * Scan a single numeric token.
 */
ORCHID_INTERNAL 
bool MhdTokenizer::scan_num(MhdToken& token)
{
    bool can_be_hex = false;
    bool can_be_oct = false;
    bool has_frc = false;
    bool has_exp = false;
    // Determine what possible radix is
    // this numeric literal is using.
    char character = peek();
    if (character == '0') {
        character = peek_next();
        if (character == 'x' || character == 'X') {
            advance();
            can_be_hex = true;
        } else {
            can_be_oct = true;
        }
    }
    if (can_be_hex) {
        // Parse a hexadecimal literal.
        while (isxdigit(character = peek())) {
            advance();
            token.m_value_str.push_back(character);
        }
    } else {
        // Parse a decimal, octal of floating point literal.
        bool has_oct_chars = true;
        while (isdigit(character = peek())) {
            advance();
            token.m_value_str.push_back(character);
            has_oct_chars &= character <= '7';
        }
        // Parse fraction.
        character = peek();
        if (character == '.') {
            advance();
            token.m_value_str.push_back(character);
            if (isdigit(character = peek())) {
                advance();
                token.m_value_str.push_back(character);
                while (isdigit(character = peek())) {
                    advance();
                    token.m_value_str.push_back(character);
                }
            } else {
                token.m_kind = MhdToken::Kind::ERR;
                token.m_value_str = "Invalid numeric literal fraction.";
                return false;
            }
            has_frc = true;
        }
        // Parse exponent.
        character = peek();
        if (character == 'e' || character == 'E' ||
            character == 'd' || character == 'D') {
            advance();
            token.m_value_str.push_back('e');
            if ((character = peek()) == '+' || character == '-') {
                advance();
                token.m_value_str.push_back(character);
            }
            if (isdigit(character = peek())) {
                advance();
                token.m_value_str.push_back(character);
                while (isdigit(character = peek())) {
                    advance();
                    token.m_value_str.push_back(character);
                }
            }
            else {
                token.m_kind = MhdToken::Kind::ERR;
                token.m_value_str = "Invalid numeric literal exponent.";
                return false;
            }
            has_exp = true;
        }
        can_be_oct &= !has_frc && !has_exp;
        if (can_be_oct && !has_oct_chars) {
            token.m_kind = MhdToken::Kind::ERR;
            token.m_value_str = "Invalid octal number.";
            return false;
        }
    }
    if (has_frc || has_exp) {
        char* end_ptr = nullptr;
        token.m_kind = MhdToken::Kind::CT_DBL;
        token.m_value_dbl = std::strtod(token.m_value_str.c_str(), &end_ptr);
        if (token.m_value_str.c_str() == end_ptr) {
            token.m_kind = MhdToken::Kind::ERR;
            token.m_value_str = "Floating point overflow.";
            return false;
        }
    } else {
        char* end_ptr = nullptr;
        token.m_kind = MhdToken::Kind::CT_INT;
        token.m_value_int = std::strtol(token.m_value_str.c_str(), &end_ptr,
                                        can_be_hex ? 16 : can_be_oct ? 8 : 10);
        if (token.m_value_str.c_str() == end_ptr) {
            token.m_kind = MhdToken::Kind::ERR;
            token.m_value_str = "Integer overflow.";
            return false;
        }
    }
    return false;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
/**
 * Scan a single identifier token.
 */
bool MhdTokenizer::scan_id(MhdToken& token)
{
    token.m_value_str.push_back(peek());
    token.m_kind = MhdToken::Kind::ID;
    advance();
    return true;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
