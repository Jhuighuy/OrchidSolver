// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptParser.hpp"
#include "OrchidScriptScanner.hpp"

#include <sstream>
#include <vector>
#include <set>

//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
std::string 
MhdParseError::make_parse_error(const MhdScriptToken& token, 
                                const std::string& what)
{
    std::ostringstream parse_error;
    parse_error << "ERROR AT "
                << token.m_loc_file << ","
                << token.m_loc_line << ","
                << token.m_loc_column << ": " << what;
    return parse_error.str();
}
//--------------------------------------------------------------------------------------------------------
struct MhdParseUnexpTokenError : public MhdParseError
{
public:
    template<typename... T>
    MhdParseUnexpTokenError(const MhdScriptToken& token, const T&... expected)
        : MhdParseError(make_error_unexp_token(token, expected...)) {}
private:
    template<typename... T>
    static std::string make_error_unexp_token(const MhdScriptToken& token, 
                                              const T&... expected)
    {
        return MhdParseError::make_parse_error(token, "unexpected token");
    }
};  // struct MhdParseUnexpTokenError
//--------------------------------------------------------------------------------------------------------
struct MhdParseUnexpDefaultError : public MhdParseError
{
public:
    MhdParseUnexpDefaultError(struct MhdScriptParser*)
        : MhdParseError(std::string("what")) {}
};  // struct MhdParseUnexpDefaultError
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptExpr::Ptr 
MhdScriptParser::parse_wrap()
{
    /// Parse statement.
    MhdScriptExpr::Ptr expr;
    try {
        expr = parse();
    } catch (const MhdParseError& parse_exc) {
        printf("%s\n%s\n", parse_exc.what(), m_tokenizer.m_text);
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptExpr::Ptr 
MhdScriptParser::parse()
{
    /// Parse statement.
    if (m_token.m_kind == MhdScriptKind::NONE) {
        /* Peek first token. */
        peek(); 
    }
    MhdScriptExpr::Ptr expr;
    switch (m_token.m_kind) {
        /* Empty statement or end of stream. */
        case MhdScriptKind::END:
        case MhdScriptKind::OP_SEMICOLON:
            peek();
            expr = std::make_shared<MhdScriptExprEmpty>();
            return expr;
        /* Compound statement. */
        case MhdScriptKind::OP_BRACE_OPEN:
            peek();
            expr = parse_expression_compound();
            return expr;
        /* Declaration statement. */
        case MhdScriptKind::KW_FUNCTION:
            peek();
            expr = parse_expression_decl_function();
            return expr;
        case MhdScriptKind::KW_STRUCT:
        case MhdScriptKind::KW_CLASS:
            peek();
            expr = parse_expression_decl_struct();
            return expr;
        case MhdScriptKind::KW_NAMESPACE:
            peek();
            expr = parse_expression_decl_namespace();
            return expr;
        /* Selection statement. */
        case MhdScriptKind::KW_IF:
            peek();
            expr = parse_expression_cond_if();
            return expr;
        case MhdScriptKind::KW_SWITCH:
            peek();
            expr = parse_expression_cond_switch();
            return expr;
        /* Loop statement. */
        case MhdScriptKind::KW_WHILE:
            peek();
            expr = parse_expression_loop_while();
            return expr;
        case MhdScriptKind::KW_DO:
            peek();
            expr = parse_expression_loop_do();
            return expr;
        case MhdScriptKind::KW_FOR:
            peek();
            expr = parse_expression_loop_for();
            return expr;
        case MhdScriptKind::KW_FOREACH:
            peek();
            expr = parse_expression_loop_foreach();
            return expr;
        /* Try-Catch statement. */
        case MhdScriptKind::KW_TRY:
            peek();
            expr = parse_expression_try_catch();
            return expr;
        /* Jump statement. */
        case MhdScriptKind::KW_BREAK:
            peek();
            expr = parse_expression_jump_break();
            return expr;
        case MhdScriptKind::KW_CONTINUE:
            peek();
            expr = parse_expression_jump_continue();
            return expr;
        case MhdScriptKind::KW_RETURN:
            peek();
            expr = parse_expression_jump_return();
            return expr;
        case MhdScriptKind::KW_THROW:
            peek();
            expr = parse_expression_jump_throw();
            return expr;
        /* Declaration or expression statements. */
        default:
            expr = parse_expression();
            if (m_token.m_kind == MhdScriptKind::OP_SEMICOLON) {
                peek();
            } else {
                throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_SEMICOLON);
            }
            return expr;
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptExpr::Ptr
MhdScriptParser::parse_program_wrap()
{
    /// Parse program.
    MhdScriptExpr::Ptr expr;
    try {
        expr = parse_program();
    } catch (const MhdParseError& parse_exc) {
        printf("%s\n%s\n", parse_exc.what(), m_tokenizer.m_text);
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptExpr::Ptr
MhdScriptParser::parse_program()
{
    /// Parse program.
    MhdScriptExpr::Vec exprs;
    exprs.push_back(parse());
    while (m_token.m_kind != MhdScriptKind::END) {
        exprs.push_back(parse());
    }
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprCompound>(exprs);
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_compound()
{
    /// Parse COMPOUND expression.
    MhdScriptExpr::Vec exprs;
    while (m_token.m_kind != MhdScriptKind::OP_BRACE_CLOSE) {
        exprs.push_back(parse());
    }
    peek();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprCompound>(exprs);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr
MhdScriptParser::parse_expression_decl_function()
{
    /// Parse FUNCTION expression.
    std::string id;
    if (m_token.m_kind == MhdScriptKind::ID) {
        id = m_token.m_value_str;
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::ID);
    }
    MhdScriptExpr::Ptr func;
    MhdScriptExpr::Ptr expr = std::make_shared<MhdScriptExprIdent>(id, true);
    while (m_token.m_kind == MhdScriptKind::OP_DOT) {
        peek();
        expr = parse_expression_unary_factor_subscript(expr);
    }
    func = parse_expression_unary_operand_func();
    expr = std::make_shared<MhdScriptExprAssignment>(MhdScriptKind::OP_ASG, expr, func);
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr
MhdScriptParser::parse_expression_decl_struct()
{
    /// Parse STRUCT expression.
    std::string id;
    std::string id_base;
    if (m_token.m_kind == MhdScriptKind::ID) {
        id = m_token.m_value_str;
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::ID);
    }
    if (m_token.m_kind == MhdScriptKind::OP_COLON) {
        peek();
        if (m_token.m_kind == MhdScriptKind::ID) {
            id_base = m_token.m_value_str;
            peek();
        } else {
            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::ID);
        }
    }
    if (m_token.m_kind == MhdScriptKind::OP_BRACE_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_BRACE_OPEN);
    }
    while (m_token.m_kind != MhdScriptKind::OP_BRACE_CLOSE) {
        parse();
    }
    peek();
    return nullptr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr
MhdScriptParser::parse_expression_decl_namespace()
{
    /// Parse NAMESPACE expression.
    std::string id;
    MhdScriptExpr::Vec exprs;
    if (m_token.m_kind == MhdScriptKind::ID) {
        id = m_token.m_value_str;
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::ID);
    }
    if (m_token.m_kind == MhdScriptKind::OP_BRACE_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_BRACE_OPEN);
    }
    while (m_token.m_kind != MhdScriptKind::OP_BRACE_CLOSE) {
        exprs.push_back(parse());
    }
    peek();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprNamespace>(id, exprs);
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_cond_if()
{
    /// Parse IF conditional expression.
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Ptr then_branch;
    MhdScriptExpr::Ptr else_branch;
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_OPEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_CLOSE);
    }
    then_branch = parse();
    if (m_token.m_kind == MhdScriptKind::KW_ELSE) {
        peek();
        else_branch = parse();
    }
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprIf>(cond, then_branch, else_branch);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_cond_switch()
{
    /// Parse SWITCH conditional expression.
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Map cases;
    MhdScriptExpr::Ptr case_default;
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_OPEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_CLOSE);
    }
    if (m_token.m_kind == MhdScriptKind::OP_BRACE_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_BRACE_OPEN);
    }
    while (m_token.m_kind != MhdScriptKind::OP_BRACE_CLOSE) {
        /* Parse case label. */
        MhdScriptExpr::Ptr* pcase_branch = nullptr;
        switch (m_token.m_kind) {
            case MhdScriptKind::KW_CASE:
                /* Ordinary case. */
                peek();
                cases.emplace_back();
                cases.back().first = parse_expression();
                pcase_branch = &cases.back().second;
                break;
            case MhdScriptKind::KW_DEFAULT:
                /* Default case. */
                peek();
                if (case_default != nullptr) {
                    throw MhdParseUnexpDefaultError(this);
                }
                pcase_branch = &case_default;
                break;
            default:
                /* Error case. */
                throw MhdParseUnexpTokenError(m_token, MhdScriptKind::KW_CASE,
                                                       MhdScriptKind::KW_DEFAULT);
        }
        if (m_token.m_kind == MhdScriptKind::OP_COLON) {
            peek();
        } else {
            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_COLON);
        }
        /* Parse case body. */
        MhdScriptExpr::Vec case_branch_exprs;
        while (m_token.m_kind != MhdScriptKind::OP_BRACE_CLOSE &&
               m_token.m_kind != MhdScriptKind::KW_CASE &&
               m_token.m_kind != MhdScriptKind::KW_DEFAULT) {
            case_branch_exprs.push_back(parse());
        }
        *pcase_branch = std::make_shared<MhdScriptExprCompound>(case_branch_exprs); 
    }
    peek();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprSwitch>(cond, cases, case_default);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_loop_while()
{
    /// Parse WHILE loop expression.
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Ptr body;
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_OPEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_CLOSE);
    }
    body = parse();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprWhile>(cond, body);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_loop_do()
{
    /// Parse DO-WHILE loop expression:
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Ptr body;
    body = parse();
    if (m_token.m_kind == MhdScriptKind::KW_WHILE) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::KW_WHILE);
    }
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_OPEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_CLOSE);
    }
    if (m_token.m_kind == MhdScriptKind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_SEMICOLON);
    }
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprDoWhile>(cond, body);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_loop_for()
{
    /// Parse FOR loop expression.
    MhdScriptExpr::Ptr init;
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Ptr iter;
    MhdScriptExpr::Ptr body;
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_OPEN);
    }
    if (m_token.m_kind != MhdScriptKind::OP_SEMICOLON) {
        init = parse_expression();
    }
    if (m_token.m_kind == MhdScriptKind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_SEMICOLON);
    }
    if (m_token.m_kind != MhdScriptKind::OP_SEMICOLON) {
        cond = parse_expression();
    }
    if (m_token.m_kind == MhdScriptKind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_SEMICOLON);
    }
    if (m_token.m_kind != MhdScriptKind::OP_PAREN_CLOSE) {
        iter = parse_expression();
    }
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_CLOSE);
    }
    body = parse();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprFor>(init, cond, iter, body);
    return expr; 
}
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_loop_foreach()
{
    /// Parse FOREACH loop expression.
    std::string id;
    MhdScriptExpr::Ptr cont;
    MhdScriptExpr::Ptr body;
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_OPEN);
    }
    if (m_token.m_kind == MhdScriptKind::ID) {
        id = m_token.m_value_str;
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::ID);
    }
    if (m_token.m_kind == MhdScriptKind::OP_COLON) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_SEMICOLON);
    }
    cont = parse_expression();
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_CLOSE);
    }
    body = parse();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprForEach>(id, cont, body);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_try_catch()
{
    /// Parse TRY-CATCH expression.
    MhdScriptExpr::Ptr try_block;
    MhdScriptExpr::Ptr catch_block;
    std::string catch_arg;
    try_block = parse();
    if (m_token.m_kind == MhdScriptKind::KW_CATCH) {
        peek();
        if (m_token.m_kind == MhdScriptKind::OP_PAREN_OPEN) {
            peek();
        } else {
            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_OPEN);
        }
        if (m_token.m_kind == MhdScriptKind::ID) {
            catch_arg = m_token.m_value_str;
            peek();
        } else {
            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::ID);
        }
        if (m_token.m_kind == MhdScriptKind::OP_PAREN_CLOSE) {
            peek();
        } else {
            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_CLOSE);
        }
    }
    catch_block = parse();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprTryCatch>(try_block, catch_block, catch_arg);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_jump_break()
{
    /// Parse BREAK jump expression.
    MhdScriptExpr::Ptr expr;
    if (m_token.m_kind != MhdScriptKind::OP_SEMICOLON) {
        expr = parse_expression();
    }
    if (m_token.m_kind == MhdScriptKind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_SEMICOLON);
    }
    expr = std::make_shared<MhdScriptExprBreak>(expr);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_jump_continue()
{
    /// Parse CONTINUE jump expression.
    MhdScriptExpr::Ptr expr;
    if (m_token.m_kind == MhdScriptKind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_SEMICOLON);
    }
    expr = std::make_shared<MhdScriptExprContinue>();
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_jump_return()
{
    /// Parse RETURN jump expression.
    MhdScriptExpr::Ptr expr;
    if (m_token.m_kind != MhdScriptKind::OP_SEMICOLON) {
        expr = parse_expression();
    }
    if (m_token.m_kind == MhdScriptKind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_SEMICOLON);
    }
    expr = std::make_shared<MhdScriptExprReturn>(expr);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_jump_throw()
{
    /// Parse THROW jump expression.
    MhdScriptExpr::Ptr expr;
    if (m_token.m_kind != MhdScriptKind::OP_SEMICOLON) {
        expr = parse_expression();
    }
    if (m_token.m_kind == MhdScriptKind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_SEMICOLON);
    }
    expr = std::make_shared<MhdScriptExprThrow>(expr);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression()
{
    /// Parse expressions.
    return parse_expression_binary_asg();
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_comma()
{
    /// Parse COMMA expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_asg();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_COMMA) {
        peek();
        expr = std::make_shared<MhdScriptExprCompound>(expr, parse_expression_binary_asg());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_asg()
{
    /// Parse ASSIGNMENT expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_ternary();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_ASG ||
           op == MhdScriptKind::OP_ADD_ASG ||
           op == MhdScriptKind::OP_SUB_ASG ||
           op == MhdScriptKind::OP_MUL_ASG ||
           op == MhdScriptKind::OP_DIV_ASG ||
           op == MhdScriptKind::OP_MOD_ASG ||
           op == MhdScriptKind::OP_OR_BW_ASG ||
           op == MhdScriptKind::OP_XOR_BW_ASG ||
           op == MhdScriptKind::OP_AND_BW_ASG ||
           op == MhdScriptKind::OP_LSHIFT_ASG ||
           op == MhdScriptKind::OP_RSHIFT_ASG) {
        peek();
        expr = std::make_shared<MhdScriptExprAssignment>(op, expr, parse_expression_ternary());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_ternary()
{
    /// Parse TERNARY expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_or();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_QUESTION) {
        peek();
        MhdScriptExpr::Ptr then_branch;
        MhdScriptExpr::Ptr else_branch;
        then_branch = parse_expression();
        if (m_token.m_kind == MhdScriptKind::OP_COLON) {
            peek();
        } else {
            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_COLON);
        }
        else_branch = parse_expression();
        expr = std::make_shared<MhdScriptExprIf>(expr, then_branch, else_branch);
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_or()
{
    /// Parse BINARY LOGICAL OR expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_and();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_OR) {
        peek();
        expr = std::make_shared<MhdScriptExprLogical>(op, expr, parse_expression_binary_and());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_and()
{
    /// Parse BINARY LOGICAL AND expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_or_bw();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_AND) {
        peek();
        expr = std::make_shared<MhdScriptExprLogical>(op, expr, parse_expression_binary_or_bw());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_or_bw()
{
    /// Parse BINARY BITWISE OR expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_xor_bw();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_OR_BW) {
        peek();
        expr = std::make_shared<MhdScriptExprBitwise>(op, expr, parse_expression_binary_xor_bw());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_xor_bw()
{
    /// Parse BINARY BITWISE XOR expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_and_bw();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_XOR_BW) {
        peek();
        expr = std::make_shared<MhdScriptExprBitwise>(op, expr, parse_expression_binary_and_bw());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_and_bw()
{
    /// Parse BINARY BITWISE AND expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_eq_neq();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_AND_BW) {
        peek();
        expr = std::make_shared<MhdScriptExprBitwise>(op, expr, parse_expression_binary_eq_neq());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_eq_neq()
{
    /// Parse BINARY EQUALS or NOT EQUALS expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_lt_lte_gt_gte();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_EQ ||
           op == MhdScriptKind::OP_NEQ) {
        peek();
        expr = std::make_shared<MhdScriptExprLogical>(op, expr, parse_expression_binary_lt_lte_gt_gte());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_lt_lte_gt_gte()
{
    /// Parse BINARY LESS(-EQUALS) or GREATER(-EQUALS) expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_shift();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_LT || op == MhdScriptKind::OP_LTE ||
           op == MhdScriptKind::OP_GT || op == MhdScriptKind::OP_GTE) {
        peek();
        expr = std::make_shared<MhdScriptExprLogical>(op, expr, parse_expression_binary_shift());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_shift()
{
    /// Parse BINARY BITWISE SHIFT expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_add_sub();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_LSHIFT ||
           op == MhdScriptKind::OP_RSHIFT) {
        peek();
        expr = std::make_shared<MhdScriptExprBitwise>(op, expr, parse_expression_binary_add_sub());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_add_sub()
{
    /// Parse BINARY ADD or SUBTRACT expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_mul_div_mod();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_ADD ||
           op == MhdScriptKind::OP_SUB) {
        peek();
        expr = std::make_shared<MhdScriptExprArithmetic>(op, expr, parse_expression_binary_mul_div_mod());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_mul_div_mod()
{
    /// Parse BINARY MULTIPLY or DIVIDE expressions.
    MhdScriptKind op;
    MhdScriptExpr::Ptr expr = parse_expression_unary();
    while (op = m_token.m_kind,
           op == MhdScriptKind::OP_MUL ||
           op == MhdScriptKind::OP_DIV ||
           op == MhdScriptKind::OP_MOD) {
        peek();
        expr = std::make_shared<MhdScriptExprArithmetic>(op, expr, parse_expression_unary());
    }
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary()
{
    /// Parse UNARY expression.
    MhdScriptExpr::Ptr expr;
    switch (m_token.m_kind) {
        /* Logic/Bitwise unary expression. */
        case MhdScriptKind::OP_NOT:
        case MhdScriptKind::OP_NOT_BW:
            expr = parse_expression_unary_not();
            return expr;
        /* Arithmetic unary expression. */
        case MhdScriptKind::OP_ADD:
        case MhdScriptKind::OP_SUB:
            expr = parse_expression_unary_negate();
            return expr;
        /* Arithmetic unary increment/decrement expression. */
        case MhdScriptKind::OP_INC:
        case MhdScriptKind::OP_DEC:
            ORCHID_ASSERT(0);
            return expr;
        /* Parentheses/Brace expression. */
        case MhdScriptKind::OP_BRACE_OPEN:
            peek();
            expr = parse_expression_compound();
            return expr;
        case MhdScriptKind::OP_PAREN_OPEN:
            peek();
            expr = parse_expression();
            if (m_token.m_kind == MhdScriptKind::OP_PAREN_CLOSE) {
                peek();
            } else {
                throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_CLOSE);
            }
            return expr;
        /* Operand expression. */
        default:
            expr = parse_expression_unary_factor();
            return expr;
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_not()
{
    /// Parse UNARY NOT expression.
    MhdScriptExpr::Ptr expr;
    MhdScriptKind op = m_token.m_kind;
    peek();
    expr = std::make_shared<MhdScriptExprNot>(op, parse_expression_unary());
    return expr;
}
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_negate()
{
    /// Parse UNARY NEGATE expression.
    MhdScriptExpr::Ptr expr;
    MhdScriptKind op = m_token.m_kind;
    peek();
    expr = std::make_shared<MhdScriptExprNegate>(op, parse_expression_unary());
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_operand()
{
    /// Parse OPERAND expression.
    MhdScriptExpr::Ptr expr;
    switch (m_token.m_kind) {
        /* Constant operand expression. */
        case MhdScriptKind::CT_NIL:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal());
            peek();
            return expr;
        case MhdScriptKind::CT_LGC:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal(m_token.m_value_int != 0));
            peek();
            return expr;
        case MhdScriptKind::CT_INT:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal(m_token.m_value_int));
            peek();
            return expr;
        case MhdScriptKind::CT_DBL:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal(m_token.m_value_dbl));
            peek();
            return expr;
        case MhdScriptKind::CT_STR:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal(m_token.m_value_str));
            peek();
            return expr;
        /* Function operand expression. */
        case MhdScriptKind::OP_BRACKET_OPEN:
            peek();
            if (m_token.m_kind == MhdScriptKind::OP_BRACKET_CLOSE) {
                peek();
                expr = parse_expression_unary_operand_func();
            } else {
                expr = parse_expression_unary_operand_list();
            }
            return expr;
        /* Identifier operand expression. */
        case MhdScriptKind::ID:
            expr = std::make_shared<MhdScriptExprIdent>(m_token.m_value_str);
            peek();
            return expr;
        case MhdScriptKind::KW_LET:
            peek();
            if (m_token.m_kind == MhdScriptKind::ID) {
                expr = std::make_shared<MhdScriptExprIdent>(m_token.m_value_str, true);
                peek();
            } else {
                throw MhdParseUnexpTokenError(m_token, MhdScriptKind::ID);
            }
            return expr;
        /* Error case. */
        default:
            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::CT_NIL,
                                                   MhdScriptKind::CT_LGC,
                                                   MhdScriptKind::CT_INT,
                                                   MhdScriptKind::CT_DBL,
                                                   MhdScriptKind::CT_STR,
                                                   MhdScriptKind::OP_BRACKET_OPEN,
                                                   MhdScriptKind::ID, MhdScriptKind::KW_LET);
    }
}
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_operand_func()
{
    /// Parse FUNCTION OPERAND expression.
    /** @todo Implement capture list. */
    if (m_token.m_kind == MhdScriptKind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_OPEN);
    }
    std::set<std::string> args_set;
    while (m_token.m_kind != MhdScriptKind::OP_PAREN_CLOSE) {
        /* Parse argument name. */
        if (m_token.m_kind == MhdScriptKind::ID) {
            if (args_set.count(m_token.m_value_str) == 0) {
                args_set.insert(m_token.m_value_str);
            } else {
                throw MhdParseError(MhdScriptError::ERR_FUNC_ARG_REDECL);
            }
            peek();
        } else {
            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::ID);
        }
        switch (m_token.m_kind) {
            /* Expect another argument. */
            case MhdScriptKind::OP_COMMA:
                peek();
                break;
            /* Final argument. */
            case MhdScriptKind::OP_PAREN_CLOSE:
                break;
            /* Error case. */
            default:
                throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_COMMA,
                                                       MhdScriptKind::OP_PAREN_CLOSE);
        }
    }
    peek();
    MhdScriptExpr::Ptr body = parse();
    MhdScriptExpr::Ptr expr;
    const std::vector<std::string> args{args_set.cbegin(), args_set.cend()};
    expr = std::make_shared<MhdScriptExprConstFunc>(args, body);
    return expr;
}
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_operand_list()
{
    ORCHID_ASSERT(0);
    MhdScriptExpr::Ptr expr;
    //expr = std::make_shared<MhdScriptExprConstArray>(parse_expression_unary_factor_index());
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_factor()
{
    /// Parse OPERAND expression FACTOR.
    MhdScriptExpr::Ptr expr = parse_expression_unary_operand();
    while (m_token.m_kind == MhdScriptKind::OP_PAREN_OPEN ||
           m_token.m_kind == MhdScriptKind::OP_BRACKET_OPEN ||
           m_token.m_kind == MhdScriptKind::OP_DOT) {
        switch (m_token.m_kind) {
            /* Call expression. */
            case MhdScriptKind::OP_PAREN_OPEN:
                peek();
                expr = parse_expression_unary_factor_call(expr);
                break;
            /* Index expression. */
            case MhdScriptKind::OP_BRACKET_OPEN:
                peek();
                expr = parse_expression_unary_factor_index(expr);
                break;
            /* Subscript expression. */
            case MhdScriptKind::OP_DOT:
                peek();
                expr = parse_expression_unary_factor_subscript(expr);
            default:
                break;
        }
    }
    return expr;
}
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_factor_call(MhdScriptExpr::Ptr expr)
{
    /// Parse CALL expression FACTOR.
    MhdScriptExpr::Vec exprs;
    MhdScriptExprIndex* expr_index;
    if (expr_index = dynamic_cast<MhdScriptExprIndex*>(expr.get()),
        expr_index != nullptr) {
        /* Subscript or Index is followed by the call.
         * Pass the accessed object as a first argument. */
        exprs.push_back(expr_index->m_array);
    }
    while (m_token.m_kind != MhdScriptKind::OP_PAREN_CLOSE) {
        exprs.push_back(parse_expression());
        switch (m_token.m_kind) {
            /* Expect another argument. */
            case MhdScriptKind::OP_COMMA:
                peek();
                break;
            /* Final argument. */
            case MhdScriptKind::OP_PAREN_CLOSE:
                break;
            /* Error case. */
            default:
                throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_COMMA,
                                                       MhdScriptKind::OP_PAREN_CLOSE);
        }
    }
    peek();
    expr = std::make_shared<MhdScriptExprCall>(expr, exprs);
    return expr;
}
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_factor_index(MhdScriptExpr::Ptr expr)
{
    /// Parse INDEX expression FACTOR.
    MhdScriptExpr::Vec exprs;
    while (m_token.m_kind != MhdScriptKind::OP_BRACKET_CLOSE) {
        exprs.push_back(parse_expression());
        switch (m_token.m_kind) {
            /* Expect another index. */
            case MhdScriptKind::OP_COMMA:
                peek();
                break;
            /* Final index. */
            case MhdScriptKind::OP_BRACKET_CLOSE:
                break;
            /* Error case. */
            default:
                throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_COMMA,
                                                       MhdScriptKind::OP_BRACKET_CLOSE);
        }
    }
    peek();
    expr = std::make_shared<MhdScriptExprIndex>(expr, exprs);
    return expr;
}
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_factor_subscript(MhdScriptExpr::Ptr expr)
{
    /// Parse SUBSCRIPT expression FACTOR.
    MhdScriptExpr::Vec exprs;
    switch (m_token.m_kind) {
        case MhdScriptKind::ID:
            /* Normal subscript. */
            exprs.push_back(std::make_shared<MhdScriptExprConst>(m_token.m_value_str));
            peek();
            break;
        case MhdScriptKind::KW_OPERATOR:
            /* Overloaded operator subscript. */
            peek();
            switch (m_token.m_kind) {
                case MhdScriptKind::OP_INC:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator++"));
                    break;
                case MhdScriptKind::OP_DEC:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator--"));
                    break;
                case MhdScriptKind::OP_ADD:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator+"));
                    break;
                case MhdScriptKind::OP_ADD_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator+="));
                    break;
                case MhdScriptKind::OP_SUB:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator-"));
                    break;
                case MhdScriptKind::OP_SUB_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator-="));
                    break;
                case MhdScriptKind::OP_MUL:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator*"));
                    break;
                case MhdScriptKind::OP_MUL_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator*="));
                    break;
                case MhdScriptKind::OP_DIV:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator/"));
                    break;
                case MhdScriptKind::OP_DIV_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator/="));
                    break;
                case MhdScriptKind::OP_MOD:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator%"));
                    break;
                case MhdScriptKind::OP_MOD_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator%="));
                    break;
                case MhdScriptKind::OP_NOT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator!"));
                    break;
                case MhdScriptKind::OP_NOT_BW:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator~"));
                    break;
                case MhdScriptKind::OP_EQ:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator=="));
                    break;
                case MhdScriptKind::OP_NEQ:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator!="));
                    break;
                case MhdScriptKind::OP_LT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator<"));
                    break;
                case MhdScriptKind::OP_LTE:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator<="));
                    break;
                case MhdScriptKind::OP_GT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator>"));
                    break;
                case MhdScriptKind::OP_GTE:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator>="));
                    break;
                case MhdScriptKind::OP_LSHIFT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator<<"));
                    break;
                case MhdScriptKind::OP_LSHIFT_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator<<="));
                    break;
                case MhdScriptKind::OP_RSHIFT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator>>"));
                    break;
                case MhdScriptKind::OP_RSHIFT_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator>>="));
                    break;
                case MhdScriptKind::OP_AND:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator&&"));
                    break;
                case MhdScriptKind::OP_AND_BW:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator&"));
                    break;
                case MhdScriptKind::OP_AND_BW_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator&="));
                    break;
                case MhdScriptKind::OP_OR:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator||"));
                    break;
                case MhdScriptKind::OP_OR_BW:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator|"));
                    break;
                case MhdScriptKind::OP_OR_BW_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator|="));
                    break;
                case MhdScriptKind::OP_XOR_BW:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator^"));
                    break;
                case MhdScriptKind::OP_XOR_BW_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator^="));
                    break;
                case MhdScriptKind::OP_PAREN_OPEN:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator()"));
                    if (m_token.m_kind == MhdScriptKind::OP_PAREN_CLOSE) {
                        peek();
                    } else {
                        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_PAREN_CLOSE);
                    }
                    break;
                case MhdScriptKind::OP_BRACKET_OPEN:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator[]"));
                    if (m_token.m_kind == MhdScriptKind::OP_BRACKET_CLOSE) {
                        peek();
                    } else {
                        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_BRACKET_CLOSE);
                    }
                    break;
                case MhdScriptKind::OP_BRACE_OPEN:
                    /* Unary operators. */
                    peek();
                    switch (m_token.m_kind) {
                        case MhdScriptKind::OP_ADD:
                            peek();
                            exprs.push_back(std::make_shared<MhdScriptExprConst>("operator{+}"));
                            break;
                        case MhdScriptKind::OP_SUB:
                            peek();
                            exprs.push_back(std::make_shared<MhdScriptExprConst>("operator{-}"));
                            break;
                        case MhdScriptKind::OP_INC:
                            peek();
                            exprs.push_back(std::make_shared<MhdScriptExprConst>("operator{++}"));
                            break;
                        case MhdScriptKind::OP_DEC:
                            peek();
                            exprs.push_back(std::make_shared<MhdScriptExprConst>("operator{--}"));
                            break;
                        default:
                            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_ADD,
                                                                   MhdScriptKind::OP_SUB,
                                                                   MhdScriptKind::OP_INC,
                                                                   MhdScriptKind::OP_DEC);
                    }
                    if (m_token.m_kind == MhdScriptKind::OP_BRACE_CLOSE) {
                        peek();
                    } else {
                        throw MhdParseUnexpTokenError(m_token, MhdScriptKind::OP_BRACE_CLOSE);
                    }
                    break;
                default:
                    throw MhdParseUnexpTokenError(m_token, "<operator>", "<{operator}>");
            }
            break;
        default:
            throw MhdParseUnexpTokenError(m_token, MhdScriptKind::ID,
                                                   MhdScriptKind::KW_OPERATOR);
    }
    expr = std::make_shared<MhdScriptExprIndex>(expr, exprs);
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
