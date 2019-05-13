// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptParser.hpp"
#include "OrchidScriptScanner.hpp"

#include <unordered_map>
#include <algorithm>
#include <vector>
#include <set>

class MhdIncDec
{
    int& m_val;
public:
    MhdIncDec(int& val)
        : m_val(val) { ++m_val; }
    ~MhdIncDec() { --m_val; }
};  // class MhdIncDec

#define MhdParseUnexpTokenError(a,...) MhdParseError(a)
#define MhdParseErrorUnexpContinue(a,...) MhdParseError(a)
#define MhdParseErrorUnexpBreak(a,...) MhdParseError(a)
#define MhdParseErrorUnexpReturn(a,...) MhdParseError(a)

//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptExpr::Ptr MhdScriptParser::parse_wrap()
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
MhdScriptExpr::Ptr MhdScriptParser::parse()
{
    /// Parse statement.
    if (m_token.m_kind == MhdScriptKind::NONE) {
        /* Peek first token. */
        advance(); 
    }
    /* Empty statement or end of stream. */
    if (matched(MhdScriptKind::OP_SEMICOLON, MhdScriptKind::END)) {
        return std::make_shared<MhdScriptExprEmpty>();
    }
    /* Compound statement. */
    if (matched(MhdScriptKind::OP_BRACE_OPEN)) {
        return parse_expression_compound();
    }
    /* Declaration statement. */
    if (matched(MhdScriptKind::KW_FUNCTION)) {
        return parse_expression_decl_function();
    }
    if (matched(MhdScriptKind::KW_CLASS)) {
        return parse_expression_decl_class();
    }
    if (matched(MhdScriptKind::KW_NAMESPACE)) {
        return parse_expression_decl_namespace();
    }
    /* Selection statement. */
    if (matched(MhdScriptKind::KW_IF)) {
        return parse_expression_cond_if();
    }
    if (matched(MhdScriptKind::KW_SWITCH)) {
        return parse_expression_cond_switch();
    }
    /* Loop statement. */
    if (matched(MhdScriptKind::KW_DO)) {
        return parse_expression_loop_do();
    }
    if (matched(MhdScriptKind::KW_WHILE)) {
        return parse_expression_loop_while();
    }
    if (matched(MhdScriptKind::KW_FOR)) {
        return parse_expression_loop_for();
    }
    if (matched(MhdScriptKind::KW_FOREACH)) {
        return parse_expression_loop_foreach();
    }
    /* Try-Catch statement. */
    if (matched(MhdScriptKind::KW_TRY)) {
        return parse_expression_try_catch();
    }
    /* Jump statement. */
    if (matched(MhdScriptKind::KW_BREAK)) {
        return parse_expression_jump_break();
    }
    if (matched(MhdScriptKind::KW_CONTINUE)) {
        return parse_expression_jump_continue();
    }
    if (matched(MhdScriptKind::KW_RETURN)) {
        return parse_expression_jump_return();
    }
    if (matched(MhdScriptKind::KW_THROW)) {
        return parse_expression_jump_throw();
    }
    /* Declaration or expression statements. */
    MhdScriptExpr::Ptr expr = parse_expression();
    expect(MhdScriptKind::OP_SEMICOLON);
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptExpr::Ptr MhdScriptParser::parse_program_wrap()
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
MhdScriptExpr::Ptr MhdScriptParser::parse_program()
{
    /// Parse program.
    MhdScriptExpr::Vec exprs;
    while (!matched(MhdScriptKind::END)) {
        exprs.push_back(parse());
    }
    MhdScriptExpr::Ptr expr =
        std::make_shared<MhdScriptExprCompound>(exprs, false);
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_compound()
{
    /// Parse COMPOUND expression.
    MhdScriptExpr::Vec exprs;
    while (!matched(MhdScriptKind::OP_BRACE_CLOSE)) {
        exprs.push_back(parse());
    }
    MhdScriptExpr::Ptr expr = std::make_shared<MhdScriptExprCompound>(exprs);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_decl_function()
{
    /// Parse FUNCTION DECLARATION expression.
    std::string id;
    if (m_token.m_kind == MhdScriptKind::ID) {
        id = m_token.m_value_str;
        advance();
    } else {
        throw MhdParseUnexpTokenError(m_token);
    }
    MhdScriptExpr::Ptr expr = std::make_shared<MhdScriptExprIdent>(id, true);
    while (m_token.m_kind == MhdScriptKind::OP_DOT) {
        advance();
        expr = parse_expression_operand_factor_subscript(expr);
    }
    MhdScriptExpr::Ptr func;
    func = parse_expression_operand_primary_func();
    expr = std::make_shared<MhdScriptExprAssignment>(MhdScriptKind::OP_ASG, expr, func);
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_decl_class()
{
    /// Parse CLASS DECLARATION expression.
    std::string id;
    std::string id_base;
    std::map<std::string, MhdScriptExpr::Ptr> id_fields;
    if (m_token.m_kind == MhdScriptKind::ID) {
        id = m_token.m_value_str;
        advance();
    } else {
        throw MhdParseUnexpTokenError(m_token);
    }
    if (m_token.m_kind == MhdScriptKind::OP_COLON) {
        advance();
        if (m_token.m_kind == MhdScriptKind::ID) {
            id_base = m_token.m_value_str;
            advance();
        } else {
            throw MhdParseUnexpTokenError(m_token);
        }
    }
    if (m_token.m_kind == MhdScriptKind::OP_BRACE_OPEN) {
        advance();
    } else {
        throw MhdParseUnexpTokenError(m_token);
    }
    while (m_token.m_kind != MhdScriptKind::OP_BRACE_CLOSE) {
        switch (m_token.m_kind) {
            /* Class field. */
            case MhdScriptKind::ID:
                break;
            /* Class constuctor/destructor. */
            case MhdScriptKind::KW_NEW:
                break;
            case MhdScriptKind::KW_DELETE:
                break;
            /* Class method. */
            case MhdScriptKind::KW_FUNCTION:
                break;
            case MhdScriptKind::KW_OPERATOR:
                break;
            /* End of class. */
            case MhdScriptKind::OP_BRACE_CLOSE:
                break;
            default:
                throw MhdParseUnexpTokenError(m_token);
        }
        parse();
    }
    advance();
    return nullptr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_decl_namespace()
{
    /// Parse NAMESPACE expression.
    std::string id;
    MhdScriptExpr::Vec exprs;
    if (m_token.m_kind == MhdScriptKind::ID) {
        id = m_token.m_value_str;
        advance();
    } else {
        throw MhdParseUnexpTokenError(m_token);
    }
    if (m_token.m_kind == MhdScriptKind::OP_BRACE_OPEN) {
        advance();
    } else {
        throw MhdParseUnexpTokenError(m_token);
    }
    while (m_token.m_kind != MhdScriptKind::OP_BRACE_CLOSE) {
        exprs.push_back(parse());
    }
    advance();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprNamespace>(id, exprs);
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_cond_if()
{
    /// Parse IF conditional expression.
    MhdScriptExpr::Ptr if_cond;
    MhdScriptExpr::Ptr if_then_branch;
    MhdScriptExpr::Ptr if_else_branch;
    expect(MhdScriptKind::OP_PAREN_OPEN);
    if_cond = parse_expression();
    expect(MhdScriptKind::OP_PAREN_CLOSE);
    if_then_branch = parse();
    if (matched(MhdScriptKind::KW_ELSE)) {
        if_else_branch = parse();
    }
    return std::make_shared<MhdScriptExprIf>(if_cond, if_then_branch, if_else_branch);
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_cond_switch()
{
    /// Parse SWITCH conditional expression.
    MhdScriptExpr::Ptr switch_cond;
    MhdScriptExpr::Map switch_cases;
    expect(MhdScriptKind::OP_PAREN_OPEN);
    switch_cond = parse_expression();
    expect(MhdScriptKind::OP_PAREN_CLOSE);
    expect(MhdScriptKind::OP_BRACE_OPEN);
    while (true) {
        switch_cases.emplace_back();
        if (matched(MhdScriptKind::KW_CASE)) {
            switch_cases.back().first = parse_expression();
        } else {
            expect(MhdScriptKind::KW_DEFAULT);
            switch_cases.back().first = nullptr;
        }
        expect(MhdScriptKind::OP_COLON);
        MhdScriptExpr::Vec switch_case_exprs;
        while (true) {
            /* End of switch. */
            if (matched(MhdScriptKind::OP_BRACE_CLOSE)) {
                return std::make_shared<MhdScriptExprSwitch>(switch_cond, switch_cases);
            }
            /* End of case. */
            if (matches(MhdScriptKind::KW_CASE,
                        MhdScriptKind::KW_DEFAULT)) {
                auto expr = std::make_shared<MhdScriptExprCompound>(switch_case_exprs, false);
                switch_cases.back().second = expr;
                break;
            }
            switch_case_exprs.push_back(parse());
        }
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_loop_do()
{
    /// Parse DO-WHILE loop expression.
    MhdScriptExpr::Ptr dowhile_cond;
    MhdScriptExpr::Ptr dowhile_body;
    dowhile_body = parse();
    expect(MhdScriptKind::KW_WHILE);
    expect(MhdScriptKind::OP_PAREN_OPEN);
    {
        MhdIncDec inside_loop{ m_inside_loop };
        dowhile_body = parse_expression();
    }
    expect(MhdScriptKind::OP_PAREN_CLOSE);
    expect(MhdScriptKind::OP_SEMICOLON);
    return std::make_shared<MhdScriptExprDoWhile>(dowhile_cond, dowhile_body);
}
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_loop_while()
{
    /// Parse WHILE loop expression.
    MhdScriptExpr::Ptr while_cond;
    MhdScriptExpr::Ptr while_body;
    expect(MhdScriptKind::OP_PAREN_OPEN);
    while_cond = parse_expression();
    expect(MhdScriptKind::OP_PAREN_CLOSE);
    {
        MhdIncDec inside_loop{ m_inside_loop };
        while_body = parse();
    }
    return std::make_shared<MhdScriptExprWhile>(while_cond, while_body);
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_loop_for()
{
    /// Parse FOR loop expression.
    MhdScriptExpr::Ptr for_init;
    MhdScriptExpr::Ptr for_cond;
    MhdScriptExpr::Ptr for_iter;
    MhdScriptExpr::Ptr for_body;
    expect(MhdScriptKind::OP_PAREN_OPEN);
    if (!matched(MhdScriptKind::OP_SEMICOLON)) {
        for_init = parse_expression();
        expect(MhdScriptKind::OP_SEMICOLON);
    }
    if (!matched(MhdScriptKind::OP_SEMICOLON)) {
        for_cond = parse_expression();
        expect(MhdScriptKind::OP_SEMICOLON);
    }
    if (!matched(MhdScriptKind::OP_PAREN_CLOSE)) {
        for_iter = parse_expression();
        expect(MhdScriptKind::OP_PAREN_CLOSE);
    }
    {
        MhdIncDec inside_loop{ m_inside_loop };
        for_body = parse();
    }
    return std::make_shared<MhdScriptExprFor>(for_init, for_cond, for_iter, for_body);
}
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_loop_foreach()
{
    /// Parse FOREACH loop expression.
    std::string foreach_id;
    MhdScriptExpr::Ptr foreach_cont;
    MhdScriptExpr::Ptr foreach_body;
    expect(MhdScriptKind::OP_PAREN_OPEN);
    expects(MhdScriptKind::ID);
    foreach_id = m_token.m_value_str;
    advance();
    expect(MhdScriptKind::OP_COLON);
    foreach_cont = parse_expression();
    expect(MhdScriptKind::OP_PAREN_CLOSE);
    {
        MhdIncDec inside_loop{ m_inside_loop };
        foreach_body = parse();
    }
    return std::make_shared<MhdScriptExprForEach>(foreach_id, foreach_cont, foreach_body);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_try_catch()
{
    /// Parse ``try-catch`` expression.
    /// Catch section is optional, exception object is also optional.
    MhdScriptExpr::Ptr try_block;
    MhdScriptExpr::Ptr try_catch_block;
    std::string try_catch_arg;
    try_block = parse();
    if (matched(MhdScriptKind::KW_CATCH)) {
        expect(MhdScriptKind::OP_PAREN_OPEN);
        expects(MhdScriptKind::ID);
        try_catch_arg = m_token.m_value_str;
        advance();
        expect(MhdScriptKind::OP_PAREN_CLOSE);
        try_catch_block = parse();
    }
    return std::make_shared<MhdScriptExprTryCatch>(try_block, try_catch_block, try_catch_arg);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_jump_break()
{
    /// Parse ``break`` jump expression.
    if (m_inside_loop == 0 &&
        m_inside_switch == 0) {
        throw MhdParseErrorUnexpBreak(m_token);
    }
    MhdScriptExpr::Ptr expr;
    if (!matched(MhdScriptKind::OP_SEMICOLON)) {
        expr = parse_expression();
        expect(MhdScriptKind::OP_SEMICOLON);
    }
    return std::make_shared<MhdScriptExprBreak>(expr);
}
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_jump_continue()
{
    /// Parse ``continue`` jump expression.
    if (m_inside_loop == 0) {
        throw MhdParseErrorUnexpContinue(m_token);
    }
    expect(MhdScriptKind::OP_SEMICOLON);
    return std::make_shared<MhdScriptExprContinue>();
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_jump_return()
{
    /// Parse ``return`` jump expression.
    if (m_inside_func == 0) {
        throw MhdParseErrorUnexpReturn(m_token);
    }
    MhdScriptExpr::Ptr expr;
    if (!matched(MhdScriptKind::OP_SEMICOLON)) {
        expr = parse_expression();
        expect(MhdScriptKind::OP_SEMICOLON);
    }
    return std::make_shared<MhdScriptExprReturn>(expr);
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_jump_throw()
{
    /// Parse ``try`` jump expression.
    MhdScriptExpr::Ptr expr;
    if (!matched(MhdScriptKind::OP_SEMICOLON)) {
        expr = parse_expression();
        expect(MhdScriptKind::OP_SEMICOLON);
    }
    return std::make_shared<MhdScriptExprThrow>(expr);
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
#define PARSE_BINARY_EXPRESSION(class_name, func_name, prev_func_name) \
MHD_INTERNAL \
MhdScriptExpr::Ptr MhdScriptParser::func_name() \
{ \
    MhdScriptKind op; \
    MhdScriptExpr::Ptr expr = prev_func_name(); \
    while (op = m_token.m_kind, matched(OPS)) { \
        expr = std::make_shared<class_name>(op, expr, prev_func_name()); \
    } \
    return expr; \
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_comma()
{
    /// Parse COMMA expressions.
    MhdScriptExpr::Ptr expr = parse_expression_binary_asg();
    while (matched(MhdScriptKind::OP_COMMA)) {
        expr = std::make_shared<MhdScriptExprCompound>(expr, parse_expression_binary_asg());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
#define OPS MhdScriptKind::OP_ASG, \
            MhdScriptKind::OP_ADD_ASG, \
            MhdScriptKind::OP_SUB_ASG, \
            MhdScriptKind::OP_MUL_ASG, \
            MhdScriptKind::OP_DIV_ASG, \
            MhdScriptKind::OP_MOD_ASG
PARSE_BINARY_EXPRESSION(MhdScriptExprAssignment,
                        parse_expression_binary_asg, 
                        parse_expression_ternary)
#undef OPS
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_ternary()
{
    /// Parse TERNARY expressions.
    MhdScriptExpr::Ptr expr = parse_expression_binary_or();
    while (matched(MhdScriptKind::OP_QUESTION)) {
        MhdScriptExpr::Ptr tern_then_branch;
        MhdScriptExpr::Ptr tern_else_branch;
        tern_then_branch = parse_expression();
        expect(MhdScriptKind::OP_COLON);
        tern_else_branch = parse_expression();
        expr = std::make_shared<MhdScriptExprIf>(expr, tern_then_branch, tern_else_branch);
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
#define OPS MhdScriptKind::OP_OR
PARSE_BINARY_EXPRESSION(MhdScriptExprLogical, 
                        parse_expression_binary_or, 
                        parse_expression_binary_and)
#undef OPS
#define OPS MhdScriptKind::OP_AND
PARSE_BINARY_EXPRESSION(MhdScriptExprLogical,
                        parse_expression_binary_and, 
                        parse_expression_binary_eq_neq)
#undef OPS
//--------------------------------------------------------------------------------------------------------
#define OPS MhdScriptKind::OP_EQ, \
            MhdScriptKind::OP_NEQ
PARSE_BINARY_EXPRESSION(MhdScriptExprLogical, 
                        parse_expression_binary_eq_neq, 
                        parse_expression_binary_lt_lte_gt_gte)
#undef OPS
#define OPS MhdScriptKind::OP_LT, MhdScriptKind::OP_LTE, \
            MhdScriptKind::OP_GT, MhdScriptKind::OP_GTE
PARSE_BINARY_EXPRESSION(MhdScriptExprLogical, 
                        parse_expression_binary_lt_lte_gt_gte, 
                        parse_expression_binary_add_sub)
#undef OPS
//--------------------------------------------------------------------------------------------------------
#define OPS MhdScriptKind::OP_ADD, \
            MhdScriptKind::OP_SUB
PARSE_BINARY_EXPRESSION(MhdScriptExprArithmetic, 
                        parse_expression_binary_add_sub, 
                        parse_expression_binary_mul_div_mod)
#undef OPS
#define OPS MhdScriptKind::OP_MUL, \
            MhdScriptKind::OP_DIV, MhdScriptKind::OP_MOD
PARSE_BINARY_EXPRESSION(MhdScriptExprArithmetic, 
                        parse_expression_binary_mul_div_mod, 
                        parse_expression_unary)
#undef OPS
//--------------------------------------------------------------------------------------------------------
#undef PARSE_BINARY_EXPRESSION
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL 
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_unary()
{
    /// Parse UNARY expression.
    /* Logic/Bitwise unary expression. */
    if (matches(MhdScriptKind::OP_NOT)) {
        return parse_expression_unary_not();
    }
    /* Arithmetic unary expression. */
    if (matches(MhdScriptKind::OP_ADD,
                MhdScriptKind::OP_SUB)) {
        return parse_expression_unary_negate();
    }
    /* Increment/decrement expression. */
    if (matches(MhdScriptKind::OP_INC,
                MhdScriptKind::OP_DEC)) {
        ORCHID_ASSERT(0);
        return nullptr;
    }
    /* Parentheses/Brace expression. */
    if (matched(MhdScriptKind::OP_BRACE_OPEN)) {
        return parse_expression_compound();
    }
    if (matched(MhdScriptKind::OP_PAREN_OPEN)) {
        auto expr{ parse_expression_compound() };
        expect(MhdScriptKind::OP_PAREN_CLOSE);
        return expr;
    }
    /* Operand expression. */
    return parse_expression_operand();
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_unary_not()
{
    /// Parse UNARY NOT expression.
    MhdScriptExpr::Ptr expr;
    MhdScriptKind op = m_token.m_kind;
    advance();
    return std::make_shared<MhdScriptExprNot>(op, parse_expression_unary());
}
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_unary_negate()
{
    /// Parse UNARY NEGATE expression.
    MhdScriptExpr::Ptr expr;
    MhdScriptKind op = m_token.m_kind;
    advance();
    return std::make_shared<MhdScriptExprNegate>(op, parse_expression_unary());
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_operand()
{
    /// Parse operand expression.
    MhdScriptExpr::Ptr expr = parse_expression_operand_primary();
    do {
        /* Call expression. */
        if (matched(MhdScriptKind::OP_PAREN_OPEN)) {
            expr = parse_expression_operand_factor_call(expr);
            continue;
        }
        /* Index expression. */
        if (matched(MhdScriptKind::OP_BRACKET_OPEN)) {
            expr = parse_expression_operand_factor_index(expr);
            continue;
        }
        if (matched(MhdScriptKind::OP_DOT)) {
            expr = parse_expression_operand_factor_subscript(expr);
            continue;
        }
    } while (false);
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_operand_primary()
{
    /// Parse primary operand expression.
    /// Primary operands are constant keywords, like `true`, numeric or string constants,
    /// functions, arrays, lists, maps or identifiers.
    /* Keyword primary operand. */
    if (matched(MhdScriptKind::KW_TRUE)) {
        return std::make_shared<MhdScriptExprConst>(true);
    }
    if (matched(MhdScriptKind::KW_FALSE)) {
        return std::make_shared<MhdScriptExprConst>(false);
    }
    if (matched(MhdScriptKind::KW_NULL)) {
        return std::make_shared<MhdScriptExprConst>(nullptr);
    }
    /* Constant primary operand. */
    if (matches(MhdScriptKind::CT_INT)) {
        auto expr{ std::make_shared<MhdScriptExprConst>(m_token.m_value_int) };
        advance();
        return expr;
    }
    if (matches(MhdScriptKind::CT_DBL)) {
        auto expr{ std::make_shared<MhdScriptExprConst>(m_token.m_value_dbl) };
        advance();
        return expr;
    }
    if (matches(MhdScriptKind::CT_STR)) {
        auto expr{ std::make_shared<MhdScriptExprConst>(m_token.m_value_str) };
        advance();
        return expr;
    }
    /* Function primary operand. */
    if (matched(MhdScriptKind::KW_FUNCTION)) {
        return parse_expression_operand_primary_func();
    }
    if (matched(MhdScriptKind::OP_BRACKET_OPEN)) {
        expect(MhdScriptKind::OP_BRACKET_CLOSE);
        return parse_expression_operand_primary_func();
    }
    /* Identifier primary operand. */
    if (matched(MhdScriptKind::KW_LET)) {
        expects(MhdScriptKind::ID);
        auto expr{ std::make_shared<MhdScriptExprIdent>(m_token.m_value_str, true) };
        advance();
        return expr;
    }
    if (matches(MhdScriptKind::ID)) {
        auto expr{ std::make_shared<MhdScriptExprIdent>(m_token.m_value_str) };
        advance();
        return expr;
    }
    unexpected();
    return nullptr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_operand_primary_func()
{
    /// Parse primary function operand expression.
    std::vector<std::string> func_args;
    expect(MhdScriptKind::OP_PAREN_OPEN);
    while (true) {
        if (matches(MhdScriptKind::ID)) {
            const auto& func_arg = m_token.m_value_str;
            if (std::find(func_args.cbegin(), 
                          func_args.cend(), func_arg) != func_args.cend()) {
                throw MhdParseError(MhdScriptError::ERR_FUNC_ARG_REDECL);
            }
            func_args.push_back(func_arg);
            advance();
            if (matched(MhdScriptKind::OP_COMMA)) {
                continue;
            }
        }
        if (matched(MhdScriptKind::OP_PAREN_CLOSE)) {
            break;
        }
        unexpected();
    }
    MhdScriptExpr::Ptr func_body;
    {
        MhdIncDec inside_func{ m_inside_func };
        func_body = parse();
    }
    return std::make_shared<MhdScriptExprConstFunc>(func_args, func_body);
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_operand_primary_list()
{
    ORCHID_ASSERT(0);
    MhdScriptExpr::Ptr expr;
    //expr = std::make_shared<MhdScriptExprConstArray>(parse_expression_operand_factor_index());
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_operand_factor_call(MhdScriptExpr::Ptr called_expr)
{
    /// Parse CALL expression factor.
    MhdScriptExpr::Vec call_args;
    /* Subscript or Index is followed by the call.
     * Pass the accessed object as a first argument. 
     * @todo This should be moved out of parser, also condition is incorrect. */
    MhdScriptExprIndex* called_expr_index;
    if (called_expr_index = dynamic_cast<MhdScriptExprIndex*>(called_expr.get()),
        called_expr_index != nullptr) {
        call_args.push_back(called_expr_index->m_array);
    }
    while (true) {
        if (matched(MhdScriptKind::OP_PAREN_CLOSE)) {
            return std::make_shared<MhdScriptExprCall>(called_expr, call_args);
        }
        call_args.push_back(parse_expression());
        if (matched(MhdScriptKind::OP_COMMA)) {
            continue;
        }
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_operand_factor_index(MhdScriptExpr::Ptr indexed_expr)
{
    /// Parse index expression factor.
    /// At least one index is expected.
    MhdScriptExpr::Vec indices;
    while (true) {
        indices.push_back(parse_expression());
        if (matched(MhdScriptKind::OP_BRACKET_CLOSE)) {
            return std::make_shared<MhdScriptExprIndex>(indexed_expr, indices);
        }
        expect(MhdScriptKind::OP_COMMA);
    }
}
MHD_INTERNAL
MhdScriptExpr::Ptr MhdScriptParser::parse_expression_operand_factor_subscript(MhdScriptExpr::Ptr indexed_expr)
{
    /// Parse subscript expression factor.
    /// Subscript factors are translated into the index factors.
    if (matches(MhdScriptKind::ID)) {
        auto index_expr{ std::make_shared<MhdScriptExprConst>(m_token.m_value_str) };
        advance();
        return std::make_shared<MhdScriptExprIndex>(indexed_expr, MhdScriptExpr::Vec{ index_expr });
    }
    if (matched(MhdScriptKind::KW_OPERATOR)) {
        auto index_expr{ std::make_shared<MhdScriptExprConst>(parse_operator()) };
        return std::make_shared<MhdScriptExprIndex>(indexed_expr, MhdScriptExpr::Vec{ index_expr });
    }
    unexpected();
    return nullptr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
const char* MhdScriptParser::parse_operator()
{
    /// Parse operator.
    /// Unary or postfix versions of operators (like `+x`, `y--`) are distinguished by the curly braces.
    if (matched(MhdScriptKind::OP_PAREN_OPEN)) {
        expect(MhdScriptKind::OP_PAREN_CLOSE);
        return MhdScriptOp::OP_CALL;
    }
    if (matched(MhdScriptKind::OP_BRACKET_OPEN)) {
        expect(MhdScriptKind::OP_BRACKET_CLOSE);
        return MhdScriptOp::OP_INDEX;
    }
    if (matched(MhdScriptKind::OP_BRACE_OPEN)) {
        /* Unary operators. */
        if (matched(MhdScriptKind::OP_ADD)) {
            expect(MhdScriptKind::OP_BRACE_CLOSE);
            return MhdScriptOp::OP_PLUS;
        }
        if (matched(MhdScriptKind::OP_SUB)) {
            expect(MhdScriptKind::OP_BRACE_CLOSE);
            return MhdScriptOp::OP_MINUS;
        }
        /* Postfix operators. */
        if (matched(MhdScriptKind::OP_INC)) {
            expect(MhdScriptKind::OP_BRACE_CLOSE);
            return MhdScriptOp::OP_INC_POSTFIX;
        }
        if (matched(MhdScriptKind::OP_DEC)) {
            expect(MhdScriptKind::OP_BRACE_CLOSE);
            return MhdScriptOp::OP_DEC_POSTFIX;
        }
        unexpected();
        return nullptr;
    }
    /* Other operators. */
    static const std::unordered_map<MhdScriptKind, const char*> operators{
        { MhdScriptKind::OP_NOT,     MhdScriptOp::OP_NOT     },
        { MhdScriptKind::OP_EQ,      MhdScriptOp::OP_EQ      },
        { MhdScriptKind::OP_NEQ,     MhdScriptOp::OP_NEQ     },
        { MhdScriptKind::OP_LT,      MhdScriptOp::OP_LT      },
        { MhdScriptKind::OP_GT,      MhdScriptOp::OP_GT      },
        { MhdScriptKind::OP_LTE,     MhdScriptOp::OP_LTE     },
        { MhdScriptKind::OP_GTE,     MhdScriptOp::OP_GTE     },
        { MhdScriptKind::OP_OR,      MhdScriptOp::OP_OR      },
        { MhdScriptKind::OP_AND,     MhdScriptOp::OP_AND     },
        { MhdScriptKind::OP_ADD,     MhdScriptOp::OP_ADD     },
        { MhdScriptKind::OP_SUB,     MhdScriptOp::OP_SUB     },
        { MhdScriptKind::OP_MUL,     MhdScriptOp::OP_MUL     },
        { MhdScriptKind::OP_DIV,     MhdScriptOp::OP_DIV     },
        { MhdScriptKind::OP_MOD,     MhdScriptOp::OP_MOD     },
        { MhdScriptKind::OP_INC,     MhdScriptOp::OP_INC     },
        { MhdScriptKind::OP_DEC,     MhdScriptOp::OP_DEC     },
        { MhdScriptKind::OP_ADD_ASG, MhdScriptOp::OP_ADD_ASG },
        { MhdScriptKind::OP_SUB_ASG, MhdScriptOp::OP_SUB_ASG },
        { MhdScriptKind::OP_MUL_ASG, MhdScriptOp::OP_MUL_ASG },
        { MhdScriptKind::OP_DIV_ASG, MhdScriptOp::OP_DIV_ASG },
        { MhdScriptKind::OP_MOD_ASG, MhdScriptOp::OP_MOD_ASG },
    };
    const auto operators_iter = operators.find(m_token.m_kind);
    if (operators_iter != operators.cend()) {
        advance();
        return operators_iter->second;
    }
    unexpected();
    return nullptr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
