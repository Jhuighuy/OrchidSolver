// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidParser.hpp"

//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse()
{
    // Parse statements.
    switch (m_token.m_kind) {
        // Empty statement.
        case MhdToken::Kind::OP_SEMICOLON:
            peek();
            return std::make_shared<MhdExprEmpty>();
        // Compound statement.
        case MhdToken::Kind::OP_BRACE_OPEN:
            peek();
            return parse_expression_compound();
        // Selection statement.
        case MhdToken::Kind::KW_IF:
            peek();
            return parse_expression_cond_if();
        case MhdToken::Kind::KW_SWITCH:
            peek();
            return parse_expression_cond_switch();
        // Loop statement.
        case MhdToken::Kind::KW_WHILE:
            peek();
            return parse_expression_loop_while();
        case MhdToken::Kind::KW_DO:
            peek();
            return parse_expression_loop_do();
        case MhdToken::Kind::KW_FOR:
            peek();
            return parse_expression_loop_for();
        // Jump statement.
        case MhdToken::Kind::KW_BREAK:
            peek();
            return parse_expression_jump_break();
        case MhdToken::Kind::KW_CONTINUE:
            peek();
            return parse_expression_jump_continue();
        case MhdToken::Kind::KW_RETURN:
            peek();
            return parse_expression_jump_return();
        // Declaration or expression statements.
        default: {
                MhdExpr::Ptr expr = parse_expression();
                if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
                    peek();
                } else {
                    return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
                }
                return expr;
            }
    }
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_compound()
{
    MhdExpr::Vec exprs;
    while (m_token.m_kind != MhdToken::Kind::OP_BRACE_CLOSE) {
        exprs.push_back(parse());
    }
    peek();
    MhdExpr::Ptr expr;
    expr = std::make_shared<MhdExprCompound>(exprs);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_cond_if()
{
    // Parse IF conditional expression.
    MhdExpr::Ptr cond;
    MhdExpr::Ptr then_branch;
    MhdExpr::Ptr else_branch;
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    then_branch = parse();
    if (m_token.m_kind == MhdToken::Kind::KW_ELSE) {
        peek();
        else_branch = parse();
    }
    MhdExpr::Ptr expr;
    expr = std::make_shared<MhdExprCondIf>(cond, then_branch, else_branch);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_cond_switch()
{
    // Parse SWITCH conditional expression.
    MhdExpr::Ptr cond;
    MhdExpr::Map cases;
    MhdExpr::Ptr case_default;
    if (m_token.m_kind != MhdToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind != MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdToken::Kind::OP_BRACE_OPEN) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    while (m_token.m_kind != MhdToken::Kind::OP_BRACE_CLOSE) {
        MhdExpr::Ptr* pcase = nullptr;
        if (m_token.m_kind == MhdToken::Kind::KW_CASE) {
            peek();
            cases.push_back({});
            cases.back().first = parse_expression();
            pcase = &cases.back().second;
        } else if (m_token.m_kind == MhdToken::Kind::KW_DEFAULT) {
            peek();
            if (case_default == nullptr) {
                pcase = &case_default;
            } else {
                return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_DEFAULT);
            }
        } else {
            return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
        }
        if (m_token.m_kind != MhdToken::Kind::OP_COLON) {
            peek();
        } else {
            return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
        }
        MhdExpr::Vec exprs;
        ORCHID_ASSERT(0);
    }
    peek();
    MhdExpr::Ptr expr;
    expr = std::make_shared<MhdExprCondSwitch>(cond, cases, case_default);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_loop_while()
{
    // Parse WHILE loop expression.
    MhdExpr::Ptr cond;
    MhdExpr::Ptr body;
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    body = parse();
    MhdExpr::Ptr expr;
    expr = std::make_shared<MhdExprLoopWhile>(cond, body);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_loop_do()
{
    // Parse DO-WHILE loop expression.
    MhdExpr::Ptr cond;
    MhdExpr::Ptr body;
    body = parse();
    if (m_token.m_kind == MhdToken::Kind::KW_WHILE) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    MhdExpr::Ptr expr;
    expr = std::make_shared<MhdExprLoopDoWhile>(cond, body);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_loop_for()
{
    // Parse FOR loop expression.
    MhdExpr::Ptr init;
    MhdExpr::Ptr cond;
    MhdExpr::Ptr iter;
    MhdExpr::Ptr body;
    MhdExpr::Ptr expr;
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdToken::Kind::OP_SEMICOLON) {
        init = parse_expression();
    }
    if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdToken::Kind::OP_SEMICOLON) {
        cond = parse_expression();
    }
    if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdToken::Kind::OP_PAREN_CLOSE) {
        iter = parse_expression();
    }
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    body = parse();
    expr = std::make_shared<MhdExprLoopFor>(init, cond, iter, body);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_jump_break()
{
    // Parse BREAK jump expression.
    MhdExpr::Ptr expr;
    if (m_token.m_kind != MhdToken::Kind::OP_SEMICOLON) {
        expr = parse_expression();
    }
    if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    expr = std::make_shared<MhdExprJumpBreak>(expr);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_jump_continue()
{
    // Parse CONTINUE jump expression.
    MhdExpr::Ptr expr;
    if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    expr = std::make_shared<MhdExprJumpContinue>();
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_jump_return()
{
    // Parse RETURN jump expression.
    MhdExpr::Ptr expr;
    if (m_token.m_kind != MhdToken::Kind::OP_SEMICOLON) {
        expr = parse_expression();
    }
    if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    expr = std::make_shared<MhdExprJumpReturn>(expr);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression()
{
    // Parse expressions.
    return parse_expression_binary_asg();
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_binary_asg()
{
    // Parse ASSIGNMENT expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_or();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_ASG) {
        peek();
        expr = std::make_shared<MhdExprBinaryAssignment>(op, expr, parse_expression_binary_or());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_binary_or()
{
    // Parse BINARY LOGICAL OR expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_and();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_OR) {
        peek();
        expr = std::make_shared<MhdExprBinaryLogical>(op, expr, parse_expression_binary_and());
    }
    return expr;
}
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_binary_and()
{
    // Parse BINARY LOGICAL AND expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_or_bw();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_AND) {
        peek();
        expr = std::make_shared<MhdExprBinaryLogical>(op, expr, parse_expression_binary_or_bw());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_binary_or_bw()
{
    // Parse BINARY BITWISE OR expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_xor_bw();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_OR_BW) {
        peek();
        expr = std::make_shared<MhdExprBinaryBitwise>(op, expr, parse_expression_binary_xor_bw());
    }
    return expr;
}
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_binary_xor_bw()
{
    // Parse BINARY BITWISE XOR expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_and_bw();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_XOR_BW) {
        peek();
        expr = std::make_shared<MhdExprBinaryBitwise>(op, expr, parse_expression_binary_and_bw());
    }
    return expr;
}
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_binary_and_bw()
{
    // Parse BINARY BITWISE AND expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_eq_neq();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_XOR_BW) {
        peek();
        expr = std::make_shared<MhdExprBinaryBitwise>(op, expr, parse_expression_binary_eq_neq());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_binary_eq_neq()
{
    // Parse BINARY EQUALS or NOT EQUALS expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_lt_lte_gt_gte();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_EQ ||
            op == MhdToken::Kind::OP_NEQ) {
        peek();
        expr = std::make_shared<MhdExprBinaryLogical>(op, expr, parse_expression_binary_lt_lte_gt_gte());
    }
    return expr;
}
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_binary_lt_lte_gt_gte()
{
    // Parse BINARY LESS(-EQUALS) or GREATER(-EQUALS) expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_add_sub();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_LT || op == MhdToken::Kind::OP_LTE ||
            op == MhdToken::Kind::OP_GT || op == MhdToken::Kind::OP_GTE) {
        peek();
        expr = std::make_shared<MhdExprBinaryLogical>(op, expr, parse_expression_binary_add_sub());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_binary_add_sub()
{
    // Parse BINARY ADD or SUBTRACT expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_mul_div_mod();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_ADD ||
            op == MhdToken::Kind::OP_SUB) {
        peek();
        expr = std::make_shared<MhdExprBinaryArithmetic>(op, expr, parse_expression_binary_mul_div_mod());
    }
    return expr;
}
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_binary_mul_div_mod()
{
    // Parse BINARY MULTIPLY or DIVIDE expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_unary();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_MUL ||
            op == MhdToken::Kind::OP_DIV ||
            op == MhdToken::Kind::OP_MOD) {
        peek();
        expr = std::make_shared<MhdExprBinaryArithmetic>(op, expr, parse_expression_unary());
    }
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERNAL 
MhdExpr::Ptr MhdParser::parse_expression_unary()
{
    // Parse UNARY expression.
    switch (m_token.m_kind) {
        // Logic unary operations.
        case MhdToken::Kind::OP_NOT:
            return parse_expression_unary_not();
        // Arithmetic unary operations.
        case MhdToken::Kind::OP_ADD:
        case MhdToken::Kind::OP_SUB:
            return parse_expression_unary_negate();
        // Parentheses or cast operations.
        case MhdToken::Kind::OP_PAREN_OPEN:
            return nullptr;
        // Expression operand.
        default:
            return parse_expression_unary_factor();
    }
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_unary_not()
{
    // Parse UNARY NOT expression.
    MhdExpr::Ptr expr;
    expr = std::make_shared<MhdExprUnaryNot>(m_token.m_kind, parse_expression_unary());
    peek();
    return expr;
}
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_unary_negate()
{
    // Parse UNARY NEGATE expression.
    MhdExpr::Ptr expr;
    expr = std::make_shared<MhdExprUnaryNegate>(m_token.m_kind, parse_expression_unary());
    peek();
    return expr;
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_unary_operand()
{
    // Parse OPERAND expression.
    MhdExpr::Ptr expr;
    switch (m_token.m_kind) {
        // Constant expression operand.
        case MhdToken::Kind::CT_STR:
            expr = std::make_shared<MhdExprConst>(m_token.m_value_str);
            peek();
            return expr;
        case MhdToken::Kind::CT_DBL:
            expr = std::make_shared<MhdExprConst>(m_token.m_value_dbl);
            peek();
            return expr;
        case MhdToken::Kind::CT_INT:
            expr = std::make_shared<MhdExprConst>(m_token.m_value_int);
            peek();
            return expr;
        case MhdToken::Kind::CT_LGC:
            expr = std::make_shared<MhdExprConst>(m_token.m_value_int != 0);
            peek();
            return expr;
        // Identifier expression operand.
        case MhdToken::Kind::ID:
            expr = std::make_shared<MhdExprIdent>(m_token.m_value_str);
            peek();
            return expr;
        // Error case.
        default:
            return std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN);
    }
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_unary_factor()
{
    // Parse OPERAND expression FACTOR.
    MhdExpr::Ptr expr = parse_expression_unary_operand();
    switch (m_token.m_kind) {
        case MhdToken::Kind::OP_PAREN_OPEN:
            peek();
            expr = std::make_shared<MhdExprFactorCall>(expr, parse_expression_unary_factor_call());
            break;
        case MhdToken::Kind::OP_BRACKET_OPEN:
            peek();
            expr = std::make_shared<MhdExprFactorIndex>(expr, parse_expression_unary_factor_index());
            break;
        default:
            break;
    }
    return expr;
}
ORCHID_INTERNAL
MhdExpr::Vec MhdParser::parse_expression_unary_factor_call()
{
    // Parse CALL expression FACTOR.
    MhdExpr::Vec exprs;
    while (m_token.m_kind != MhdToken::Kind::OP_PAREN_CLOSE) {
        exprs.push_back(parse_expression());
        switch (m_token.m_kind) {
            case MhdToken::Kind::OP_PAREN_CLOSE:
                break;
            case MhdToken::Kind::OP_COMMA:
                peek();
                break;
            default:
                exprs = {std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN)};
                return exprs;                
        }
    }
    peek();
    return exprs;
}
ORCHID_INTERNAL
MhdExpr::Vec MhdParser::parse_expression_unary_factor_index()
{
    // Parse INDEX expression FACTOR.
    MhdExpr::Vec exprs;
    while (m_token.m_kind != MhdToken::Kind::OP_BRACKET_CLOSE) {
        exprs.push_back(parse_expression());
        switch (m_token.m_kind) {
            case MhdToken::Kind::OP_BRACKET_CLOSE:
                break;
            case MhdToken::Kind::OP_COMMA:
                peek();
                break;
            default:
                exprs = {std::make_shared<MhdExprError>(MhdParserErr::ERR_UNEXP_TOKEN)};
                break;
        }
    }
    peek();
    return exprs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################

std::map<std::string, MhdDynamic> g_vars;
#include <cstdio>
extern "C" void orchid_solver_scanner_test()
{
    MhdParser p("for(x=0;x<10;)x=x+1;");
    auto e = p.parse();
    auto g = e.get();
    printf("%u\n", g->eval());
}
//int main() 
//{
//    orchid_solver_scanner_test();
//    return 0;
//}
