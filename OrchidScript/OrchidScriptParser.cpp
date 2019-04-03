// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptParser.hpp"
#include "OrchidScriptParserSyntax.hpp"

#include <exception>

struct MhdParseException : public std::runtime_error
{
public:
    MhdParseException(...) 
        : std::runtime_error("HUI!") {}   
};  // struct MhdParseException

//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERFACE
MhdExpr::Ptr MhdParser::parse_wrap()
{
    MhdExpr::Ptr expr;
    try {
        expr = parse();
    } catch (const MhdParseException& parse_exc) {
        printf("Error: %s\n%s\n", m_tokenizer.m_text, parse_exc.what());
    }
    return expr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse()
{
    // Parse statements.
    MhdExpr::Ptr expr;
    switch (m_token.m_kind) {
        // Empty statement.
        case MhdToken::Kind::OP_SEMICOLON:
            peek();
            expr = std::make_shared<MhdExprEmpty>();
            return expr;
        // Compound statement.
        case MhdToken::Kind::OP_BRACE_OPEN:
            peek();
            expr = parse_expression_compound();
            return expr;
        // Selection statement.
        case MhdToken::Kind::KW_IF:
            peek();
            expr = parse_expression_cond_if();
            return expr;
        case MhdToken::Kind::KW_SWITCH:
            peek();
            expr = parse_expression_cond_switch();
            return expr;
        // Loop statement.
        case MhdToken::Kind::KW_WHILE:
            peek();
            expr = parse_expression_loop_while();
            return expr;
        case MhdToken::Kind::KW_DO:
            peek();
            expr = parse_expression_loop_do();
            return expr;
        case MhdToken::Kind::KW_FOR:
            peek();
            expr = parse_expression_loop_for();
            return expr;
        // Jump statement.
        case MhdToken::Kind::KW_BREAK:
            peek();
            expr = parse_expression_jump_break();
            return expr;
        case MhdToken::Kind::KW_CONTINUE:
            peek();
            expr = parse_expression_jump_continue();
            return expr;
        case MhdToken::Kind::KW_RETURN:
            peek();
            expr = parse_expression_jump_return();
            return expr;
        case MhdToken::Kind::KW_THROW:
            peek();
            expr = parse_expression_jump_throw();
            return expr;
        // Declaration or expression statements.
        default:
            expr = parse_expression();
            if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
                peek();
            } else {
                throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
            }
            return expr;
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
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
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
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind != MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdToken::Kind::OP_BRACE_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
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
                throw MhdParseException(MhdParserErr::ERR_UNEXP_DEFAULT);
            }
        } else {
            throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
        }
        if (m_token.m_kind != MhdToken::Kind::OP_COLON) {
            peek();
        } else {
            throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
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
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
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
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
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
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdToken::Kind::OP_SEMICOLON) {
        init = parse_expression();
    }
    if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdToken::Kind::OP_SEMICOLON) {
        cond = parse_expression();
    }
    if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdToken::Kind::OP_PAREN_CLOSE) {
        iter = parse_expression();
    }
    if (m_token.m_kind == MhdToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    body = parse();
    MhdExpr::Ptr expr;
    expr = std::make_shared<MhdExprLoopFor>(init, cond, iter, body);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_try_catch()
{
    // Parse TRY-CATCH expression.
    MhdExpr::Ptr try_block;
    MhdExpr::Ptr catch_block;
    MhdExpr::Ptr expr;
    try_block = parse();
    if (m_token.m_kind == MhdToken::Kind::KW_CATCH) {
        peek();
        ORCHID_ASSERT(0);
    }
    expr = std::make_shared<MhdExprTryCatch>(try_block, catch_block);
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
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
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
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
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
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    expr = std::make_shared<MhdExprJumpReturn>(expr);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_jump_throw()
{
    // Parse THROW jump expression.
    MhdExpr::Ptr expr;
    if (m_token.m_kind != MhdToken::Kind::OP_SEMICOLON) {
        expr = parse_expression();
    }
    if (m_token.m_kind == MhdToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
    }
    expr = std::make_shared<MhdExprJumpThrow>(expr);
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
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_ASG ||
            op == MhdToken::Kind::OP_OR_BW_ASG ||
            op == MhdToken::Kind::OP_XOR_BW_ASG ||
            op == MhdToken::Kind::OP_AND_BW_ASG ||
            op == MhdToken::Kind::OP_LSHIFT_ASG ||
            op == MhdToken::Kind::OP_RSHIFT_ASG ||
            op == MhdToken::Kind::OP_ADD_ASG ||
            op == MhdToken::Kind::OP_SUB_ASG ||
            op == MhdToken::Kind::OP_MUL_ASG ||
            op == MhdToken::Kind::OP_DIV_ASG ||
            op == MhdToken::Kind::OP_MOD_ASG) {
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
    MhdExpr::Ptr expr = parse_expression_binary_shift();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_LT || 
            op == MhdToken::Kind::OP_LTE ||
            op == MhdToken::Kind::OP_GT || 
            op == MhdToken::Kind::OP_GTE) {
        peek();
        expr = std::make_shared<MhdExprBinaryLogical>(op, expr, parse_expression_binary_shift());
    }
    return expr;
}
//--------------------------------------------------------------------------------------------------------
ORCHID_INTERNAL
MhdExpr::Ptr MhdParser::parse_expression_binary_shift()
{
    // Parse BINARY BITWISE SHIFT expressions.
    MhdToken::Kind op;
    MhdExpr::Ptr expr = parse_expression_binary_add_sub();
    while ((op = m_token.m_kind) == MhdToken::Kind::OP_LSHIFT ||
            op == MhdToken::Kind::OP_RSHIFT) {
        peek();
        expr = std::make_shared<MhdExprBinaryBitwise>(op, expr, parse_expression_binary_add_sub());
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
    MhdExpr::Ptr expr;
    switch (m_token.m_kind) {
        // Logic/Bitwise unary operations.
        case MhdToken::Kind::OP_NOT:
        case MhdToken::Kind::OP_NOT_BW:
            expr = parse_expression_unary_not();
            return expr;
        // Arithmetic unary operations.
        case MhdToken::Kind::OP_ADD:
        case MhdToken::Kind::OP_SUB:
            expr = parse_expression_unary_negate();
            return expr;
        // Parentheses/Brace expression.
        case MhdToken::Kind::OP_BRACE_OPEN:
            peek();
            expr = parse_expression_compound();
            return expr;
        case MhdToken::Kind::OP_PAREN_OPEN:
            peek();
            expr = parse_expression();
            if (m_token.m_kind == MhdToken::Kind::OP_PAREN_CLOSE) {
                peek();
            } else {
                throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
            }
            return expr;
        // Expression operand.
        default:
            expr = parse_expression_unary_factor();
            return expr;
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
            throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
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
                throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
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
                throw MhdParseException(MhdParserErr::ERR_UNEXP_TOKEN);
        }
    }
    peek();
    return exprs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################

#include <cmath>
#include <cstdio>
std::map<std::string, MhdDynamic> g_vars;
extern "C" void orchid_solver_scanner_test()
{
    g_vars["s"] = MhdDynamic(std::function<double(double)>((double(*)(double))sqrt));
    MhdParser p("{for(x=0;x<10;)x=x+1; x=s(2);}");
    //MhdParser p("{x=1+2;}");
    auto e = p.parse_wrap();
    auto g = e.get();
    printf("%s\n", g->eval().operator std::string().c_str());
}
int main() 
{
    orchid_solver_scanner_test();
    return 0;
}
