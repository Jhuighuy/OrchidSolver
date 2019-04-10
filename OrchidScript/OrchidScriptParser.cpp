// Orchid-- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptParser.hpp"
#include "OrchidScriptParserSyntax.hpp"

#include <stdexcept>
#include <exception>

struct MhdParseException : std::runtime_error
{
public:
    MhdParseException(...) : std::runtime_error("Hui!") {}
};  // struct MhdParseException

//########################################################################################################
//########################################################################################################
//########################################################################################################
ORCHID_INTERFACE
MhdScriptExpr::Ptr MhdScriptParser::parse_wrap()
{
    MhdScriptExpr::Ptr expr;
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
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse()
{
    // Parse statements.
    MhdScriptExpr::Ptr expr;
    switch (m_token.m_kind) {
        // Empty statement.
        case MhdScriptToken::Kind::OP_SEMICOLON:
            peek();
            expr = std::make_shared<MhdScriptExprEmpty>();
            return expr;
        // Compound statement.
        case MhdScriptToken::Kind::OP_BRACE_OPEN:
            peek();
            expr = parse_expression_compound();
            return expr;
        case MhdScriptToken::Kind::KW_NAMESPACE:
            peek();
            expr = parse_expression_namespace();
            return expr;
        // Selection statement.
        case MhdScriptToken::Kind::KW_IF:
            peek();
            expr = parse_expression_cond_if();
            return expr;
        case MhdScriptToken::Kind::KW_SWITCH:
            peek();
            expr = parse_expression_cond_switch();
            return expr;
        // Loop statement.
        case MhdScriptToken::Kind::KW_WHILE:
            peek();
            expr = parse_expression_loop_while();
            return expr;
        case MhdScriptToken::Kind::KW_DO:
            peek();
            expr = parse_expression_loop_do();
            return expr;
        case MhdScriptToken::Kind::KW_FOR:
            peek();
            expr = parse_expression_loop_for();
            return expr;
        // Jump statement.
        case MhdScriptToken::Kind::KW_BREAK:
            peek();
            expr = parse_expression_jump_break();
            return expr;
        case MhdScriptToken::Kind::KW_CONTINUE:
            peek();
            expr = parse_expression_jump_continue();
            return expr;
        case MhdScriptToken::Kind::KW_RETURN:
            peek();
            expr = parse_expression_jump_return();
            return expr;
        case MhdScriptToken::Kind::KW_THROW:
            peek();
            expr = parse_expression_jump_throw();
            return expr;
        // Declaration or expression statements.
        default:
            expr = parse_expression();
            if (m_token.m_kind == MhdScriptToken::Kind::OP_SEMICOLON) {
                peek();
            } else {
                throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
            }
            return expr;
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_compound()
{
    MhdScriptExpr::Vec exprs;
    while (m_token.m_kind != MhdScriptToken::Kind::OP_BRACE_CLOSE) {
        exprs.push_back(parse());
    }
    peek();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprCompound>(exprs);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_namespace()
{
    ORCHID_ASSERT(0);
    return nullptr;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_cond_if()
{
    // Parse IF conditional expression.
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Ptr then_branch;
    MhdScriptExpr::Ptr else_branch;
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    then_branch = parse();
    if (m_token.m_kind == MhdScriptToken::Kind::KW_ELSE) {
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
    // Parse SWITCH conditional expression.
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Map cases;
    MhdScriptExpr::Ptr case_default;
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind == MhdScriptToken::Kind::OP_BRACE_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    while (m_token.m_kind != MhdScriptToken::Kind::OP_BRACE_CLOSE) {
        MhdScriptExpr::Ptr* pcase_branch = nullptr;
        if (m_token.m_kind == MhdScriptToken::Kind::KW_CASE) {
            peek();
            cases.push_back({});
            cases.back().first = parse_expression();
            pcase_branch = &cases.back().second;
        } else if (m_token.m_kind == MhdScriptToken::Kind::KW_DEFAULT) {
            peek();
            if (case_default == nullptr) {
                pcase_branch = &case_default;
            } else {
                throw MhdParseException(MhdScriptError::ERR_UNEXP_DEFAULT);
            }
        } else {
            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
        }
        if (m_token.m_kind == MhdScriptToken::Kind::OP_COLON) {
            peek();
        } else {
            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
        }
        MhdScriptExpr::Vec case_branch_exprs;
        while (m_token.m_kind != MhdScriptToken::Kind::OP_BRACE_CLOSE &&
               m_token.m_kind != MhdScriptToken::Kind::KW_CASE &&
               m_token.m_kind != MhdScriptToken::Kind::KW_DEFAULT) {
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
    // Parse WHILE loop expression.
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Ptr body;
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
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
    // Parse DO-WHILE loop expression.
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Ptr body;
    body = parse();
    if (m_token.m_kind == MhdScriptToken::Kind::KW_WHILE) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    cond = parse_expression();
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
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
    // Parse FOR loop expression.
    MhdScriptExpr::Ptr init;
    MhdScriptExpr::Ptr cond;
    MhdScriptExpr::Ptr iter;
    MhdScriptExpr::Ptr body;
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdScriptToken::Kind::OP_SEMICOLON) {
        init = parse_expression();
    }
    if (m_token.m_kind == MhdScriptToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdScriptToken::Kind::OP_SEMICOLON) {
        cond = parse_expression();
    }
    if (m_token.m_kind == MhdScriptToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind != MhdScriptToken::Kind::OP_PAREN_CLOSE) {
        iter = parse_expression();
    }
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    body = parse();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprFor>(init, cond, iter, body);
    return expr; 
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_try_catch()
{
    // Parse TRY-CATCH expression.
    MhdScriptExpr::Ptr try_block;
    MhdScriptExpr::Ptr catch_block;
    std::string catch_arg;
    try_block = parse();
    if (m_token.m_kind == MhdScriptToken::Kind::KW_CATCH) {
        peek();
        if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_OPEN) {
            peek();
        } else {
            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
        }
        if (m_token.m_kind == MhdScriptToken::Kind::ID) {
            catch_arg = m_token.m_value_str;
            peek();
        } else {
            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
        }
        if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_CLOSE) {
            peek();
        } else {
            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
        }
    }
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
    // Parse BREAK jump expression.
    MhdScriptExpr::Ptr expr;
    if (m_token.m_kind != MhdScriptToken::Kind::OP_SEMICOLON) {
        expr = parse_expression();
    }
    if (m_token.m_kind == MhdScriptToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    expr = std::make_shared<MhdScriptExprBreak>(expr);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_jump_continue()
{
    // Parse CONTINUE jump expression.
    MhdScriptExpr::Ptr expr;
    if (m_token.m_kind == MhdScriptToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    expr = std::make_shared<MhdScriptExprContinue>();
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_jump_return()
{
    // Parse RETURN jump expression.
    MhdScriptExpr::Ptr expr;
    if (m_token.m_kind != MhdScriptToken::Kind::OP_SEMICOLON) {
        expr = parse_expression();
    }
    if (m_token.m_kind == MhdScriptToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    expr = std::make_shared<MhdScriptExprReturn>(expr);
    return expr; 
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_jump_throw()
{
    // Parse THROW jump expression.
    MhdScriptExpr::Ptr expr;
    if (m_token.m_kind != MhdScriptToken::Kind::OP_SEMICOLON) {
        expr = parse_expression();
    }
    if (m_token.m_kind == MhdScriptToken::Kind::OP_SEMICOLON) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
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
    // Parse expressions.
    return parse_expression_binary_asg();
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_comma()
{
    // Parse COMMA expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_asg();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_COMMA) {
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
    // Parse ASSIGNMENT expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_ternary();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_ASG ||
           op == MhdScriptToken::Kind::OP_OR_BW_ASG ||
           op == MhdScriptToken::Kind::OP_XOR_BW_ASG ||
           op == MhdScriptToken::Kind::OP_AND_BW_ASG ||
           op == MhdScriptToken::Kind::OP_LSHIFT_ASG ||
           op == MhdScriptToken::Kind::OP_RSHIFT_ASG ||
           op == MhdScriptToken::Kind::OP_ADD_ASG ||
           op == MhdScriptToken::Kind::OP_SUB_ASG ||
           op == MhdScriptToken::Kind::OP_MUL_ASG ||
           op == MhdScriptToken::Kind::OP_DIV_ASG ||
           op == MhdScriptToken::Kind::OP_MOD_ASG) {
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
    // Parse TERNARY expressions.
    MhdScriptExpr::Ptr expr = parse_expression_binary_or();
    while (m_token.m_kind == MhdScriptToken::Kind::OP_QUESTION) {
        peek();
        MhdScriptExpr::Ptr then_branch;
        MhdScriptExpr::Ptr else_branch;
        then_branch = parse_expression();
        if (m_token.m_kind == MhdScriptToken::Kind::OP_COLON) {
            peek();
        } else {
            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
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
    // Parse BINARY LOGICAL OR expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_and();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_OR) {
        peek();
        expr = std::make_shared<MhdScriptExprLogical>(op, expr, parse_expression_binary_and());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_and()
{
    // Parse BINARY LOGICAL AND expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_or_bw();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_AND) {
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
    // Parse BINARY BITWISE OR expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_xor_bw();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_OR_BW) {
        peek();
        expr = std::make_shared<MhdScriptExprBitwise>(op, expr, parse_expression_binary_xor_bw());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_xor_bw()
{
    // Parse BINARY BITWISE XOR expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_and_bw();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_XOR_BW) {
        peek();
        expr = std::make_shared<MhdScriptExprBitwise>(op, expr, parse_expression_binary_and_bw());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_and_bw()
{
    // Parse BINARY BITWISE AND expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_eq_neq();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_XOR_BW) {
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
    // Parse BINARY EQUALS or NOT EQUALS expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_lt_lte_gt_gte();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_EQ ||
           op == MhdScriptToken::Kind::OP_NEQ) {
        peek();
        expr = std::make_shared<MhdScriptExprLogical>(op, expr, parse_expression_binary_lt_lte_gt_gte());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_lt_lte_gt_gte()
{
    // Parse BINARY LESS(-EQUALS) or GREATER(-EQUALS) expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_shift();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_LT || 
           op == MhdScriptToken::Kind::OP_LTE ||
           op == MhdScriptToken::Kind::OP_GT || 
           op == MhdScriptToken::Kind::OP_GTE) {
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
    // Parse BINARY BITWISE SHIFT expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_add_sub();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_LSHIFT ||
           op == MhdScriptToken::Kind::OP_RSHIFT) {
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
    // Parse BINARY ADD or SUBTRACT expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_binary_mul_div_mod();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_ADD ||
           op == MhdScriptToken::Kind::OP_SUB) {
        peek();
        expr = std::make_shared<MhdScriptExprArithmetic>(op, expr, parse_expression_binary_mul_div_mod());
    }
    return expr;
}
MHD_INTERNAL 
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_binary_mul_div_mod()
{
    // Parse BINARY MULTIPLY or DIVIDE expressions.
    MhdScriptToken::Kind op;
    MhdScriptExpr::Ptr expr = parse_expression_unary();
    while (op = m_token.m_kind,
           op == MhdScriptToken::Kind::OP_MUL ||
           op == MhdScriptToken::Kind::OP_DIV ||
           op == MhdScriptToken::Kind::OP_MOD) {
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
    // Parse UNARY expression.
    MhdScriptExpr::Ptr expr;
    switch (m_token.m_kind) {
        // Logic/Bitwise unary operations.
        case MhdScriptToken::Kind::OP_NOT:
        case MhdScriptToken::Kind::OP_NOT_BW:
            expr = parse_expression_unary_not();
            return expr;
        // Arithmetic unary operations.
        case MhdScriptToken::Kind::OP_ADD:
        case MhdScriptToken::Kind::OP_SUB:
            expr = parse_expression_unary_negate();
            return expr;
        // Arithmetic unary increment/decrement.
        case MhdScriptToken::Kind::OP_INC:
        case MhdScriptToken::Kind::OP_DEC:
            ORCHID_ASSERT(0);
            return expr;
        // Parentheses/Brace expression.
        case MhdScriptToken::Kind::OP_BRACE_OPEN:
            peek();
            expr = parse_expression_compound();
            return expr;
        case MhdScriptToken::Kind::OP_PAREN_OPEN:
            peek();
            expr = parse_expression();
            if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_CLOSE) {
                peek();
            } else {
                throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
            }
            return expr;
        // Expression operand.
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
    // Parse UNARY NOT expression.
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprNot>(m_token.m_kind, parse_expression_unary());
    peek();
    return expr;
}
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_negate()
{
    // Parse UNARY NEGATE expression.
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprNegate>(m_token.m_kind, parse_expression_unary());
    peek();
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_operand()
{
    // Parse OPERAND expression.
    MhdScriptExpr::Ptr expr;
    switch (m_token.m_kind) {
        // Constant expression operand.
        case MhdScriptToken::Kind::CT_NIL:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal());
            peek();
            return expr;
        case MhdScriptToken::Kind::CT_LGC:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal(m_token.m_value_int != 0));
            peek();
            return expr;
        case MhdScriptToken::Kind::CT_INT:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal(m_token.m_value_int));
            peek();
            return expr;
        case MhdScriptToken::Kind::CT_DBL:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal(m_token.m_value_dbl));
            peek();
            return expr;
        case MhdScriptToken::Kind::CT_STR:
            expr = std::make_shared<MhdScriptExprConst>(MhdScriptVal(m_token.m_value_str));
            peek();
            return expr;
        // Identifier expression operand.
        case MhdScriptToken::Kind::ID:
            expr = std::make_shared<MhdScriptExprIdent>(m_token.m_value_str);
            peek();
            return expr;
        // Function expression operand.
        case MhdScriptToken::Kind::OP_BRACKET_OPEN:
            peek();
            expr = parse_expression_unary_operand_func();
            return expr;
        // Error case.
        default:
            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
}
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_operand_func()
{
    // Parse FUNCTION OPERAND expression.
    if (m_token.m_kind == MhdScriptToken::Kind::OP_BRACKET_CLOSE) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_OPEN) {
        peek();
    } else {
        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    std::set<std::string> args;
    while (m_token.m_kind != MhdScriptToken::Kind::OP_PAREN_CLOSE) {
        if (m_token.m_kind == MhdScriptToken::Kind::ID) {
            if (args.count(m_token.m_value_str) == 0) {
                args.insert(m_token.m_value_str);
            } else {
                throw MhdParseException(MhdScriptError::ERR_FUNC_ARG_REDECL);
            }
            peek();
        } else {
            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
        }
        switch (m_token.m_kind) {
            case MhdScriptToken::Kind::OP_COMMA:
                peek();
                break;
            case MhdScriptToken::Kind::OP_PAREN_CLOSE:
                break;
            default:
                throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
        }
    }
    peek();
    MhdScriptExpr::Ptr body = parse();
    MhdScriptExpr::Ptr expr;
    expr = std::make_shared<MhdScriptExprConstFunc>(std::vector<std::string>{args.cbegin(), args.cend()}, body);
    return expr;
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
MhdScriptExpr::Ptr 
MhdScriptParser::parse_expression_unary_factor()
{
    // Parse OPERAND expression FACTOR.
    MhdScriptExpr::Ptr expr = parse_expression_unary_operand();
    while (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_OPEN ||
           m_token.m_kind == MhdScriptToken::Kind::OP_BRACKET_OPEN ||
           m_token.m_kind == MhdScriptToken::Kind::OP_DOT) {
        switch (m_token.m_kind) {
            case MhdScriptToken::Kind::OP_PAREN_OPEN:
                peek();
                expr = std::make_shared<MhdScriptExprCall>(expr, parse_expression_unary_factor_call());
                break;
            case MhdScriptToken::Kind::OP_BRACKET_OPEN:
                peek();
                expr = std::make_shared<MhdScriptExprIndex>(expr, parse_expression_unary_factor_index());
                break;
            case MhdScriptToken::Kind::OP_DOT:
                peek();
                expr = std::make_shared<MhdScriptExprIndex>(expr, parse_expression_unary_factor_subscript());
            default:
                break;
        }
    }
    return expr;
}
MHD_INTERNAL
MhdScriptExpr::Vec 
MhdScriptParser::parse_expression_unary_factor_call()
{
    // Parse CALL expression FACTOR.
    MhdScriptExpr::Vec exprs;
    while (m_token.m_kind != MhdScriptToken::Kind::OP_PAREN_CLOSE) {
        exprs.push_back(parse_expression());
        switch (m_token.m_kind) {
            case MhdScriptToken::Kind::OP_COMMA:
                peek();
                break;
            case MhdScriptToken::Kind::OP_PAREN_CLOSE:
                break;
            default:
                throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
        }
    }
    peek();
    return exprs;
}
MHD_INTERNAL
MhdScriptExpr::Vec 
MhdScriptParser::parse_expression_unary_factor_index()
{
    // Parse INDEX expression FACTOR.
    MhdScriptExpr::Vec exprs;
    while (m_token.m_kind != MhdScriptToken::Kind::OP_BRACKET_CLOSE) {
        exprs.push_back(parse_expression());
        switch (m_token.m_kind) {
            case MhdScriptToken::Kind::OP_COMMA:
                peek();
                break;
            case MhdScriptToken::Kind::OP_BRACKET_CLOSE:
                break;
            default:
                throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
        }
    }
    peek();
    return exprs;
}
MHD_INTERNAL
MhdScriptExpr::Vec 
MhdScriptParser::parse_expression_unary_factor_subscript()
{
    // Parse SUBSCRIPT expression FACTOR.
    MhdScriptExpr::Vec exprs;
    switch (m_token.m_kind) {
        case MhdScriptToken::Kind::ID:
            exprs.push_back(std::make_shared<MhdScriptExprConst>(m_token.m_value_str));
            peek();
            break;
        case MhdScriptToken::Kind::KW_OPERATOR:
            peek();
            switch (m_token.m_kind) {
                case MhdScriptToken::Kind::OP_ADD:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator+"));
                    break;
                case MhdScriptToken::Kind::OP_ADD_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator+="));
                    break;
                case MhdScriptToken::Kind::OP_INC:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator++"));
                    break;
                case MhdScriptToken::Kind::OP_SUB:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator-"));
                    break;
                case MhdScriptToken::Kind::OP_SUB_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator-="));
                    break;
                case MhdScriptToken::Kind::OP_DEC:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator--"));
                    break;
                case MhdScriptToken::Kind::OP_MUL:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator*"));
                    break;
                case MhdScriptToken::Kind::OP_MUL_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator*="));
                    break;
                case MhdScriptToken::Kind::OP_DIV:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator/"));
                    break;
                case MhdScriptToken::Kind::OP_DIV_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator/="));
                    break;
                case MhdScriptToken::Kind::OP_MOD:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator%"));
                    break;
                case MhdScriptToken::Kind::OP_MOD_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator%="));
                    break;
                case MhdScriptToken::Kind::OP_NOT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator!"));
                    break;
                case MhdScriptToken::Kind::OP_NOT_BW:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator~"));
                    break;
                case MhdScriptToken::Kind::OP_EQ:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator=="));
                    break;
                case MhdScriptToken::Kind::OP_NEQ:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator!="));
                    break;
                case MhdScriptToken::Kind::OP_LT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator<"));
                    break;
                case MhdScriptToken::Kind::OP_LTE:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator<="));
                    break;
                case MhdScriptToken::Kind::OP_GT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator>"));
                    break;
                case MhdScriptToken::Kind::OP_GTE:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator>="));
                    break;
                case MhdScriptToken::Kind::OP_LSHIFT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator<<"));
                    break;
                case MhdScriptToken::Kind::OP_LSHIFT_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator<<="));
                    break;
                case MhdScriptToken::Kind::OP_RSHIFT:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator>>"));
                    break;
                case MhdScriptToken::Kind::OP_RSHIFT_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator>>="));
                    break;
                case MhdScriptToken::Kind::OP_AND:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator&&"));
                    break;
                case MhdScriptToken::Kind::OP_AND_BW:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator&"));
                    break;
                case MhdScriptToken::Kind::OP_AND_BW_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator&="));
                    break;
                case MhdScriptToken::Kind::OP_OR:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator||"));
                    break;
                case MhdScriptToken::Kind::OP_OR_BW:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator|"));
                    break;
                case MhdScriptToken::Kind::OP_OR_BW_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator|="));
                    break;
                case MhdScriptToken::Kind::OP_XOR_BW:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator^"));
                    break;
                case MhdScriptToken::Kind::OP_XOR_BW_ASG:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator^="));
                    break;
                case MhdScriptToken::Kind::OP_PAREN_OPEN:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator()"));
                    if (m_token.m_kind == MhdScriptToken::Kind::OP_PAREN_CLOSE) {
                        peek();
                    } else {
                        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
                    }
                    break;
                case MhdScriptToken::Kind::OP_BRACKET_OPEN:
                    peek();
                    exprs.push_back(std::make_shared<MhdScriptExprConst>("operator[]"));
                    if (m_token.m_kind == MhdScriptToken::Kind::OP_BRACKET_CLOSE) {
                        peek();
                    } else {
                        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
                    }
                    break;
                case MhdScriptToken::Kind::OP_BRACE_OPEN:
                    peek();
                    switch (m_token.m_kind) {
                        case MhdScriptToken::Kind::OP_ADD:
                            peek();
                            exprs.push_back(std::make_shared<MhdScriptExprConst>("operator{+}"));
                            break;
                        case MhdScriptToken::Kind::OP_INC:
                            peek();
                            exprs.push_back(std::make_shared<MhdScriptExprConst>("operator{++}"));
                            break;
                        case MhdScriptToken::Kind::OP_SUB:
                            peek();
                            exprs.push_back(std::make_shared<MhdScriptExprConst>("operator{-}"));
                            break;
                        case MhdScriptToken::Kind::OP_DEC:
                            peek();
                            exprs.push_back(std::make_shared<MhdScriptExprConst>("operator{--}"));
                            break;
                        default:
                            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
                    }
                    if (m_token.m_kind == MhdScriptToken::Kind::OP_BRACE_CLOSE) {
                        peek();
                    } else {
                        throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
                    }
                    break;
                default:
                    throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
            }
            break;
        default:
            throw MhdParseException(MhdScriptError::ERR_UNEXP_TOKEN);
    }
    return exprs;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################

#include "OrchidScriptValue.hpp"
#include <cmath>
#include <cstdio>
std::map<std::string, MhdScriptVal> g_vars;
extern "C" void orchid_solver_scanner_test()
{
    //g_vars["s"] = MhdScriptVal(std::function<MhdScriptVal()>([]() -> MhdScriptVal {
    //    return MhdScriptVal(std::function<int()>([](){ return 1488; }));
    //}));
    //MhdScriptParser p("{for(x=0;x<10;)x=x+1; x=s()();}");
    MhdScriptParser p("{y = 2;[](x){ y = 1; x; }(1234)+y;}");
    //MhdScriptParser p("{x=1+2;}");
    auto e = p.parse_wrap();
    auto g = e.get();
    printf("%s\n", g->eval().operator std::string().c_str());
}
int main() 
{
    //printf("%d\n", sizeof(true + 1ul));
    MhdScriptVal a1(100);
    MhdScriptVal a2(2.0);
    a2[1] = 200.0;
    printf("%s\n", (a1 = a1 + a2).operator std::string().c_str());

    orchid_solver_scanner_test();
    return 0;
}
