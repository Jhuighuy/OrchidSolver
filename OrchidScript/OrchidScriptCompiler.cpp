// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptCompiler.hpp"

#include <unordered_map>
#include <algorithm>
#include <vector>
#include <set>

namespace MhdLangOp = MhdScriptOp;

class MhdIncDec
{
    int& m_val;
public:
    MhdIncDec(int& val)
        : m_val(val) { ++m_val; }
    ~MhdIncDec() { --m_val; }
};  // class MhdIncDec

#define MhdCompileUnexpTokenError(a,...) MhdCompileError(a)
#define MhdCompileErrorUnexpContinue(a,...) MhdCompileError(a)
#define MhdCompileErrorUnexpBreak(a,...) MhdCompileError(a)
#define MhdCompileErrorUnexpReturn(a,...) MhdCompileError(a)

static const std::unordered_map<MhdLangKind, MhdLangOpcode> assignment_op{
    { MhdLangKind::OP_ADD_ASG, MhdLangOpcode::OP_ADD_ASG },
    { MhdLangKind::OP_SUB_ASG, MhdLangOpcode::OP_SUB_ASG },
    { MhdLangKind::OP_MUL_ASG, MhdLangOpcode::OP_MUL_ASG },
    { MhdLangKind::OP_DIV_ASG, MhdLangOpcode::OP_DIV_ASG },
    { MhdLangKind::OP_MOD_ASG, MhdLangOpcode::OP_MOD_ASG },
};

//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_wrap(MhdLangByteCode& bytecode)
{
    /// Compile statement.
    try {
        compile(bytecode);
    } catch (const MhdCompileError& compile_exc) {
        printf("%s\n%s\n", compile_exc.what(), m_tokenizer.m_text);
    }
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile(MhdLangByteCode& bytecode)
{
    /// Compile statement.
    /* Peek first token. */
    if (m_token.m_kind == MhdLangKind::NONE) {
        advance(); 
    }
    /* Empty statement or end of stream. */
    if (matched(MhdLangKind::OP_SEMICOLON, MhdLangKind::END)) {
        return;
    }
    /* Compound statement. */
    if (matched(MhdLangKind::OP_BRACE_OPEN)) {
        compile_expression_compound(bytecode);
        return;
    }
    /* Declaration statement. */
    if (matched(MhdLangKind::KW_LET)) {
        compile_expression_decl_var(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_FUNCTION)) {
        compile_expression_decl_function(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_CLASS)) {
        compile_expression_decl_class(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_NAMESPACE)) {
        compile_expression_decl_namespace(bytecode);
        return;
    }
    /* Selection statement. */
    if (matched(MhdLangKind::KW_IF)) {
        compile_expression_cond_if(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_SWITCH)) {
        compile_expression_cond_switch(bytecode);
        return;
    }
    /* Loop statement. */
    if (matched(MhdLangKind::KW_DO)) {
        compile_expression_loop_do(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_WHILE)) {
        compile_expression_loop_while(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_FOR)) {
        compile_expression_loop_for(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_FOREACH)) {
        compile_expression_loop_foreach(bytecode);
        return;
    }
    /* Try-Catch statement. */
    if (matched(MhdLangKind::KW_TRY)) {
        compile_expression_try_catch(bytecode);
        return;
    }
    /* Jump statement. */
    if (matched(MhdLangKind::KW_BREAK)) {
        compile_expression_jump_break(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_CONTINUE)) {
        compile_expression_jump_continue(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_RETURN)) {
        compile_expression_jump_return(bytecode);
        return;
    }
    if (matched(MhdLangKind::KW_THROW)) {
        compile_expression_jump_throw(bytecode);
        return;
    }
    /* Declaration or expression statements. */
    compile_expression(bytecode);
    expect(MhdLangKind::OP_SEMICOLON);
    bytecode.emit_code(MhdLangOpcode::DISCARD_1);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_program_wrap(MhdLangByteCode& bytecode)
{
    /// Compile program.
    try {
        compile_program(bytecode);
    } catch (const MhdCompileError& compile_exc) {
        printf("%s\n%s\n", compile_exc.what(), m_tokenizer.m_text);
    }
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_program(MhdLangByteCode& bytecode)
{
    /// Compile program.
    while (!matched(MhdLangKind::END)) {
        compile(bytecode);
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_compound(MhdLangByteCode& bytecode)
{
    /// Compile COMPOUND expression.
    while (!matched(MhdLangKind::OP_BRACE_CLOSE)) {
        compile(bytecode);
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_decl_var(MhdLangByteCode& bytecode)
{
    /// Compile local variable declaration `let x=y;`.
    expects(MhdLangKind::ID);
    const std::string ident{ m_token.m_value_str };
    advance();
    bytecode.emit_code(MhdLangOpcode::REF_CSTR);
    bytecode.emit_cstr(ident.c_str());
    if (matched(MhdLangKind::OP_ASG)) {
        compile_expression(bytecode);
        bytecode.emit_code(MhdLangOpcode::OPREF_ASG);
    } else {
        bytecode.emit_code(MhdLangOpcode::REF_DISCARD);
    }
    expect(MhdLangKind::OP_SEMICOLON);
    bytecode.emit_code(MhdLangOpcode::DISCARD_1);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_decl_function(MhdLangByteCode& bytecode)
{
    ORCHID_ASSERT(bytecode, 0);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_decl_class(MhdLangByteCode& bytecode)
{
    ORCHID_ASSERT(bytecode, 0);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_decl_namespace(MhdLangByteCode& bytecode)
{
    /// Compile NAMESPACE expression.
    ORCHID_ASSERT(bytecode, 0);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_cond_if(MhdLangByteCode& bytecode)
{
    /// Compile `if(x){}else{}` conditional expression.
    MhdLangByteCodeLabel else_label{};
    expect(MhdLangKind::OP_PAREN_OPEN);
    compile_expression_comma(bytecode);
    expect(MhdLangKind::OP_PAREN_CLOSE);
    bytecode.emit_code(MhdLangOpcode::JUMP_Z);
    bytecode.emit_addr(else_label);
    compile(bytecode);
    if (matched(MhdLangKind::KW_ELSE)) {
        MhdLangByteCodeLabel end_label{};
        bytecode.emit_code(MhdLangOpcode::JUMP);
        bytecode.emit_addr(end_label);
        bytecode.label(else_label);
        compile(bytecode);
        bytecode.label(end_label);
    } else {
        bytecode.label(else_label);
    }
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_cond_switch(MhdLangByteCode& bytecode)
{
    /// Compile `switch(x){}` conditional expression.
    ORCHID_ASSERT(0);
    //MhdLangExpr::Map switch_cases;
    //expect(MhdLangKind::OP_PAREN_OPEN);
    //compile_expression();
    //expect(MhdLangKind::OP_PAREN_CLOSE);
    //expect(MhdLangKind::OP_BRACE_OPEN);
    //while (true) {
    //    //switch_cases.emplace_back();
    //    if (matched(MhdLangKind::KW_CASE)) {
    //        compile_expression();
    //    } else {
    //        expect(MhdLangKind::KW_DEFAULT);
    //        nullptr;
    //    }
    //    expect(MhdLangKind::OP_COLON);
    //    //MhdLangExpr::Vec switch_case_exprs;
    //    while (true) {
    //        /* End of switch. */
    //        if (matched(MhdLangKind::OP_BRACE_CLOSE)) {
    //            //return std::make_shared<MhdLangExprSwitch>(switch_cond, switch_cases);
    //        }
    //        /* End of case. */
    //        if (matches(MhdLangKind::KW_CASE,
    //                    MhdLangKind::KW_DEFAULT)) {
    //            //auto expr = std::make_shared<MhdLangExprCompound>(switch_case_exprs, false);
    //            //switch_cases.back().second = expr;
    //            break;
    //        }
    //        //switch_case_exprs.push_back(compile());
    //    }
    //}
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_loop_do(MhdLangByteCode& bytecode)
{
    /// Compile `do{}while();` loop expression.
    MhdLangByteCodeLabel start_label{};
    MhdLangByteCodeLabel end_label{};
    bytecode.label(start_label);
    compile(bytecode);
    expect(MhdLangKind::KW_WHILE);
    expect(MhdLangKind::OP_PAREN_OPEN);
    compile_expression_comma(bytecode);
    expect(MhdLangKind::OP_PAREN_CLOSE);
    expect(MhdLangKind::OP_SEMICOLON);
    bytecode.emit_code(MhdLangOpcode::JUMP_NZ);
    bytecode.emit_addr(start_label);
    bytecode.label(end_label);
}
void MhdLangCompiler::compile_expression_loop_while(MhdLangByteCode& bytecode)
{
    /// Compile `while(){}` loop expression.
    MhdLangByteCodeLabel start_label{};
    MhdLangByteCodeLabel end_label{};
    bytecode.label(start_label);
    expect(MhdLangKind::OP_PAREN_OPEN);
    compile_expression_comma(bytecode);
    expect(MhdLangKind::OP_PAREN_CLOSE);
    bytecode.emit_code(MhdLangOpcode::JUMP_Z);
    bytecode.emit_addr(end_label);
    compile(bytecode);
    bytecode.emit_code(MhdLangOpcode::JUMP);
    bytecode.emit_addr(start_label);
    bytecode.label(end_label);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_loop_for(MhdLangByteCode& bytecode)
{
    /// Compile `for(;;){}` loop expression.
    expect(MhdLangKind::OP_PAREN_OPEN);
    if (!matched(MhdLangKind::OP_SEMICOLON)) {
        compile_expression_comma(bytecode);
        expect(MhdLangKind::OP_SEMICOLON);
    }
    MhdLangByteCodeLabel start_label{};
    MhdLangByteCodeLabel end_label{};
    bytecode.label(start_label);
    if (!matched(MhdLangKind::OP_SEMICOLON)) {
        compile_expression_comma(bytecode);
        expect(MhdLangKind::OP_SEMICOLON);
        bytecode.emit_code(MhdLangOpcode::JUMP_Z);
        bytecode.emit_addr(end_label);
    } 
    if (!matched(MhdLangKind::OP_PAREN_CLOSE)) {
        MhdLangByteCodeLabel body_label{};
        MhdLangByteCodeLabel iter_label{};
        bytecode.emit_code(MhdLangOpcode::JUMP);
        bytecode.emit_addr(body_label);
        bytecode.label(iter_label);
        compile_expression_comma(bytecode);
        expect(MhdLangKind::OP_PAREN_CLOSE);
        bytecode.emit_code(MhdLangOpcode::DISCARD_1);
        bytecode.emit_code(MhdLangOpcode::JUMP);
        bytecode.emit_addr(start_label);
        bytecode.label(body_label);
        compile(bytecode);
        bytecode.emit_code(MhdLangOpcode::JUMP);
        bytecode.emit_addr(iter_label);
    } else {
        compile(bytecode);
        bytecode.emit_code(MhdLangOpcode::JUMP);
        bytecode.emit_addr(start_label);
    }
    bytecode.label(end_label);
}
void MhdLangCompiler::compile_expression_loop_foreach(MhdLangByteCode& bytecode)
{
    /// Compile `foreach(x:y){}` loop expression.
    expect(MhdLangKind::OP_PAREN_OPEN);
    expects(MhdLangKind::ID);
    const std::string ident{ m_token.m_value_str };
    advance();
    expect(MhdLangKind::OP_COLON);
    compile_expression(bytecode);
    bytecode.emit_code(MhdLangOpcode::DUP_1X1);
    bytecode.emit_code(MhdLangOpcode::LOAD_CSTR);   /* b = y.end(); */
    bytecode.emit_cstr("end");
    bytecode.emit_code(MhdLangOpcode::INDEX_CALL_0);
    bytecode.emit_code(MhdLangOpcode::LOAD_CSTR);   /* a = y.begin(); */
    bytecode.emit_cstr("begin");
    bytecode.emit_code(MhdLangOpcode::INDEX_CALL_0);
    MhdLangByteCodeLabel start_label{};
    MhdLangByteCodeLabel end_label{};
    bytecode.label(start_label);
    bytecode.emit_code(MhdLangOpcode::DUP_2X1);
    bytecode.emit_code(MhdLangOpcode::OP_NEQ);
    bytecode.emit_code(MhdLangOpcode::JUMP_Z);
    bytecode.emit_addr(end_label);
    bytecode.emit_code(MhdLangOpcode::DUP_1X1);
    bytecode.emit_code(MhdLangOpcode::LOAD_CSTR);   /* x = a.value(); */
    bytecode.emit_cstr("value");
    bytecode.emit_code(MhdLangOpcode::INDEX_CALL_0);
    bytecode.emit_code(MhdLangOpcode::REF_CSTR);
    bytecode.emit_cstr(ident.c_str());
    bytecode.emit_code(MhdLangOpcode::OPREF_ASG);
    compile(bytecode);
    bytecode.emit_code(MhdLangOpcode::LOAD_CSTR);   /* a.advance(); */
    bytecode.emit_cstr("advance");
    bytecode.emit_code(MhdLangOpcode::INDEX_CALL_0);
    bytecode.emit_code(MhdLangOpcode::JUMP);
    bytecode.emit_addr(start_label);
    bytecode.emit_code(MhdLangOpcode::DISCARD_2);
    bytecode.label(end_label);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_try_catch(MhdLangByteCode& bytecode)
{
    /// Compile `try{}catch(x){}` expression.
    /// Catch section is optional, exception object is also optional.
    ORCHID_ASSERT(bytecode, 0);
    //std::string try_catch_arg;
    //compile();
    //if (matched(MhdLangKind::KW_CATCH)) {
    //    expect(MhdLangKind::OP_PAREN_OPEN);
    //    expects(MhdLangKind::ID);
    //    try_catch_arg = m_token.m_value_str;
    //    advance();
    //    expect(MhdLangKind::OP_PAREN_CLOSE);
    //    compile();
    //}
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_jump_break(MhdLangByteCode& bytecode)
{
    /// Compile `break;` jump expression.
    ORCHID_ASSERT(bytecode, 0);
}
void MhdLangCompiler::compile_expression_jump_continue(MhdLangByteCode& bytecode)
{
    /// Compile `continue;` jump expression.
    ORCHID_ASSERT(bytecode, 0);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_jump_return(MhdLangByteCode& bytecode)
{
    /// Compile `return x;` jump expression.
    if (matched(MhdLangKind::OP_SEMICOLON)) {
        bytecode.emit_code(MhdLangOpcode::LOAD_NULLPTR);
    } else {
        compile_expression(bytecode);
        expect(MhdLangKind::OP_SEMICOLON);
    }
    bytecode.emit_code(MhdLangOpcode::RET);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_jump_throw(MhdLangByteCode& bytecode)
{
    /// Compile `throw x;` jump expression.
    ORCHID_ASSERT(bytecode, 0);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression(MhdLangByteCode& bytecode)
{
    /// Compile general expressions.
    compile_expression_binary_asg(bytecode);
}
void MhdLangCompiler::compile_expression_comma(MhdLangByteCode& bytecode)
{
    /// Compile `,` expressions.
    /// These expressions discard values of each subexpression except of the last one.
    compile_expression_binary_asg(bytecode);
    //compile_expression_binary_asg(bytecode);
    //do {
    //    if (matched(MhdLangKind::OP_COMMA)) {
    //        bytecode.emit_code(MhdLangOpcode::DISCARD_1);
    //        compile_expression_binary_asg(bytecode);
    //        continue;
    //    }
    //} while (false);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_binary_asg(MhdLangByteCode& bytecode)
{
    /// Compile `_=` operators.
    compile_expression_ternary(bytecode, true);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_ternary(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile `?:` operator.
    compile_expression_or(bytecode, can_assign);
    while (true) {
        if (matched(MhdLangKind::OP_QUESTION)) {
            MhdLangByteCodeLabel else_label{};
            MhdLangByteCodeLabel end_label{};
            bytecode.emit_code(MhdLangOpcode::JUMP_Z);
            bytecode.emit_addr(else_label);
            compile_expression(bytecode);
            bytecode.emit_code(MhdLangOpcode::JUMP);
            bytecode.emit_addr(end_label);
            bytecode.label(else_label);
            expect(MhdLangKind::OP_COLON);
            compile_expression(bytecode);
            bytecode.label(end_label);
            continue;
        }
        break;
    }
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_or(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile `||` operator.
    /// RHS is evaluated only if LHS is false or is object with an overloaded `||` operator.
    compile_expression_and(bytecode, can_assign);
    while (true) {
        if (matched(MhdLangKind::OP_OR)) {
            /* @todo Add check for overloaded `||` operator. */
            MhdLangByteCodeLabel rhs_label{};
            MhdLangByteCodeLabel end_label{};
            bytecode.emit_code(MhdLangOpcode::JUMP_Z);
            bytecode.emit_addr(rhs_label);
            bytecode.emit_code(MhdLangOpcode::LOAD_TRUE);
            bytecode.emit_code(MhdLangOpcode::JUMP);
            bytecode.emit_addr(end_label);
            bytecode.label(rhs_label);
            compile_expression_and(bytecode);
            bytecode.label(end_label);
            continue;
        }
        break;
    }
}
void MhdLangCompiler::compile_expression_and(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile `&&` operator.
    /// RHS is evaluated only if LHS is true or is object with an overloaded `&&` operator.
    compile_expression_eq_neq(bytecode, can_assign);
    while (true) {
        if (matched(MhdLangKind::OP_AND)) {
            /* @todo Add check for overloaded `&&` operator. */
            MhdLangByteCodeLabel rhs_label{};
            MhdLangByteCodeLabel end_label{};
            bytecode.emit_code(MhdLangOpcode::JUMP_NZ);
            bytecode.emit_addr(rhs_label);
            bytecode.emit_code(MhdLangOpcode::LOAD_FALSE);
            bytecode.emit_code(MhdLangOpcode::JUMP);
            bytecode.emit_addr(end_label);
            bytecode.label(rhs_label);
            compile_expression_eq_neq(bytecode);
            bytecode.label(end_label);
            continue;
        }
        break;
    }
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_eq_neq(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile `==`, `!=` operators.
    compile_expression_lt_lte_gt_gte(bytecode, can_assign);
    while (true) {
        if (matched(MhdLangKind::OP_EQ)) {
            compile_expression_lt_lte_gt_gte(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_EQ);
            continue;
        }
        if (matched(MhdLangKind::OP_NEQ)) {
            compile_expression_lt_lte_gt_gte(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_NEQ);
            continue;
        }
        break;
    }
}
void MhdLangCompiler::compile_expression_lt_lte_gt_gte(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile `&lt;`, `&gt;`, `&lt;=`, `&gt;=` operators.
    compile_expression_add_sub(bytecode, can_assign);
    while (true) {
        if (matched(MhdLangKind::OP_LT)) {
            compile_expression_add_sub(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_LT);
            continue;
        }
        if (matched(MhdLangKind::OP_GT)) {
            compile_expression_add_sub(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_GT);
            continue;
        }
        if (matched(MhdLangKind::OP_LTE)) {
            compile_expression_add_sub(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_LTE);
            continue;
        }
        if (matched(MhdLangKind::OP_GTE)) {
            compile_expression_add_sub(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_GTE);
            continue;
        }
        break;
    }
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_add_sub(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile binary `+`, `-` operators.
    compile_expression_mul_div_mod(bytecode, can_assign);
    while (true) {
        if (matched(MhdLangKind::OP_ADD)) {
            compile_expression_mul_div_mod(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_ADD);
            continue;
        }
        if (matched(MhdLangKind::OP_SUB)) {
            compile_expression_mul_div_mod(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_SUB);
            continue;
        }
        break;
    }
}
void MhdLangCompiler::compile_expression_mul_div_mod(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile `*`, `/`, `%` operators.
    compile_expression_unary(bytecode, can_assign);
    while (true) {
        if (matched(MhdLangKind::OP_MUL)) {
            compile_expression_unary(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_MUL);
            continue;
        }
        if (matched(MhdLangKind::OP_DIV)) {
            compile_expression_unary(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_DIV);
            continue;
        }
        if (matched(MhdLangKind::OP_MOD)) {
            compile_expression_unary(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_MOD);
            continue;
        }
        break;
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_unary(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile unary expression.
    /* Logic unary expression. */
    if (matches(MhdLangKind::OP_NOT)) {
        compile_expression_unary_not(bytecode);
        return;
    }
    /* Arithmetic unary expression. */
    if (matches(MhdLangKind::OP_ADD,
                MhdLangKind::OP_SUB)) {
        compile_expression_unary_plus_minus(bytecode);
        return;
    }
    /* Parentheses expression. */
    if (matched(MhdLangKind::OP_PAREN_OPEN)) {
        compile_expression_comma(bytecode);
        expect(MhdLangKind::OP_PAREN_CLOSE);
        return;
    }
    /* Operand expression. */
    compile_operand(bytecode, can_assign);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_unary_not(MhdLangByteCode& bytecode)
{
    /// Compile `!` expression.
    if (matched(MhdLangKind::OP_NOT)) {
        compile_expression_unary(bytecode);
        bytecode.emit_code(MhdLangOpcode::OP_NOT);
        return;
    }
    unexpected();
}
void MhdLangCompiler::compile_expression_unary_plus_minus(MhdLangByteCode& bytecode)
{
    /// Compile unary `+`, `-` expression.
    if (matched(MhdLangKind::OP_ADD)) {
        compile_expression_unary(bytecode);
        bytecode.emit_code(MhdLangOpcode::OP_PLUS);
        return;
    }
    if (matched(MhdLangKind::OP_SUB)) {
        compile_expression_unary(bytecode);
        bytecode.emit_code(MhdLangOpcode::OP_MINUS);
        return;
    }
    unexpected();
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_operand(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile operand.
    compile_operand_primary(bytecode, can_assign);
    while (true) {
        /* Call expression factor. */
        if (matched(MhdLangKind::OP_PAREN_OPEN)) {
            compile_operand_factor_call(bytecode);
            continue;
        }
        /* Index expression factor. */
        if (matched(MhdLangKind::OP_BRACKET_OPEN)) {
            compile_operand_factor_index(bytecode, can_assign);
            continue;
        }
        if (matched(MhdLangKind::OP_DOT)) {
            compile_operand_factor_index_dot(bytecode, can_assign);
            continue;
        }
        break;
    }
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_operand_primary(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile primary operand.
    /// Primary operands are constant keywords, like `true`, numeric or string constants,
    /// functions, arrays, lists, maps or identifiers.
    /* Keyword primary operand. */
    if (matched(MhdLangKind::KW_TRUE)) {
        bytecode.emit_code(MhdLangOpcode::LOAD_TRUE);
        return;
    }
    if (matched(MhdLangKind::KW_FALSE)) {
        bytecode.emit_code(MhdLangOpcode::LOAD_FALSE);
        return;
    }
    if (matched(MhdLangKind::KW_NULL)) {
        bytecode.emit_code(MhdLangOpcode::LOAD_NULLPTR);
        return;
    }
    /* Constant primary operand. */
    if (matches(MhdLangKind::CT_INT)) {
        const unsigned int value{ m_token.m_value_uint };
        advance();
        switch (value) {
        case 0:
            bytecode.emit_code(MhdLangOpcode::LOAD_UI_0);
            return;
        case 1:
            bytecode.emit_code(MhdLangOpcode::LOAD_UI_1);
            return;
        case 2:
            bytecode.emit_code(MhdLangOpcode::LOAD_UI_2);
            return;
        case 3:
            bytecode.emit_code(MhdLangOpcode::LOAD_UI_3);
            return;
        }
        bytecode.emit_code(MhdLangOpcode::LOAD_UI32);
        bytecode.emit_ui32(value);
        return;
    }
    if (matches(MhdLangKind::CT_DBL)) {
        const double value{ m_token.m_value_dbl };
        advance();
        bytecode.emit_code(MhdLangOpcode::LOAD_FP64);
        bytecode.emit_fp64(value);
        return;
    }
    if (matches(MhdLangKind::CT_STR)) {
        const std::string value{ m_token.m_value_str };
        advance();
        bytecode.emit_code(MhdLangOpcode::LOAD_CSTR);
        bytecode.emit_cstr(value.c_str());
        return;
    }
    /* List/Map primary operand. */
    //if (matched(MhdLangKind::OP_BRACKET_OPEN)) {
    //    compile_operand_primary_list(bytecode);
    //    return false;
    //}
    //if (matched(MhdLangKind::OP_BRACE_OPEN)) {
    //    compile_operand_primary_map(bytecode);
    //    return false;
    //}
    /* Function primary operand. */
    if (matched(MhdLangKind::KW_FUNCTION)) {
        compile_operand_primary_func(bytecode);
        return;
    }
    if (matched(MhdLangKind::OP_BRACKET_OPEN)) {
        expect(MhdLangKind::OP_BRACKET_CLOSE);
        compile_operand_primary_func(bytecode);
        return;
    }
    /* Identifier primary operand. */
    if (matches(MhdLangKind::ID)) {
        const std::string ident{ m_token.m_value_str };
        advance();
        /* Assignment. */
        if (can_assign && matched(MhdLangKind::OP_ASG)) {
            compile_expression(bytecode);
            bytecode.emit_code(MhdLangOpcode::STORE_VAR_CSTR);
            bytecode.emit_cstr(ident.c_str());
            return;
        }
        /* No assignment. */
        bytecode.emit_code(MhdLangOpcode::LOAD_VAR_CSTR);
        bytecode.emit_cstr(ident.c_str());
        return;
    }
    unexpected();
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_operand_primary_list(MhdLangByteCode& bytecode)
{
    /// Compile list operand `%[]`.
    ORCHID_ASSERT(bytecode, 0);
}
void MhdLangCompiler::compile_operand_primary_map(MhdLangByteCode& bytecode)
{
    /// Compile map operand `%{}`.
    ORCHID_ASSERT(bytecode, 0);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_operand_primary_func(MhdLangByteCode& bytecode)
{
    /// Compile function operand `[](){}`.
    /// Arguments are assumed to be passed in direct order.
    std::vector<std::string> func_args;
    expect(MhdLangKind::OP_PAREN_OPEN);
    while (true) {
        if (matches(MhdLangKind::ID)) {
            const std::string func_arg{ m_token.m_value_str };
            if (std::find(func_args.cbegin(), func_args.cend(), 
                          func_arg) != func_args.cend()) {
                throw MhdCompileError(MhdLangError::ERR_FUNC_ARG_REDECL);
            }
            func_args.push_back(func_arg);
            advance();
            if (matched(MhdLangKind::OP_COMMA)) {
                continue;
            }
        }
        expect(MhdLangKind::OP_PAREN_CLOSE);
        break;
    }
    MhdLangByteCodeLabel func_label{};
    MhdLangByteCodeLabel end_label{};
    bytecode.emit_code(MhdLangOpcode::JUMP);
    bytecode.emit_addr(end_label);
    bytecode.label(func_label);
    std::for_each(func_args.crbegin(), func_args.crend(), 
                  [&](const std::string& func_arg) {
        bytecode.emit_code(MhdLangOpcode::REF_CSTR);
        bytecode.emit_cstr(func_arg.c_str());
        bytecode.emit_code(MhdLangOpcode::OPREF_ASG);
        bytecode.emit_code(MhdLangOpcode::DISCARD_1);
    });
    compile(bytecode);
    bytecode.emit_code(MhdLangOpcode::LOAD_NULLPTR);
    bytecode.emit_code(MhdLangOpcode::RET);
    bytecode.label(end_label);
    bytecode.emit_code(MhdLangOpcode::LOAD_FUNC);
    bytecode.emit_addr(func_label);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_operand_factor_call(MhdLangByteCode& bytecode)
{
    /// Compile call `x()` factor.
    std::uint16_t num_args{ 0 };
    for (;; ++num_args) {
        if (matched(MhdLangKind::OP_PAREN_CLOSE)) {
            break;
        }
        compile_expression(bytecode);
        if (matched(MhdLangKind::OP_COMMA)) {
            continue;
        }
    }
    emit_call(bytecode, num_args);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_operand_factor_index(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile index `x[y]` factor.
    std::uint16_t num_indices{ 1 };
    for (;; ++num_indices) {
        compile_expression(bytecode);
        if (matched(MhdLangKind::OP_BRACKET_CLOSE)) {
            break;
        }
        expect(MhdLangKind::OP_COMMA);
    }
    compile_operand_factor_index_end(bytecode, num_indices, can_assign);
}
void MhdLangCompiler::compile_operand_factor_index_dot(MhdLangByteCode& bytecode, bool can_assign)
{
    /// Compile index `x.y`, `x.operator_` factor.
    if (matches(MhdLangKind::ID)) {
        const std::string index{ m_token.m_value_str };
        advance();
        bytecode.emit_code(MhdLangOpcode::LOAD_CSTR);
        bytecode.emit_cstr(index.c_str());
        bytecode.emit_code(MhdLangOpcode::INDEX_1);
        compile_operand_factor_index_end(bytecode, 1, can_assign);
    }
    if (matched(MhdLangKind::KW_OPERATOR)) {
        const std::string index{ compile_operator() };
        bytecode.emit_code(MhdLangOpcode::LOAD_CSTR);
        bytecode.emit_cstr(index.c_str());
        bytecode.emit_code(MhdLangOpcode::INDEX_1);
        compile_operand_factor_index_end(bytecode, 1, can_assign);
    }
    unexpected();
}
void MhdLangCompiler::compile_operand_factor_index_end(MhdLangByteCode& bytecode, std::uint16_t num_indices, bool can_assign)
{
    /// Compile subscript `x.y` factor -- emit load or store code.
    /* Assignment. */
    if (can_assign && matched(MhdLangKind::OP_ASG)) {
        compile_expression(bytecode);
        emit_index_store(bytecode, num_indices);
        return;
    }
    /* Assignment with operation. */
    if (can_assign && matched(MhdLangKind::OP_ADD_ASG)) {
        emit_dupx1(bytecode, num_indices + 1);
        emit_index(bytecode, num_indices);
        compile_expression(bytecode);
        bytecode.emit_code(MhdLangOpcode::OP_ADD_ASG);
        emit_index_store(bytecode, num_indices);
        return;
    }
    /* No assignment. */
    emit_index(bytecode, num_indices);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
const char* MhdLangCompiler::compile_operator()
{
    /// Compile operator.
    /// Unary versions of operators (like `+x`) are distinguished by the curly braces.
    if (matched(MhdLangKind::OP_PAREN_OPEN)) {
        expect(MhdLangKind::OP_PAREN_CLOSE);
        return MhdLangOp::OP_CALL;
    }
    if (matched(MhdLangKind::OP_BRACKET_OPEN)) {
        expect(MhdLangKind::OP_BRACKET_CLOSE);
        return MhdLangOp::OP_INDEX;
    }
    /* Unary operators. */
    if (matched(MhdLangKind::OP_BRACE_OPEN)) {
        if (matched(MhdLangKind::OP_ADD)) {
            expect(MhdLangKind::OP_BRACE_CLOSE);
            return MhdLangOp::OP_PLUS;
        }
        if (matched(MhdLangKind::OP_SUB)) {
            expect(MhdLangKind::OP_BRACE_CLOSE);
            return MhdLangOp::OP_MINUS;
        }
        unexpected();
        return nullptr;
    }
    /* Other operators. */
    static const std::unordered_map<MhdLangKind, const char*> operators{
        { MhdLangKind::OP_NOT,     MhdLangOp::OP_NOT     },
        { MhdLangKind::OP_EQ,      MhdLangOp::OP_EQ      },
        { MhdLangKind::OP_NEQ,     MhdLangOp::OP_NEQ     },
        { MhdLangKind::OP_LT,      MhdLangOp::OP_LT      },
        { MhdLangKind::OP_GT,      MhdLangOp::OP_GT      },
        { MhdLangKind::OP_LTE,     MhdLangOp::OP_LTE     },
        { MhdLangKind::OP_GTE,     MhdLangOp::OP_GTE     },
        { MhdLangKind::OP_OR,      MhdLangOp::OP_OR      },
        { MhdLangKind::OP_AND,     MhdLangOp::OP_AND     },
        { MhdLangKind::OP_ADD,     MhdLangOp::OP_ADD     },
        { MhdLangKind::OP_SUB,     MhdLangOp::OP_SUB     },
        { MhdLangKind::OP_MUL,     MhdLangOp::OP_MUL     },
        { MhdLangKind::OP_DIV,     MhdLangOp::OP_DIV     },
        { MhdLangKind::OP_MOD,     MhdLangOp::OP_MOD     },
        { MhdLangKind::OP_INC,     MhdLangOp::OP_INC     },
        { MhdLangKind::OP_DEC,     MhdLangOp::OP_DEC     },
        { MhdLangKind::OP_ADD_ASG, MhdLangOp::OP_ADD_ASG },
        { MhdLangKind::OP_SUB_ASG, MhdLangOp::OP_SUB_ASG },
        { MhdLangKind::OP_MUL_ASG, MhdLangOp::OP_MUL_ASG },
        { MhdLangKind::OP_DIV_ASG, MhdLangOp::OP_DIV_ASG },
        { MhdLangKind::OP_MOD_ASG, MhdLangOp::OP_MOD_ASG },
    };
    const auto operators_iter{ operators.find(m_token.m_kind) };
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
