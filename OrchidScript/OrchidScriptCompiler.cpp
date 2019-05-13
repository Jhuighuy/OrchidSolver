// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptCompiler.hpp"

#include <unordered_map>
#include <algorithm>
#include <vector>
#include <set>

using MhdLangKind = MhdScriptKind;
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

//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_wrap()
{
    /// Compile statement.
    try {
        compile();
    } catch (const MhdCompileError& compile_exc) {
        printf("%s\n%s\n", compile_exc.what(), m_tokenizer.m_text);
    }
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile(MhdLangByteCode& bytecode)
{
    /// Compile statement.
    if (m_token.m_kind == MhdLangKind::NONE) {
        /* Peek first token. */
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
    compile_expression();
    expect(MhdLangKind::OP_SEMICOLON);
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
void MhdLangCompiler::compile_expression_decl_function(MhdLangByteCode& bytecode)
{
    ORCHID_ASSERT(0);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_decl_class(MhdLangByteCode& bytecode)
{
    ORCHID_ASSERT(0);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_decl_namespace(MhdLangByteCode& bytecode)
{
    /// Compile NAMESPACE expression.
    ORCHID_ASSERT(0);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_cond_if(MhdLangByteCode& bytecode)
{
    /// Compile `if(x){}else{}` conditional expression.
    MhdLangByteCodeLabel else_label{};
    expect(MhdLangKind::OP_PAREN_OPEN);
    compile_expression(bytecode);
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
    /// Compile SWITCH conditional expression.
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
    compile_expression(bytecode);
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
    compile_expression(bytecode);
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
        compile_expression(bytecode);
        expect(MhdLangKind::OP_SEMICOLON);
    }
    MhdLangByteCodeLabel start_label{};
    MhdLangByteCodeLabel end_label{};
    bytecode.label(start_label);
    if (!matched(MhdLangKind::OP_SEMICOLON)) {
        compile_expression(bytecode);
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
        compile_expression(bytecode);
        expect(MhdLangKind::OP_PAREN_CLOSE);
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
    ORCHID_ASSERT(0);
    //std::string foreach_id;
    //expect(MhdLangKind::OP_PAREN_OPEN);
    //expects(MhdLangKind::ID);
    //foreach_id = m_token.m_value_str;
    //advance();
    //expect(MhdLangKind::OP_COLON);
    //compile_expression();
    //expect(MhdLangKind::OP_PAREN_CLOSE);
    //{
    //    MhdIncDec inside_loop{ m_inside_loop };
    //   compile();
    //}
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_try_catch(MhdLangByteCode& bytecode)
{
    /// Compile `try{}catch(x){}` expression.
    /// Catch section is optional, exception object is also optional.
    ORCHID_ASSERT(0);
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
    ORCHID_ASSERT(0);
}
void MhdLangCompiler::compile_expression_jump_continue(MhdLangByteCode& bytecode)
{
    /// Compile `continue;` jump expression.
    ORCHID_ASSERT(0);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_jump_return(MhdLangByteCode& bytecode)
{
    /// Compile `return x;` jump expression.
    ORCHID_ASSERT(0);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_jump_throw(MhdLangByteCode& bytecode)
{
    /// Compile `throw x;` jump expression.
    ORCHID_ASSERT(0);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression(MhdLangByteCode& bytecode)
{
    /// Compile expressions.
    compile_expression_comma(bytecode);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_comma(MhdLangByteCode& bytecode)
{
    /// Compile `,` expressions.
    /// These expressions discard values of each subexpression except of the last one.
    compile_expression_binary_asg(bytecode);
    do {
        if (matched(MhdLangKind::OP_COMMA)) {
            bytecode.emit_code(MhdLangOpcode::DISCARD_1);
            compile_expression_binary_asg(bytecode);
            continue;
        }
    } while (false);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_binary_asg(MhdLangByteCode& bytecode)
{
    /// Compile `_=` operators.
    do {
        if (matched(MhdLangKind::OP_ASG)) {
            compile_expression_ternary(bytecode);
            compile_expression_ternary(bytecode);
            continue;
        }
        /* No assignment operator. */
        compile_expression_ternary(bytecode);
    } while (false);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_ternary(MhdLangByteCode& bytecode)
{
    /// Compile `?:` operator.
    compile_expression_binary_or(bytecode);
    do {
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
    } while (false);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_binary_or(MhdLangByteCode& bytecode)
{
    /// Compile `||` operator.
    /// RHS is evaluated only if LHS is false or is object with an overloaded `||` operator.
    compile_expression_binary_and(bytecode);
    do {
        if (matched(MhdLangKind::OP_OR)) {
            /* @todo Add check for overloaded `||` operator. */
            MhdLangByteCodeLabel rhs_label{};
            MhdLangByteCodeLabel end_label{};
            bytecode.emit_code(MhdLangOpcode::JUMP_Z);
            bytecode.emit_addr(rhs_label);
            bytecode.emit_code(MhdLangOpcode::LOAD_TRUE);
            bytecode.emit_code(MhdLangOpcode::JUMP);
            bytecode.emit_addr(end_label);
            compile_expression_binary_and(bytecode);
            bytecode.label(end_label);
            continue;
        }
    } while (false);
}
void MhdLangCompiler::compile_expression_binary_and(MhdLangByteCode& bytecode)
{
    /// Compile `&&` operator.
    /// RHS is evaluated only if LHS is true or is object with an overloaded `&&` operator.
    compile_expression_binary_eq_neq(bytecode);
    do {
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
            compile_expression_binary_eq_neq(bytecode);
            bytecode.label(end_label);
            continue;
        }
    } while (false);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_binary_eq_neq(MhdLangByteCode& bytecode)
{
    /// Compile `==`, `!=` operators.
    compile_expression_binary_lt_lte_gt_gte(bytecode);
    do {
        if (matched(MhdLangKind::OP_EQ)) {
            compile_expression_binary_lt_lte_gt_gte(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_EQ);
            continue;
        }
        if (matched(MhdLangKind::OP_NEQ)) {
            compile_expression_binary_lt_lte_gt_gte(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_NEQ);
            continue;
        }
    } while (false);
}
void MhdLangCompiler::compile_expression_binary_lt_lte_gt_gte(MhdLangByteCode& bytecode)
{
    /// Compile `&lt;`, `&gt;`, `&lt;=`, `&gt;=` operators.
    compile_expression_binary_add_sub(bytecode);
    do {
        if (matched(MhdLangKind::OP_LT)) {
            compile_expression_binary_add_sub(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_LT);
            continue;
        }
        if (matched(MhdLangKind::OP_GT)) {
            compile_expression_binary_add_sub(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_GT);
            continue;
        }
        if (matched(MhdLangKind::OP_LTE)) {
            compile_expression_binary_add_sub(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_LTE);
            continue;
        }
        if (matched(MhdLangKind::OP_GTE)) {
            compile_expression_binary_add_sub(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_GTE);
            continue;
        }
    } while (false);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_binary_add_sub(MhdLangByteCode& bytecode)
{
    /// Compile binary `+`, `-` operators.
    compile_expression_binary_mul_div_mod(bytecode);
    do {
        if (matched(MhdLangKind::OP_ADD)) {
            compile_expression_binary_mul_div_mod(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_ADD);
            continue;
        }
        if (matched(MhdLangKind::OP_SUB)) {
            compile_expression_binary_mul_div_mod(bytecode);
            bytecode.emit_code(MhdLangOpcode::OP_SUB);
            continue;
        }
    } while (false);
}
void MhdLangCompiler::compile_expression_binary_mul_div_mod(MhdLangByteCode& bytecode)
{
    /// Compile `*`, `/`, `%` operators.
    compile_expression_unary(bytecode);
    do {
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
    } while (false);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_unary(MhdLangByteCode& bytecode)
{
    /// Compile UNARY expression.
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
    /* Increment/decrement expression. */
    if (matches(MhdLangKind::OP_INC,
                MhdLangKind::OP_DEC)) {
        ORCHID_ASSERT(0);
        return;
    }
    /* Parentheses/Brace expression. */
    if (matched(MhdLangKind::OP_BRACE_OPEN)) {
        compile_expression_compound(bytecode);
        return;
    }
    if (matched(MhdLangKind::OP_PAREN_OPEN)) {
        compile_expression(bytecode);
        expect(MhdLangKind::OP_PAREN_CLOSE);
        return;
    }
    /* Operand expression. */
    compile_expression_operand(bytecode);
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
void MhdLangCompiler::compile_expression_operand(MhdLangByteCode& bytecode)
{
    /// Compile operand expression.
    compile_expression_operand_primary(bytecode);
    do {
        /* Call expression factor. */
        if (matched(MhdLangKind::OP_PAREN_OPEN)) {
            compile_expression_operand_factor_call(bytecode);
            continue;
        }
        /* Index expression factor. */
        if (matched(MhdLangKind::OP_BRACKET_OPEN)) {
            compile_expression_operand_factor_index(bytecode);
            continue;
        }
        if (matched(MhdLangKind::OP_DOT)) {
            compile_expression_operand_factor_subscript(bytecode);
            continue;
        }
    } while (false);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_operand_primary(MhdLangByteCode& bytecode)
{
    /// Compile primary operand expression.
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
        const unsigned int value{ m_token.m_value_int };
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
    /* Function primary operand. */
    if (matched(MhdLangKind::KW_FUNCTION)) {
        compile_expression_operand_primary_func();
        return;
    }
    if (matched(MhdLangKind::OP_BRACKET_OPEN)) {
        expect(MhdLangKind::OP_BRACKET_CLOSE);
        compile_expression_operand_primary_func();
        return;
    }
    /* Identifier primary operand. */
    if (matched(MhdLangKind::KW_LET)) {
        expects(MhdLangKind::ID);
        //auto expr{ std::make_shared<MhdLangExprIdent>(m_token.m_value_str, true) };
        advance();
        //return expr;
    }
    if (matches(MhdLangKind::ID)) {
        //auto expr{ std::make_shared<MhdLangExprIdent>(m_token.m_value_str) };
        advance();
        //return expr;
    }
    unexpected();
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_operand_primary_func()
{
    /// Compile primary function operand expression.
    std::vector<std::string> func_args;
    expect(MhdLangKind::OP_PAREN_OPEN);
    while (true) {
        if (matches(MhdLangKind::ID)) {
            //const auto& func_arg = m_token.m_value_str;
            //if (std::find(func_args.cbegin(), 
            //              func_args.cend(), func_arg) != func_args.cend()) {
            //    throw MhdCompileError(MhdLangError::ERR_FUNC_ARG_REDECL);
            //}
            //func_args.push_back(func_arg);
            advance();
            if (matched(MhdLangKind::OP_COMMA)) {
                continue;
            }
        }
        if (matched(MhdLangKind::OP_PAREN_CLOSE)) {
            break;
        }
        unexpected();
    }
    //void func_body;
    {
        MhdIncDec inside_func{ m_inside_func };
        //func_body = compile();
    }
    //return std::make_shared<MhdLangExprConstFunc>(func_args, func_body);
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_operand_primary_list()
{
    ORCHID_ASSERT(0);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangCompiler::compile_expression_operand_factor_call(MhdLangByteCode& bytecode)
{
    /// Compile CALL expression factor.
    //MhdLangExpr::Vec call_args;
    /* Subscript or Index is followed by the call.
     * Pass the accessed object as a first argument. 
     * @todo This should be moved out of compiler, also condition is incorrect. */
    //MhdLangExprIndex* called_expr_index;
    //if (called_expr_index = dynamic_cast<MhdLangExprIndex*>(called_expr.get()),
    //    called_expr_index != nullptr) {
    //    call_args.push_back(called_expr_index->m_array);
    //}
    while (true) {
        if (matched(MhdLangKind::OP_PAREN_CLOSE)) {
            //return std::make_shared<MhdLangExprCall>(called_expr, call_args);
        }
        //call_args.push_back(compile_expression());
        if (matched(MhdLangKind::OP_COMMA)) {
            continue;
        }
    }
}
//--------------------------------------------------------------------------------------------------------
void MhdLangCompiler::compile_expression_operand_factor_index(MhdLangByteCode& bytecode)
{
    /// Compile index expression factor.
    /// At least one index is expected.
    //MhdLangExpr::Vec indices;
    while (true) {
        //indices.push_back(compile_expression());
        if (matched(MhdLangKind::OP_BRACKET_CLOSE)) {
            //return std::make_shared<MhdLangExprIndex>(indexed_expr, indices);
        }
        expect(MhdLangKind::OP_COMMA);
    }
}
void MhdLangCompiler::compile_expression_operand_factor_subscript(MhdLangByteCode& bytecode)
{
    /// Compile subscript expression factor.
    /// Subscript factors are translated into the index factors.
    if (matches(MhdLangKind::ID)) {
        //auto index_expr{ std::make_shared<MhdLangExprConst>(m_token.m_value_str) };
        advance();
        //return std::make_shared<MhdLangExprIndex>(indexed_expr, MhdLangExpr::Vec{ index_expr });
    }
    if (matched(MhdLangKind::KW_OPERATOR)) {
        //auto index_expr{ std::make_shared<MhdLangExprConst>(compile_operator()) };
        //return std::make_shared<MhdLangExprIndex>(indexed_expr, MhdLangExpr::Vec{ index_expr });
    }
    unexpected();
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
const char* MhdLangCompiler::compile_operator()
{
    /// Compile operator.
    /// Unary or postfix versions of operators (like `+x`, `y--`) are distinguished by the curly braces.
    if (matched(MhdLangKind::OP_PAREN_OPEN)) {
        expect(MhdLangKind::OP_PAREN_CLOSE);
        return MhdLangOp::OP_CALL;
    }
    if (matched(MhdLangKind::OP_BRACKET_OPEN)) {
        expect(MhdLangKind::OP_BRACKET_CLOSE);
        return MhdLangOp::OP_INDEX;
    }
    if (matched(MhdLangKind::OP_BRACE_OPEN)) {
        /* Unary operators. */
        if (matched(MhdLangKind::OP_ADD)) {
            expect(MhdLangKind::OP_BRACE_CLOSE);
            return MhdLangOp::OP_PLUS;
        }
        if (matched(MhdLangKind::OP_SUB)) {
            expect(MhdLangKind::OP_BRACE_CLOSE);
            return MhdLangOp::OP_MINUS;
        }
        /* Postfix operators. */
        if (matched(MhdLangKind::OP_INC)) {
            expect(MhdLangKind::OP_BRACE_CLOSE);
            return MhdLangOp::OP_INC_POSTFIX;
        }
        if (matched(MhdLangKind::OP_DEC)) {
            expect(MhdLangKind::OP_BRACE_CLOSE);
            return MhdLangOp::OP_DEC_POSTFIX;
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
