// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScript.hpp"
#include "OrchidScriptCompiler.hpp"
#include "OrchidScriptValue.hpp"
#include "OrchidScriptVar.hpp"

#include <vector>
#include <algorithm>
#include <utility>

MhdScriptVal print(const std::vector<MhdScriptVal>& args)
{
    for (auto& arg: args) {
        puts(arg.operator std::string().c_str());
    }
    return MhdScriptVal();
}

typedef MhdScriptVal MhdLangVal;
typedef MhdScriptRef MhdLangRef;

struct MhdLangVM
{
public:
    MhdLangByteCode m_bytecode;
    std::vector<MhdLangVal> m_val_stack;
    std::vector<MhdLangRef> m_ref_stack;
    std::uint32_t m_ip = 0;
public:
    void interpret();
private:
    MhdLangOpcode decode_op() {
        std::uint8_t op_byte = m_bytecode[m_ip++];
        return static_cast<MhdLangOpcode>(op_byte);
    }
    std::uint8_t decode_ui8() {
        return m_bytecode[m_ip++];
    }
    std::uint16_t decode_ui16() {
        const auto ui16 = *reinterpret_cast<std::uint16_t*>(&m_bytecode[m_ip]);
        m_ip += sizeof(ui16);
        return ui16;
    }
    std::uint32_t decode_ui32() {
        const auto ui32 = *reinterpret_cast<std::uint32_t*>(&m_bytecode[m_ip]);
        m_ip += sizeof(ui32);
        return ui32;
    }
    float decode_fp32() {
        const auto fp32 = *reinterpret_cast<float*>(&m_bytecode[m_ip]);
        m_ip += sizeof(fp32);
        return fp32;
    }
    double decode_fp64() {
        const auto fp64 = *reinterpret_cast<double*>(&m_bytecode[m_ip]);
        m_ip += sizeof(fp64);
        return fp64;
    }
    std::string decode_cstr() {
        std::string cstr{ reinterpret_cast<char*>(&m_bytecode[m_ip]) };
        m_ip += cstr.size() + 1;
        return cstr;
    }
private:
    MhdLangVal pop()
    {
        const MhdLangVal popped{ m_val_stack.back() };    
        m_val_stack.pop_back();
        return popped;
    }
    void push(const MhdLangVal& val)
    {
        m_val_stack.push_back(val);
    }
private:
    MhdLangRef pop_ref()
    {
        const MhdLangRef popped{ m_ref_stack.back() };    
        m_ref_stack.pop_back();
        return popped;
    }
    void push_ref(const MhdLangRef& ref)
    {
        m_ref_stack.push_back(ref);
    }
};    // struct MhdLangVM
//########################################################################################################
//########################################################################################################
//########################################################################################################
void MhdLangVM::interpret()
{
#if 1
    #define SWITCH_OP() \
        while (m_ip < m_bytecode.size()) { \
            const MhdLangOpcode op{ decode_op() }; \
            switch (op) {
    #define END_SWITCH_OP() }}
    #define CASE(c) \
        case MhdLangOpcode::c: /*puts(#c);*/
    #define BREAK() break;
#else
    static void* case_table[] = {
    #define OP(opcode) &&LABEL_##opcode,
        MHD_LANG_OPCODES
    #undef OP
    };
    #define SWITCH_OP() \
        while (m_ip < m_bytecode.size()) { \
            const op{ decode_ui8() }; \
            goto *case_table[op];
    #define END_SWITCH_OP() }
    #define CASE(s) LABEL_##opcode:
    #define BREAK() continue;
#endif

    SWITCH_OP()
    /* Do nothing. */
    CASE(NOOP)
        BREAK()
    /* Discard values on stack. */
    CASE(DISCARD_1)
        pop();
        BREAK()
    CASE(DISCARD_2)
        pop();
        pop();
        BREAK()
    CASE(DISCARD_3)
        pop();
        pop();
        pop();
        BREAK()
    CASE(DISCARD_4)
        pop();
        pop();
        pop();
        pop();
        BREAK()
    /* Duplicate values on top of the stack. */ 
        CASE(DUP_2X1) {
            auto b{ pop() };
            auto a{ pop() };
            push(a);
            push(b);
            push(a);
            push(b);
            BREAK()
        }
    /* Load constants onto stack. */
    CASE(LOAD_TRUE)
        push(MhdLangVal(true));
        BREAK()
    CASE(LOAD_FALSE)
        push(MhdLangVal(false));
        BREAK()
    CASE(LOAD_NULLPTR)
        push(MhdLangVal(nullptr));
        BREAK()
    CASE(LOAD_UI_0)
        push(MhdLangVal(0));
        BREAK()
    CASE(LOAD_UI_1)
        push(MhdLangVal(1));
        BREAK()
    CASE(LOAD_UI_2)
        push(MhdLangVal(2));
        BREAK()
    CASE(LOAD_UI_3)
        push(MhdLangVal(3));
        BREAK()
    CASE(LOAD_UI8)
        push(MhdLangVal(decode_ui8()));
        BREAK()
    CASE(LOAD_UI16)
        push(MhdLangVal(decode_ui16()));
        BREAK()
    CASE(LOAD_UI32)
        push(MhdLangVal(decode_ui32()));
        BREAK()
    CASE(LOAD_FP32)
        push(MhdLangVal(decode_fp32()));
        BREAK()
    CASE(LOAD_FP64)
        push(MhdLangVal(decode_fp64()));
        BREAK()
    CASE(LOAD_CSTR)
        push(MhdLangVal(decode_cstr()));
        BREAK()
    CASE(LOAD_FUNC) {
        std::uint32_t new_ip1{ decode_ui32() };
        push(MhdLangVal([&, new_ip1](const std::vector<MhdScriptVal>& args) -> MhdScriptVal {
            for (auto& arg : args) {
                push(arg);
            }
            auto new_ip{new_ip1};
            //printf("%d %d\n", new_ip, m_ip);
            std::swap(m_ip, new_ip);
            interpret();
            std::swap(m_ip, new_ip);
            return pop();
        }));
        BREAK()
    }
    CASE(LOAD_VAR_CSTR)
        push(MhdScriptVarScope::var(decode_cstr()));
        BREAK()
    CASE(STORE_VAR_CSTR)
        push(MhdScriptVarScope::var(decode_cstr()) = pop());
        BREAK()
    CASE(LOAD_REF)
        push(MhdLangVal(pop_ref()));
        BREAK()
    /* Perform relative jumps. */
    CASE(RET)
        return;
    CASE(JUMP)
        m_ip = decode_ui32();
        BREAK()
    CASE(JUMP_Z)
        if (pop()) {
            decode_ui32();
        } else {
            m_ip = decode_ui32();
        }
        BREAK()
    CASE(JUMP_NZ)
        if (pop()) {
            m_ip = decode_ui32();
        } else {
            decode_ui32();
        }
        BREAK()
    /* Calls and Indexing. */
    CASE(INDEX_1) {
        auto arg = pop();
        auto obj = pop();
        push(obj[arg]);
        BREAK()
    }
    CASE(STORE_INDEX_1) {
        auto val = pop();
        auto arg = pop();
        auto obj = pop();
        push(obj[arg] = val);
        BREAK()
    }
    /* Calls and Indexing. */
    CASE(CALL_0)
        push(pop()({}));
        BREAK()
    CASE(CALL_1) {
        std::vector<MhdLangVal> args{ pop() };
        push(pop()(args));
        BREAK()
    }
    CASE(CALL_2) {
        std::vector<MhdLangVal> args{ pop(), pop() };
        std::swap(args[0], args[1]);
        push(pop()(args));
        BREAK()
    }
    CASE(CALL_3) {
        std::vector<MhdLangVal> args{ pop(), pop(), pop() };
        std::swap(args[0], args[2]);
        push(pop()(args));
        BREAK()
    }
    CASE(CALL_N) {
        std::uint16_t nargs{ decode_ui16() };
        std::vector<MhdLangVal> args;
        while (nargs-- > 0) {
            args.push_back(pop());
        }
        std::reverse(args.begin(), args.end());
        push(pop()(args));
        BREAK()
    }
    /* Reference variables to separate stack. */
    CASE(REF_CSTR)
        push_ref(MhdScriptRef(&MhdScriptVarScope::var(decode_cstr())));
        BREAK()
    CASE(REF_DISCARD)
        pop_ref();
        BREAK()
    /* Operators. */
    CASE(OP_NOT)
        push(!pop());
        BREAK()
    CASE(OP_PLUS)
        push(+pop());
        BREAK()
    CASE(OP_MINUS)
        push(-pop());
        BREAK()
    CASE(OP_EQ) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs == rhs);
        BREAK()
    }
    CASE(OP_NEQ) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs != rhs);
        BREAK()
    }
    CASE(OP_LT) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs < rhs);
        BREAK()
    }
    CASE(OP_GT) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs > rhs);
        BREAK()
    }
    CASE(OP_LTE) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs <= rhs);
        BREAK()
    }
    CASE(OP_GTE) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs >= rhs);
        BREAK()
    }
    CASE(OP_ADD) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs + rhs);
        BREAK()
    }
    CASE(OP_SUB) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs - rhs);
        BREAK()
    }
    CASE(OP_MUL) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs * rhs);
        BREAK()
    }
    CASE(OP_DIV) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs / rhs);
        BREAK()
    }
    CASE(OP_MOD) {
        const auto rhs{ pop() };
        const auto lhs{ pop() };
        push(lhs % rhs);
        BREAK()
    }
    /* Assignment operators. */
    CASE(OP_ADD_ASG) {
        const auto rhs{ pop() };
        auto lhs{ pop() };
        push(lhs += rhs);
        BREAK()
    }
    /* Assignment operators. */
    CASE(OPREF_ASG) {
        push(pop_ref() = pop());
        BREAK()
    }
    CASE(OPREF_ADD_ASG)
    CASE(OPREF_SUB_ASG)
    CASE(OPREF_MUL_ASG)
    CASE(OPREF_DIV_ASG)
    CASE(OPREF_MOD_ASG) {
        BREAK()
    }
    /* Debug opcodes. */
    CASE(DEBUG_PRINT)
        print({ m_val_stack.back() });
        BREAK()
    /* Invalid opcode. */
    CASE(EXTRA)
#if 1
    default:
#endif
        puts(MhdLangOpcodeNames[(int)op]);
        ORCHID_ASSERT(0);
        return;
    END_SWITCH_OP()
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
