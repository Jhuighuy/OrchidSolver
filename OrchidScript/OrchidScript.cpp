// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptParser.hpp"
#include "OrchidScriptVar.hpp"
#include "OrchidScriptVal2.hpp"
#include "OrchidScriptForeign.hpp"

#include <fstream>
#include <cstdio>

MhdScriptVal make_map(const std::vector<MhdScriptVal>&)
{
    return MhdScriptVal(std::map<MhdScriptVal, MhdScriptVal>());
}
MhdScriptVal print(const std::vector<MhdScriptVal>& args)
{
    puts(args[0].operator std::string().c_str());
    return MhdScriptVal();
}
MhdScriptVal type(const std::vector<MhdScriptVal>& args)
{
    const MhdScriptVal& val = args[0];
    switch (val.m_type)
    {
        case MhdScriptVal::Type::LGC:
            return MhdScriptVal("bool");
        case MhdScriptVal::Type::INT:
            return MhdScriptVal("int");
        case MhdScriptVal::Type::DBL:
            return MhdScriptVal("double");
        case MhdScriptVal::Type::STR:
            return MhdScriptVal("string");
        case MhdScriptVal::Type::PTR:
            return MhdScriptVal("void*");
        case MhdScriptVal::Type::MAP:
            return MhdScriptVal("map");
        case MhdScriptVal::Type::FUN:
            return MhdScriptVal("function");
        default:
            ORCHID_ASSERT(0);
            break;
    }
}
MhdScriptVal assert_(const std::vector<MhdScriptVal>& args)
{
    const MhdScriptVal& val = args[0];
    ORCHID_ASSERT(val);
    if (!val) {
        puts("Assert failed!");
        exit(-1);
    }
    return val;
}
extern "C"
void orchid_solver_scanner_test() {}

int main(int argc, char** argv) 
{
    //auto ss = GetProcAddress(LoadLibraryA("msvcrt.dll"), "sin");
#if _MSC_VER
    std::ifstream file(/*argv[1]*/"../OrchidScript/test/test__basic.mhd");
#else
    std::ifstream file(argv[1]);
#endif
    std::string file_text;
    file.seekg(0, std::ios::end);
    file_text.resize(file.tellg());
    file.seekg(0, std::ios::beg);
    file.read(&file_text[0], file_text.size());
    file.close();
    MhdScriptVarScope::var("make_map") = MhdScriptVal(make_map);
    MhdScriptVarScope::var("print") = MhdScriptVal(print);
    MhdScriptVarScope::var("typeof") = MhdScriptVal(type);
    MhdScriptVarScope::var("assert") = MhdScriptVal(assert_);
    MhdScriptVarScope::var("foreign") = MhdScriptVal(mhd_foreign);
    MhdScriptParser parser(file_text.c_str());
    auto expr = parser.parse_program_wrap();
    if (expr != nullptr) {
        print({ expr->eval() });
    }
    return 0;
}
