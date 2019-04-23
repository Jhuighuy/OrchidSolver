// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptParser.hpp"

#include <fstream>
#include <cstdio>

MhdScriptVal make_map(const std::vector<MhdScriptVal>& args)
{
    return MhdScriptVal(std::map<MhdScriptVal, MhdScriptVal>());
}

extern "C" void orchid_solver_scanner_test()
{
    MhdScriptVarScope::var("make_map") = MhdScriptVal(
        std::function<MhdScriptVal(const std::vector<MhdScriptVal>&)>(make_map));
    MhdScriptParser p(R"({
namespace a {
    b = 1;
}
a.d = 3;
namespace a {
    c = 2;
}
a;
//a.b + a.c;
    })");

    auto e = p.parse_wrap();
    auto g = e.get();
    auto a = g->eval();
    printf("%s\n", a.operator std::string().c_str());
}

int main() 
{
    orchid_solver_scanner_test();
    return 0;
}
