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

void ffftest1(int* a) 
{
    printf("%lld\n", *a);
}

extern "C" void ffftest();

#include <ffi/ffi.h>
#include <dlfcn.h>
int main() 
{
    auto self = dlopen(nullptr, RTLD_LAZY);
    auto self_sin = (double(*)(double))dlsym(self, "sin");
    printf("%lf\n", self_sin(3));

    ffi_type* arg_types[] = {&ffi_type_pointer};
    ffi_cif cif;
    ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1, &ffi_type_void, arg_types);
    int a = 1488;
    void* pa = &a;
    void* ppa = &pa;
    int b;
    //printf("%lld\n", ffftest);
    ffi_arg rc;
    ffi_call(&cif, FFI_FN(ffftest), &rc, &ppa);
    
    orchid_solver_scanner_test();
    return 0;
}
