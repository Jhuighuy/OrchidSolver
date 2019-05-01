// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptStd.hpp"
#include "OrchidScriptVar.hpp"
#include "OrchidScriptValue.hpp"

#include <iostream>

//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptInitMe
{
public:
    template<typename T>
    MhdScriptInitMe(const T& t)
    {
        t();
    }
};  // struct MhdScriptInitMe
#define MHD_INIT_ME static MhdScriptInitMe __INIT_ME__##__LINE__ = []
#define MHD_DEF(name) \
    MhdScriptVal name(const std::vector<MhdScriptVal>& args); \
    MHD_INIT_ME() { MhdScript::evalr(#name) = name; }; \
    MhdScriptVal name(const std::vector<MhdScriptVal>& args) \
//########################################################################################################
//########################################################################################################
//########################################################################################################
//MHD_DEF(print)
//{
//    for (const MhdScriptVal& arg : args) {
//        std::cout << static_cast<std::string>(arg);
//    }
//    std::cout << std::endl;
//    return MhdScriptVal{};
//}
//########################################################################################################
//########################################################################################################
//########################################################################################################
