// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptValue.hpp"

struct MhdForeignError
{
    MhdForeignError(...) {}
};
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE extern
MhdScriptVal mhd_foreign(const std::vector<MhdScriptVal>&);
//########################################################################################################
//########################################################################################################
//########################################################################################################