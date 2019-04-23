// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptValueC.hxx"
#include "OrchidScriptValue.hpp"

//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
MhdScriptVal_p to_c(const MhdScriptVal& val)
{
	return reinterpret_cast<MhdScriptVal_p>(new MhdScriptVal(val));
}
MHD_INTERNAL
MhdScriptVal& to_cpp(MhdScriptVal_p ptr)
{
	return *reinterpret_cast<MhdScriptVal*>(ptr);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
int
mhd_script_val_uadd(MhdScriptVal_p* val, MhdScriptVal_p lhs)
{
	try {
		*val = to_c(+to_cpp(lhs));
		return 0;
	} catch (const MhdScriptInvalidOp& exc) {
		return 1;
	}
}
MHD_INTERFACE
int
mhd_script_val_usub(MhdScriptVal_p* val, MhdScriptVal_p lhs)
{
	try {
		*val = to_c(-to_cpp(lhs));
		return 0;
	} catch (const MhdScriptInvalidOp& exc) {
		return 1;
	}
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
int
mhd_script_val_add(MhdScriptVal_p* val, MhdScriptVal_p lhs, MhdScriptVal_p rhs)
{
	try {
		*val = to_c(to_cpp(lhs) + to_cpp(rhs));
		return 0;
	} catch (const MhdScriptInvalidOp& exc) {
		return 1;
	}
}
MHD_INTERFACE
int
mhd_script_val_sub(MhdScriptVal_p* val, MhdScriptVal_p lhs, MhdScriptVal_p rhs)
{
	try {
		*val = to_c(to_cpp(lhs) - to_cpp(rhs));
		return 0;
	} catch (const MhdScriptInvalidOp& exc) {
		return 1;
	}
}
MHD_INTERFACE
int
mhd_script_val_mul(MhdScriptVal_p* val, MhdScriptVal_p lhs, MhdScriptVal_p rhs)
{
	try {
		*val = to_c(to_cpp(lhs) * to_cpp(rhs));
		return 0;
	} catch (const MhdScriptInvalidOp& exc) {
		return 1;
	}
}
MHD_INTERFACE
int
mhd_script_val_div(MhdScriptVal_p* val, MhdScriptVal_p lhs, MhdScriptVal_p rhs)
{
	try {
		*val = to_c(to_cpp(lhs) / to_cpp(rhs));
		return 0;
	} catch (const MhdScriptInvalidOp& exc) {
		return 1;
	}
}
MHD_INTERFACE
int
mhd_script_val_mod(MhdScriptVal_p* val, MhdScriptVal_p lhs, MhdScriptVal_p rhs)
{
	try {
		*val = to_c(to_cpp(lhs) % to_cpp(rhs));
		return 0;
	} catch (const MhdScriptInvalidOp& exc) {
		return 1;
	}
}
//########################################################################################################
//########################################################################################################
//########################################################################################################

