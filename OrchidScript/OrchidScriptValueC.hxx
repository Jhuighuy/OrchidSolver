// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include <stdbool.h>
#ifndef MHD_INTERFACE
#define MHD_INTERFACE
#endif

typedef struct MhdScriptVal_t {
}* MhdScriptVal_p;

#ifdef __cplusplus
extern "C" {
#endif
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
extern void
mhd_script_val_dtor(MhdScriptVal_p);
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
extern void
mhd_script_val_ctor_lgc(MhdScriptVal_p*, const bool*, int);
MHD_INTERFACE
extern void
mhd_script_val_ctor_int(MhdScriptVal_p*, const int*, int);
MHD_INTERFACE
extern void
mhd_script_val_ctor_dbl(MhdScriptVal_p*, const double*, int);
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
extern void
mhd_script_val_ctor_str(MhdScriptVal_p*, const char*, int);
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
extern void
mhd_script_val_ctor_lst(MhdScriptVal_p*);
MHD_INTERFACE
extern void
mhd_script_val_ctor_map(MhdScriptVal_p*);
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
extern void
mhd_script_val_ctor_fun(MhdScriptVal_p*, const char*, const void*);
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
extern int
mhd_script_val_asg(MhdScriptVal_p, MhdScriptVal_p);
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
extern int
mhd_script_val_uadd(MhdScriptVal_p*, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_usub(MhdScriptVal_p*, MhdScriptVal_p);
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
extern int
mhd_script_val_add(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_sub(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_mul(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_div(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_mod(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
extern int
mhd_script_val_unot(MhdScriptVal_p);
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
extern int
mhd_script_val_eq(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_neq(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_lt(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_lte(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_gt(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_gte(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_and(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_or(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
extern int
mhd_script_val_unot_bw(MhdScriptVal_p*, MhdScriptVal_p);
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
extern int
mhd_script_val_and_bw(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_or_bw(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_xor_bw(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_lshift(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
MHD_INTERFACE
extern int
mhd_script_val_rshift(MhdScriptVal_p*, MhdScriptVal_p, MhdScriptVal_p);
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
extern int
mhd_script_val_index(MhdScriptVal_p*, MhdScriptVal_p*, int);
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
extern int
mhd_script_val_call(MhdScriptVal_p*, MhdScriptVal_p*, int);
//########################################################################################################
//########################################################################################################
//########################################################################################################

//########################################################################################################
//########################################################################################################
//########################################################################################################
#ifdef __cplusplus
}
#endif
