// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#pragma once

#include "OrchidScriptValue.hpp"

#include <string>
#include <unordered_map>
#include <map>

//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScriptVarScope final
{
private:
	std::size_t m_stack_prev_size;
	std::unordered_map<std::string, MhdScriptVal> m_stack_vars;
public:
	MHD_INTERFACE
	MhdScriptVarScope(bool from_global = false);
	MHD_INTERFACE
	~MhdScriptVarScope();
public:
	MHD_INTERFACE
	static MhdScriptVal& let(const std::string& var_name);
	MHD_INTERFACE
	static MhdScriptVal& var(const std::string& var_name);
public:
	MHD_INTERFACE
	static void load(const std::map<MhdScriptVal, MhdScriptVal>& vars);
	MHD_INTERFACE
	static MhdScriptVal dump();
};	// struct MhdScriptVarScope
//########################################################################################################
//########################################################################################################
//########################################################################################################
struct MhdScript final
{
public:
    static MhdScriptVal& evalr(const std::string& var_name)
    {
        return MhdScriptVarScope::var(var_name);
    }
};  // struct MhdScript
//########################################################################################################
//########################################################################################################
//########################################################################################################
