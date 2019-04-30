// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptVar.hpp"

//########################################################################################################
//########################################################################################################
//########################################################################################################
static std::vector<MhdScriptVarScope*> g_stack{ nullptr };
static MhdScriptVarScope g_scope{};
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVarScope::MhdScriptVarScope(bool from_global)
    : m_stack_prev_size(g_stack.size())
{
    if (from_global) {
        g_stack.push_back(g_stack[0]);
        g_stack.push_back(g_stack[1]);
    }
    g_stack.push_back(this);
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVarScope::~MhdScriptVarScope()
{
    g_stack.resize(m_stack_prev_size);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
MhdScriptVal& 
MhdScriptVarScope::let(const std::string& var_name)
{
    /// Redefine a variable.
    ORCHID_ASSERT(!var_name.empty());
    MhdScriptVarScope& stack_scope_top = *g_stack.back();
    return stack_scope_top.m_stack_vars[var_name];
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal& 
MhdScriptVarScope::var(const std::string& var_name)
{
    /// Find or Redefine a variable.
    ORCHID_ASSERT(!var_name.empty());
    for (auto stack_iter  = g_stack.rbegin();
              stack_iter != g_stack.rend() && *stack_iter != nullptr; 
            ++stack_iter) {
        MhdScriptVarScope& stack_scope = **stack_iter;
        const auto stack_scope_iter = stack_scope.m_stack_vars.find(var_name);
        if (stack_scope_iter != stack_scope.m_stack_vars.end()) {
            return stack_scope_iter->second;
        }
    }
    return let(var_name);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERFACE
void 
MhdScriptVarScope::load(const std::map<MhdScriptVal, MhdScriptVal>& vars)
{
    /// Load a map into the current scope.
    for (const auto& var_decl : vars) {
        let(static_cast<std::string>(var_decl.first)) = var_decl.second;
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal
MhdScriptVarScope::dump()
{
    /// Dump current scope into a map.
    MhdScriptVarScope& stack_scope_top = *g_stack.back();
    std::map<MhdScriptVal, MhdScriptVal> vars;
    for (const auto& var_decl : stack_scope_top.m_stack_vars) {
        vars[MhdScriptVal(var_decl.first)] = var_decl.second;
    }
    return MhdScriptVal(vars);
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
