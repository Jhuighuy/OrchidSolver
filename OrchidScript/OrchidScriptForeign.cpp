// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#include "OrchidScriptForeign.hpp"

#include <cctype>
#if _WIN32
#include <Windows.h>
#endif

//########################################################################################################
//########################################################################################################
//########################################################################################################
MHD_INTERNAL
void
mhd_foreign_parse_type(const char*& string,
                       std::pair<MhdScriptVal::Type, bool>& type)
{
    /// Parse a type.
    while (std::isspace(*string)) {
        ++string;
    }
    if (std::strncmp(string, "bool", 4) == 0 &&
       !std::isalnum(string[4])) {
        type.first = MhdScriptVal::Type::LGC;
        string += 4;
    } else if (std::strncmp(string, "int", 3) == 0 &&
              !std::isalnum(string[3])) {
        type.first = MhdScriptVal::Type::INT;
        string += 3;
    } else if (std::strncmp(string, "double", 6) == 0 &&
              !std::isalnum(string[6])) {
        type.first = MhdScriptVal::Type::DBL;
        string += 6;
    } else if (std::strncmp(string, "char", 4 &&
              !std::isalnum(string[4])) == 0) {
        type.first = MhdScriptVal::Type::STR;
        string += 4;
    } else if (std::strncmp(string, "void", 4 &&
              !std::isalnum(string[4])) == 0) {
        type.first = MhdScriptVal::Type::PTR;
        string += 4;
    } else {
        throw MhdForeignError("Expected 'bool', 'int', 'double', 'char*' or 'void*'.");
    }
    while (std::isspace(*string)) {
        ++string;
    }
    if (type.second = *string == '*',
        type.second) {
        ++string;
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERNAL
void
mhd_foreign_parse_signature(const char* string,
                            std::string& name,
                            std::pair<MhdScriptVal::Type, bool>& type_ret,
                            std::vector<std::pair<MhdScriptVal::Type, bool>>& type_args)
{
    /// Parse signature of a function.
    mhd_foreign_parse_type(string, type_ret);
    while (std::isspace(*string)) {
        ++string;
    }
    /* Parse function name. */
    if (std::isalpha(*string) || *string == '_' || *string == '$') {
        name.push_back(*string);
        ++string;
    } else {
        throw MhdForeignError("Expected 'a-z', 'A-Z', '_' or '$'.");
    }
    while (std::isalnum(*string) || *string == '_' || *string == '$') {
        name.push_back(*string);
        ++string;
    }
    while (std::isspace(*string)) {
        ++string;
    }
    /* Parse function return type. */
    if (*string == '(') {
        ++string;
    } else {
        throw MhdForeignError("Expected '('.");
    }
    while (*string != ')') {
        type_args.emplace_back();
        mhd_foreign_parse_type(string, type_args.back());
        while (std::isspace(*string)) {
            ++string;
        }
        if (*string == ',') {
            ++string;
        } else if (*string != ')') {
            throw MhdForeignError("Expected ',' or ')'.");
        }
    }
}
//--------------------------------------------------------------------------------------------------------
MHD_INTERFACE
MhdScriptVal 
mhd_foreign(const std::vector<MhdScriptVal>& args)
{
    std::string fun(args[0]);
    std::string lib(args[1]);
    std::string name;
    std::pair<MhdScriptVal::Type, bool> type_ret;
    std::vector<std::pair<MhdScriptVal::Type, bool>> type_args;
    mhd_foreign_parse_signature(fun.c_str(), name, type_ret, type_args);
    return MhdScriptVal();
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
