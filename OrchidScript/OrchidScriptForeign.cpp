// Orchid -- 2D / 3D Euler / MagnetoHydroDynamics solver.
// Copyright(C) Butakov Oleg 2019.

#define _CRT_SECURE_NO_WARNINGS 1
#include "OrchidScriptForeign.hpp"

#include <cstring>
#include <cctype>
#if _WIN32
#include <Windows.h>
#define WITH_FFI 0
#endif
#undef WITH_FFI
#define WITH_FFI 0
#ifndef WITH_FFI
#define WITH_FFI 1
#endif
#if WITH_FFI
#include <ffi.h>
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
//########################################################################################################
//########################################################################################################
//########################################################################################################
int add(int a, int b)
{
    printf("%d %d\n", a, b);
    return a + b;
}
void* fopen1(char* s, char* m)
{
    puts(s);
    puts(m);
    return nullptr;
}
MHD_INTERFACE
MhdScriptVal 
mhd_foreign(const std::vector<MhdScriptVal>& args)
{
    std::string fun(args[0]);
    std::string lib(args[1]);
    /* Parse arguments. */
    std::string name;
    std::pair<MhdScriptVal::Type, bool> type_ret;
    std::vector<std::pair<MhdScriptVal::Type, bool>> type_args;
    mhd_foreign_parse_signature(fun.c_str(), name, type_ret, type_args);
    /* Prepare arguments and return value. */
    MhdScriptVal val_ret(type_ret.first);
    std::vector<MhdScriptVal> val_args{ args.cbegin() + 2, args.cend() };
    for (std::size_t i = 0; i < val_args.size(); ++i) {
        val_args[i] = MhdScriptVal::operator_cast(type_args[i].first, val_args[i]);
    }
#if WITH_FFI
    void* fun_ptr;
    if (name == "fopen") {
        fun_ptr = (void*)fopen1;
    } else if (name == "fputs") {
        fun_ptr = (void*)fputs;
    }
    //fun_ptr = (void*)puts;
    void* val_ret_ptr;
    ffi_type* val_ret_type;
    /* Prepare FFI return pointer. */
    val_ret_ptr = val_ret.data();
    /* Prepare FFI return type. */
    if (type_ret.second) {
        val_ret_type = &ffi_type_pointer;
    } else switch (type_ret.first) {
        case MhdScriptVal::Type::LGC:
            val_ret_type = &ffi_type_sint8;
            break;
        case MhdScriptVal::Type::INT:
            val_ret_type = &ffi_type_sint;
            break;
        case MhdScriptVal::Type::DBL:
            val_ret_type = &ffi_type_double;
            break;
        case MhdScriptVal::Type::PTR:
            val_ret_type = &ffi_type_void;
            break;
        default:
            throw MhdForeignError();
    }
    std::vector<void*> val_ptrs;
    std::vector<void*> val_pptrs;
    std::vector<ffi_type*> val_types;
    for (std::size_t i = 0; i < val_args.size(); ++i) {
        /* Prepare FFI argument pointers. */
        if (type_args[i].second) {
            val_pptrs.emplace_back(val_args[i].data());
            val_ptrs.emplace_back(&val_pptrs.back());
        } else {
            val_ptrs.emplace_back(val_args[i].data());
        }
        /* Prepare FFI argument types. */
        if (type_args[i].second) {
            val_types.push_back(&ffi_type_pointer);
        } else switch (type_args[i].first) {
            case MhdScriptVal::Type::LGC:
                val_types.push_back(&ffi_type_sint8);
                break;
            case MhdScriptVal::Type::INT:
                val_types.push_back(&ffi_type_sint);
                break;
            case MhdScriptVal::Type::DBL:
                val_types.push_back(&ffi_type_double);
                break;
            default:
                throw MhdForeignError();
        }
    }
    ffi_cif cif;
    ffi_status cif_status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, val_types.size(), val_ret_type,
                                                                val_types.data());
    if (cif_status != FFI_OK) {
        throw MhdForeignError();
    }
    ffi_call(&cif, FFI_FN(fun_ptr), val_ret_ptr, val_ptrs.data());
#endif
    return val_ret;
}
//########################################################################################################
//########################################################################################################
//########################################################################################################
