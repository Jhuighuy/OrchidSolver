!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_script_value_c
Implicit None
Interface
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_script_val_dtor(val) Bind(C, name='mhd_script_val_dtor')
    !> {{{
    Use ISO_C_BINDING
    Type(C_PTR), Intent(In), Value :: val
    !> }}}
End Subroutine mhd_script_val_dtor
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_script_val_uadd_c(val, lhs) Bind(C, name='mhd_script_val_uadd')
    !> {{{
    Use ISO_C_BINDING
    Type(C_PTR), Intent(Out) :: val
    Type(C_PTR), Intent(In), Value :: lhs
    !> }}}
End Subroutine mhd_script_val_uadd_c
Subroutine mhd_script_val_usub_c(val, lhs) Bind(C, name='mhd_script_val_usub')
    !> {{{
    Use ISO_C_BINDING
    Type(C_PTR), Intent(Out) :: val
    Type(C_PTR), Intent(In), Value :: lhs
    !> }}}
End Subroutine mhd_script_val_usub_c
!--------------------------------------------------------------------------------------------------------
Subroutine mhd_script_val_add_c(val, lhs, rhs) Bind(C, name='mhd_script_val_add')
    !> {{{
    Use ISO_C_BINDING
    Type(C_PTR), Intent(Out) :: val
    Type(C_PTR), Intent(In), Value :: lhs, rhs
    !> }}}
End Subroutine mhd_script_val_add_c
Subroutine mhd_script_val_sub_c(val, lhs, rhs) Bind(C, name='mhd_script_val_sub')
    !> {{{
    Use ISO_C_BINDING
    Type(C_PTR), Intent(Out) :: val
    Type(C_PTR), Intent(In), Value :: lhs, rhs
    !> }}}
End Subroutine mhd_script_val_sub_c
Subroutine mhd_script_val_mul_c(val, lhs, rhs) Bind(C, name='mhd_script_val_mul')
    !> {{{
    Use ISO_C_BINDING
    Type(C_PTR), Intent(Out) :: val
    Type(C_PTR), Intent(In), Value :: lhs, rhs
    !> }}}
End Subroutine mhd_script_val_mul_c
Subroutine mhd_script_val_div_c(val, lhs, rhs) Bind(C, name='mhd_script_val_div')
    !> {{{
    Use ISO_C_BINDING
    Type(C_PTR), Intent(Out) :: val
    Type(C_PTR), Intent(In), Value :: lhs, rhs
    !> }}}
End Subroutine mhd_script_val_div_c
Subroutine mhd_script_val_mod_c(val, lhs, rhs) Bind(C, name='mhd_script_val_mod')
    !> {{{
    Use ISO_C_BINDING
    Type(C_PTR), Intent(Out) :: val
    Type(C_PTR), Intent(In), Value :: lhs, rhs
    !> }}}
End Subroutine mhd_script_val_mod_c
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Interface
End Module orchid_script_value_c



Module orchid_script_value
Use orchid_script_value_c
Use ISO_C_BINDING
Type :: MhdScriptVal
    Type(C_PTR) :: ptr
    Contains
End Type MhdScriptVal
Interface Operator (+)
    Procedure mhd_script_val_add
End Interface
Interface Operator (-)
    Procedure mhd_script_val_sub
End Interface
Interface Operator (*)
    Procedure mhd_script_val_mul
End Interface
Interface Operator (/)
    Procedure mhd_script_val_div
End Interface
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Function mhd_script_val_add(lhs, rhs) Result(val)
    !> {{{
    Class(MhdScriptVal), Allocatable :: val
    Class(MhdScriptVal), Allocatable, Intent(In) :: lhs, rhs
    !> }}}
    Allocate(val)
    Call mhd_script_val_add_c(val%ptr, lhs%ptr, rhs%ptr)
End Function mhd_script_val_add
Function mhd_script_val_sub(lhs, rhs) Result(val)
    !> {{{
    Class(MhdScriptVal), Allocatable :: val
    Class(MhdScriptVal), Allocatable, Intent(In) :: lhs, rhs
    !> }}}
    Allocate(val)
    Call mhd_script_val_sub_c(val%ptr, lhs%ptr, rhs%ptr)
End Function mhd_script_val_sub
Function mhd_script_val_mul(lhs, rhs) Result(val)
    !> {{{
    Class(MhdScriptVal), Allocatable :: val
    Class(MhdScriptVal), Allocatable, Intent(In) :: lhs, rhs
    !> }}}
    Allocate(val)
    Call mhd_script_val_mul_c(val%ptr, lhs%ptr, rhs%ptr)
End Function mhd_script_val_mul
Function mhd_script_val_div(lhs, rhs) Result(val)
    !> {{{
    Class(MhdScriptVal), Allocatable :: val
    Class(MhdScriptVal), Allocatable, Intent(In) :: lhs, rhs
    !> }}}
    Allocate(val)
    Call mhd_script_val_div_c(val%ptr, lhs%ptr, rhs%ptr)
End Function mhd_script_val_div
Function mhd_script_val_mod(lhs, rhs) Result(val)
    !> {{{
    Class(MhdScriptVal), Allocatable :: val
    Class(MhdScriptVal), Allocatable, Intent(In) :: lhs, rhs
    !> }}}
    Allocate(val)
    Call mhd_script_val_mod_c(val%ptr, lhs%ptr, rhs%ptr)
End Function mhd_script_val_mod
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_script_value

