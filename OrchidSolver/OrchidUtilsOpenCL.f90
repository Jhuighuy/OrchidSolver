!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.
    
Module orchid_solver_opencl
Use iso_c_binding
Implicit None
!########################################################################################################
!########################################################################################################
!########################################################################################################
Interface
Function clGetPlatformIDs(num_entries, platforms, num_platforms) &
    Bind(C, Name='clGetPlatformIDs')
    Use iso_c_binding
    !> {{{
    Integer(c_int32_t), Intent(In), Value :: num_entries
    Type(c_ptr), Intent(In), Value :: platforms
    Integer(c_int32_t), Intent(Out) :: num_platforms
    !> }}}
    Integer(C_Int32_t) :: clGetPlatformIDs
End Function clGetPlatformIDs
End Interface
!########################################################################################################
!########################################################################################################
!########################################################################################################
Integer(c_int32_t), Parameter :: CL_PLATFORM_PROFILE = Z'0900'
Integer(c_int32_t), Parameter :: CL_PLATFORM_VERSION = Z'0901'
Integer(c_int32_t), Parameter :: CL_PLATFORM_NAME = Z'0902'
Integer(c_int32_t), Parameter :: CL_PLATFORM_VENDOR = Z'0903'
Integer(c_int32_t), Parameter :: CL_PLATFORM_EXTENSIONS = Z'0904'
Interface
Function clGetPlatformInfo(platform, param_name, param_value_size, param_value, param_value_size_ret) &
    Bind(C, Name='clGetPlatformInfo')
    Use iso_c_binding
    !> {{{
    Integer(c_intptr_t), Intent(In), Value :: platform
    Integer(c_int32_t), Intent(In), Value :: param_name
    Integer(c_size_t), Intent(In), Value :: param_value_size
    Type(c_ptr), Intent(In), Value :: param_value
    Integer(c_size_t), Intent(Out) :: param_value_size_ret
    !> }}}
    Integer(c_int32_t) :: clGetPlatformInfo
End Function clGetPlatformInfo
End Interface
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_opencl
    