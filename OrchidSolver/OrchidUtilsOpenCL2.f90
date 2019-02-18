!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.
    
Module orchid_solver_opencl2
Use orchid_solver_opencl
Contains
Subroutine test_opencl()
    Use iso_c_binding
    Integer(c_int32_t) :: err, num, i
    Integer(c_size_t) :: ll, zz=0
    Integer(c_intptr_t), Allocatable, Target :: platform_ids(:)
    Character, Allocatable, Target :: platform_name(:)
    err = clGetPlatformIDs(0, c_null_ptr, num)
    Allocate(platform_ids(num))
    err = clGetPlatformIDs(num, c_loc(platform_ids), num)
    Do i = 1, num
        err = clGetPlatformInfo(platform_ids(i), CL_PLATFORM_NAME, zz, c_null_ptr, ll)
        Allocate(platform_name(ll))
        err = clGetPlatformInfo(platform_ids(i), CL_PLATFORM_NAME, ll, c_loc(platform_name), ll)
        Write (*,*) 'OpenCL platform: ', platform_name
    End Do
End Subroutine test_opencl
End Module orchid_solver_opencl2

    
    