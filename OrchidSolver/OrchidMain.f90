!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.
Module orchid_solver_params
    Implicit None
    
    Integer, Parameter :: dim = 2
    Logical, Parameter :: debug = .TRUE.
    Logical, Parameter :: verbose = .TRUE.
    Logical, Parameter :: mhd = .TRUE.

    Real(8), Parameter :: Mu_hydro = 0*0.000005D0
    Real(8), Parameter :: Mu_magneto = 0.0D0
    Real(8), Parameter :: Kappa = 1.0D7*0
    
    Real(8), Parameter :: Pi = 4.0D0*Atan(1.0D0)
    Real(8), Parameter :: S4Pi = 4.0D0*Sqrt(Atan(1.0D0))
    
    Real(8), Parameter :: Rgas = 8.314459848D7
    Real(8), Parameter :: Gamma = 0*1.4D0 + 1*5.0D0/3.0D0, Gamma1 = Gamma-1.0D0
    Integer, Parameter :: N_funcs = 2
    Integer, Parameter :: m_min = 0, m_max = N_funcs - 1
    Integer, Parameter :: n_min = 1, n_max = 8
    
    Real(8), Parameter :: Gamma_1 = ( Gamma - 1.0D0 )/( 2.0D0*Gamma )
    Real(8), Parameter :: Gamma_2 = ( Gamma + 1.0D0 )/( 2.0D0*Gamma )
    Real(8), Parameter :: Gamma_7 = ( Gamma - 1.0D0 )/2.0D0
    
    Real(8), Parameter :: NaN = Transfer([ Z'00000000', Z'7FF80000' ], 1.0D0)
    
    Contains
Elemental &
Function minmod1(a) Result(m)
    Real(8), Intent(In) :: a
    Real(8) :: m
    m = Max(0.0D0, Min(1.0D0, a))
End Function minmod1
Elemental &
Function minmod2(a, b) Result(m)
    Real(8), Intent(In) :: a, b
    Real(8) :: m
    m = ( Sign(1.0D0, a) + Sign(1.0D0, b) )/2.0D0*&
        Min(Abs(a), Abs(b))
End Function minmod2
Function minmod3(a, b, c) Result(m)
    Real(8), Intent(In) :: a, b, c
    Real(8) :: m
    m = Dble(Int( ( Sign(1.0D0, a) + Sign(1.0D0, b) + Sign(1.0D0, c) )/3.0D0))*&
        Min(Abs(a), Abs(b), Abs(c))
End Function minmod3
End Module orchid_solver_params
    
Module orchid_solver_helpers
    Contains
    Function to_str(k)
        ! {{{
        Integer, Intent(In) :: k
        ! ---
        Character(Len=20) :: to_str
        ! }}}
        Write (to_str, *) k
        to_str = adjustl(to_str)
    End Function to_str    
End Module orchid_solver_helpers
    
Module orchid_solver_simulation
Use orchid_solver_grid_gauss_legendre
!Use orchid_solver_opencl
Implicit None

Contains

Subroutine print_grid3(ga, g, l)
    !> {{{
    Integer, Intent(In) :: l
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: g
    !> }}}
    Integer :: i, j, k
    Integer :: output
    Real(8) :: r, theta, phi
    Real(8) :: x, y, z
    Real(8) :: v_r, v_t, v_p
    Real(8) :: v_x, v_y, v_z, v_l
    Real(8) :: vxy(2), vrp(2), a(2,2), vvv
    Open(NewUnit=output, file='../Results/fields-'//Trim(to_str(l))//'.csv', Status='replace')
    Write (output, *) 'x,y,z,r,e,u,v,w,bx,by,bz'
    Do i = ga%ncells_min, ga%ncells_max
       Write(output, '(E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6)') &
           ga%cells(i)%x, ',', ga%cells(i)%y, ',', ga%cells(i)%z, ',', &
           g(1, i), ',', &
           g(2, i)/g(1, i), ',', &
           g(3, i)/g(1, i), ',', g(4, i)/g(1, i), ',', g(5, i)/g(1, i), ',', &
           g(6, i)/Sqrt(4.0D0*Pi), ',', g(7, i)/Sqrt(4.0D0*Pi), ',', g(8, i)/Sqrt(4.0D0*Pi)
    End Do
    Close(output)
End Subroutine print_grid3

Subroutine print_grid4(ga, g, l)
    !> {{{
    Integer, Intent(In) :: l
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: g
    !> }}}
    Integer :: i, j, k
    Integer :: output
    Real(8) :: r, theta, phi
    Real(8) :: x, y, z
    Real(8) :: v_r, v_t, v_p
    Real(8) :: v_x, v_y, v_z, v_l
    Real(8) :: vxy(2), vrp(2), a(2,2), vvv
    Open(NewUnit=output, file='../Results/fields-'//Trim(to_str(l))//'.csv', Status='replace')
    !Write (output, *) 'x,y,z,r,e,u,v,w,bx,by,bz'
    Do i = ga%ncells_min, ga%ncells_max
       Write(output, '(E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6)') &
           ga%cells(i)%x, ',', ga%cells(i)%y, ',', ga%cells(i)%z, ',', &
           g(0, 1, i), ',', &
           g(0, 2, i)/g(0, 1, i), ',', &
           g(0, 3, i)/g(0, 1, i), ',', g(0, 4, i)/g(0, 1, i), ',', g(0, 5, i)/g(0, 1, i), ',', &
           g(0, 6, i)/Sqrt(4.0D0*Pi), ',', g(0, 7, i)/Sqrt(4.0D0*Pi), ',', g(0, 8, i)/Sqrt(4.0D0*Pi)
    End Do
    Close(output)
End Subroutine print_grid4
End Module orchid_solver_simulation

!> Sod test case (Pure FV).
Subroutine test_sod_1D()
    Use orchid_solver_simulation
    Use orchid_solver_grid
    Use orchid_solver_hydro_fv
    Use omp_lib
    Implicit None
    Integer :: l
    Real(8) :: Tstart, Tend
    Class(MhdGrid), Allocatable :: ga
    Class(MhdHydroSolver), Allocatable :: solver
    Real(8), Dimension(:,:), Allocatable :: g, gp

    Allocate(ga)
    Call ga%init1D(10.0D0, 200, -1, -1)
    Allocate(g(n_min:n_max, ga%ncells_min:ga%ncells_max))
    Allocate(gp(n_min:n_max, ga%ncells_min:ga%ncells_max))
    
    If (MHD) Then
        !> MHD Sod test case.
        g(:, :) = 0.0D0
        g(1, :) = 1.0D0
        g(2, :) = 1.0D0/( Gamma1*1.0D0 )
        g(6, :) = 4.0D0
        g(7, :) = 4.0D0
        g(8, :) = 2.0D0
        g(1, 1:100) = 1.08D0;
        g(2, 1:100) = 1.08D0*( 0.95D0/( Gamma1*1.08D0 ) + 0.5D0*(1.2D0**2 + 0.01D0**2 + 0.5D0**2) )
        g(3, 1:100) = 1.08D0*1.2D0;
        g(4, 1:100) = 1.08D0*0.01D0;
        g(5, 1:100) = 1.08D0*0.5D0;
        g(7, 1:100) = 3.6D0
    Else
        g(:, :) = 0.0D0
        g(1, :) = 1.0D0
        g(2, :) = 1.0D0/( Gamma1*1.0D0 )
        g(1, 1:100) = 2.0D0
        g(2, 1:100) = 10.0D0/( Gamma1*2.0D0 )
    End If

    Allocate(MhdHydroSolver :: solver)
    Call solver%init()
    Call print_grid3(ga, g, 0)
    Tstart = omp_get_wtime()
    Do l=1, 3000
        Call solver%calc_step(0.001D0, ga, g, gp)
        If (Mod(l, 100) == 0) Then
            Tend = omp_get_wtime()
            Write(*, *) 'time step:', l, TEnd - Tstart
            Tstart = Tend
            Call print_grid3(ga, g, l)
        End If
        g(:,:) = gp(:,:)
    End Do
End Subroutine test_sod_1D

!> Sod test case (DG).
Subroutine test_sod_1D_dg()
    Use orchid_solver_simulation
    Use orchid_solver_grid
    Use orchid_solver_hydro_dg
    Use omp_lib
    Implicit None
    Integer :: l
    Real(8) :: Tstart, Tend
    Class(MhdGridGaussLegendre), Allocatable :: ga
    Class(MhdHydroSolverDG), Allocatable :: solver
    Real(8), Dimension(:,:,:), Allocatable :: g, gp

    Allocate(ga)
    Call ga%init1D(10.0D0, 200, -1, -1)
    Call ga%init_gauss1D(2)
    Call ga%init_legendre1D()
    Allocate(g(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max))
    Allocate(gp(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max))
    
    !> Sod test case.
    If (MHD) Then
        !> MHD Sod test case.
        g(:, :, :) = 0.0D0
        g(0, 1, :) = 1.0D0
        g(0, 2, :) = 1.0D0/( Gamma1*1.0D0 ) + 0.125D0/Pi*(4.0D0**2 + 4.0D0**2 + 2.0D0**2)
        g(0, 6, :) = 4.0D0!/Sqrt(4.0D0*Pi)
        g(0, 7, :) = 4.0D0!/Sqrt(4.0D0*Pi)
        g(0, 8, :) = 2.0D0!/Sqrt(4.0D0*Pi)
        g(0, 1, 1:100) = 1.08D0;
        g(0, 2, 1:100) = 1.08D0*( 0.95D0/( Gamma1*1.08D0 ) + 0.5D0*(1.2D0**2 + 0.01D0**2 + 0.5D0**2)) +&
                                                        0.125D0/Pi*(4.0D0**2 + 3.6D0**2 + 2.0D0**2)
        g(0, 3, 1:100) = 1.08D0*1.2D0;
        g(0, 4, 1:100) = 1.08D0*0.01D0;
        g(0, 5, 1:100) = 1.08D0*0.5D0;
        g(0, 7, 1:100) = 3.6D0!/Sqrt(4.0D0*Pi)
    Else
        g(:, :, :) = 0.0D0
        g(0, 1, :) = 1.0D0
        g(0, 2, :) = 1.0D0/( Gamma1*1.0D0 )
        g(0, 1, 1:100) = 2.0D0
        g(0, 2, 1:100) = 10.0D0/( Gamma1*2.0D0 )
    End If
    
    Allocate(MhdHydroSolverDG :: solver)
    Call solver%init()
    Call print_grid4(ga, g, 0)
    Tstart = omp_get_wtime()
    Do l=1, 3000
        Call solver%calc_step_dg(0.001D0, ga, g, gp)
        If (Mod(l, 100) == 0) Then
            Tend = omp_get_wtime()
            Write(*, *) 'time step:', l, TEnd - Tstart
            Tstart = Tend
            Call print_grid4(ga, g, l)
        End If
        g(:,:,:) = gp(:,:,:)
    End Do
End Subroutine test_sod_1D_dg
    
Subroutine test_sod_2D
    Use orchid_solver_simulation
    Use orchid_solver_grid
    Use orchid_solver_hydro_fv
    Use omp_lib
    Implicit None
    Integer :: l
    Real(8) :: Tstart, Tend
    Class(MhdGrid), Allocatable :: ga
    Class(MhdHydroSolver), Allocatable :: solver
    Real(8), Dimension(:,:), Allocatable :: g, gp

    Allocate(ga)
    Call ga%init2D_polar(3.0D0, 10.0D0, 200, 200, -1, -2)
    Allocate(g(n_min:n_max, ga%ncells_min:ga%ncells_max))
    Allocate(gp(n_min:n_max, ga%ncells_min:ga%ncells_max))
    
    g(:, :) = 0.0D0
    g(1, :) = 1.0D0
    g(2, :) = 1.0D0/( Gamma1*1.0D0 )
    g(3, :) = 1.0D0
    If (MHD) Then
        !> MHD Sod test case.
        g(7, :) = 2.0D0!/Sqrt(4.0D0*Pi)
    End If

    Allocate(MhdHydroSolver :: solver)
    Call solver%init()
    Call print_grid3(ga, g, 0)
    Tstart = omp_get_wtime()
    Do l=1,1800
        Call solver%calc_step(0.001D0, ga, g, gp)
        If (Mod(l, 100) == 0) Then
            Tend = omp_get_wtime()
            Write(*, *) 'time step:', l, TEnd - Tstart
            Tstart = Tend
            Call print_grid3(ga, gp, l)
        End If
        g(:,:) = gp(:,:)
    End Do
End Subroutine test_sod_2D

Subroutine test_ot_2D
    Use orchid_solver_simulation
    Use orchid_solver_grid
    Use orchid_solver_hydro_fv
    Use orchid_solver_hydro_var
    Use omp_lib
    Implicit None
    Integer :: l, i
    Real(8) :: Tstart, Tend
    Class(MhdGrid), Allocatable :: ga
    Class(MhdHydroSolver), Allocatable :: solver
    Real(8), Dimension(:,:), Allocatable :: g, gp
    Type(MhdHydroVars3DMHD) :: vvv
    Allocate(ga)
    Call ga%init2D(1.0D0, 1.0D0, 256, 256, -3, -3, -3, -3)
    Allocate(g(n_min:n_max, ga%ncells_min:ga%ncells_max))
    Allocate(gp(n_min:n_max, ga%ncells_min:ga%ncells_max))
    
    g(:,:) = 0.0D0
    g(1,:) = 25.0D0/36.0D0/Pi
    g(2,:) = 5.0D0/12.0D0/Pi
    Do i = ga%ncells_min, ga%ncells_max
        g(3, i) = -Sin(2.0D0*Pi*ga%cells(i)%y)
        g(4, i) = +Sin(2.0D0*Pi*ga%cells(i)%x)
        g(6, i) = -Sin(2.0D0*Pi*ga%cells(i)%y)
        g(7, i) = +Sin(4.0D0*Pi*ga%cells(i)%x)
        vvv = mhd_hydro_vars_load3D_mhd(1.0D0, 0.0D0, 0.0D0, q_prim=g(:, i))
        g(:, i) = vvv%U(:)
    End Do

    Allocate(MhdHydroSolver :: solver)
    Call solver%init()
    Call print_grid3(ga, g, 0)
    Tstart = omp_get_wtime()
    Do l=1,50000000
        Call solver%calc_step(0.0001D0, ga, g, gp)
        If (Mod(l, 10) == 0) Then
            Tend = omp_get_wtime()
            Write(*, *) 'time step:', l, TEnd - Tstart
            Tstart = Tend
            Call print_grid3(ga, gp, l)
        End If
        g(:,:) = gp(:,:)
    End Do
End Subroutine test_ot_2D
    
Program orchid_solver
    Use orchid_solver_simulation
    Use orchid_solver_hydro_dg
    Use orchid_solver_pois
    Use orchid_solver_config
    use, intrinsic :: iso_fortran_env
    use omp_lib
#ifdef ORCHID_MPI    
    use MPI
#endif    
    Implicit None
    
    Class(MhdGridGaussLegendre), Allocatable :: ga
    Real(8) :: Tstart, Tend
    Real(8), Dimension(:,:,:), Allocatable :: g, gp
    Real(8), Dimension(:), Allocatable :: u, up, flu
    Real(8), Dimension(:,:), Allocatable :: fl
    Real(8), Dimension(:), Allocatable :: f

    Integer :: MPI_err, MPI_rank, MPI_size
    Integer::l, i, n, m
    Real(8) :: x, y, r
    Integer :: sz, rk
    
    Real(8) :: vxy(2), vrp(2), a(2,2)
    Class(MhdHydroSolverDG), Allocatable :: solver
    Class(MhdPoisSolver), Allocatable :: pois
    
    !Call config_test()
    !Stop
    
#ifdef ORCHID_MPI    
    Call MPI_Init(MPI_err)
    Call MPI_Comm_Rank(MPI_COMM_WORLD, MPI_rank, MPI_err)
    Call MPI_Comm_Size(MPI_COMM_WORLD, MPI_size, MPI_err)
    !Write(*,*) mpi_err
    !Call MPI_Comm_Size(MPI_COMM_WORLD, sz, mpi_error)
    !Write(*,*) mpi_err
    Write(*,*) MPI_rank, MPI_size
    Call MPI_Finalize(mpi_err)
    Stop
#endif    
    
    !>-------------------------------------------------------------------------------
    !> Write the damn cool Logo.
    Write(*,*) ''
    Write(*,*) '          \\\\\\ \\\\\\    \\\\\\\ \\\ \\\ \\\\\\\ \\\\\\\       ' 
    Write(*,*) '         /// /// ////\\\   /// /// /// ///   ///   ///  \\\      '
    Write(*,*) '        //   // //// ///  ///  // ///////   ///   ///   ///      '
    Write(*,*) '       //   // ///\\\//  ///     /// ///   ///   ///   ///       '
    Write(*,*) '      /// /// ///  \\\  ///     /// ///   ///   ///   ///        '
    Write(*,*) '      \\\\\\\ \\\   \\\ \\\\\\\ \\\ \\\ \\\\\\\ \\\\\\\\         '
    Write(*,*) ''
    Write(*,*) '        \\\\\\    \\\\\\\ \\\    \\\   \\\ \\\\\\\  \\\\\\       ' 
    Write(*,*) '        \\\  \\   /// /// ///    ///   /// ///      ////\\\      '
    Write(*,*) '          \\ //  //   // ///    ///   /// /////    //// ///      '
    Write(*,*) '      //   \\   //   // ///  // /// ///  ///      ///\\\//       '
    Write(*,*) '      \\   /// /// /// /// /// /// ///  ///      ///  \\\        '
    Write(*,*) '       \\\\\\  \\\\\\\ \\\\\\\  \\\\\   \\\\\\\\ \\\   \\\       '
    Write(*,*) ''
    Write(*,*) ''
    !>-------------------------------------------------------------------------------
    
    Call test_sod_1D_dg()
    !Call test_ot_2D()
    Stop

    !Call test_opencl
    
    Allocate(MhdHydroSolverDG :: solver)
    !Allocate(MhdPoisSolver :: pois)
    Allocate(ga)
    Call ga%init1D(10.0D0, 200, -1, -1)
    Call ga%init_gauss1D(2)
    Call ga%init_legendre1D()

    !Do n = ga%ncell_nodes_min, ga%ncell_nodes_max
    !   Write(*,*) ga%cell_nodes(n)%x, ga%cell_nodes(n)%y, ga%cell_nodes(n)%z, ga%cell_nodes(n)%w
    !End Do
    !Stop
   
    !Write(*,*) ga%node2cell
    !Call ga%init2D_polar(3.0D0, 10.0D0, 200, 200, -1, -2)
    !Call ga%init_gauss_dummy()
    !Call ga%init_legendre_dummy()
    !Call ga%init2D(1.0D0, 1.0D0, 100, 100, -2, -2, -2, -2)
    !Write(*, *) ga%faces
    !Call ga%init2D(1.0D0, 1.0D0, 100, 100, -2, -2, -2, -2)
    
    Allocate(f(ga%ncells_min:ga%ncells_max))
    Allocate(u(ga%ncells_min:ga%ncells_max))
    Allocate(up(ga%ncells_min:ga%ncells_max))
    Allocate(flu(ga%nfaces_min:ga%nfaces_max))

    !Do i = ga%ncells_min, ga%ncells_max
    !    x = ga%cells(i)%x
    !    y = ga%cells(i)%y
    !    r = Sqrt(x**2 + y**2)
    !    u(i) = 0.0D0
    !    up(i) = 0.0D0
    !    flu(i) = 0.0D0
    !    !u(i) = (3.0D0 - r)*(10.0D0 - r)
    !    !u(i) = (x - x**2)*(y - y**2)
    !    !f(i) = Sin(4*Pi*x)!Exp(x+y)!-8*Pi**2*Sin(2*Pi*x)*Sin(2*Pi*y)
    !    !f(i) = ( Exp(y)*Exp(x)*x*(x+3.0d0)*( y - y**2 ) + Exp(y)*Exp(x)*y*(y+3.0d0)*( x - x**2 ) )
    !    !f(i) = +2.0D0*( ( y - y**2 ) + ( x - x**2 ) ) 
    !    !f(i) = (Cos(r) - r*Sin(r))/r
    !    f(i) = -(4.0D0*r-13.0D0)/r
    !    !f(i) = 4.0D0*Pi*Pi*Sin(2*Pi*x)*Sin(2*Pi*y)
    !    !f(i) = (x - x**2)!*(y - y**2)
    !End Do
    !Allocate(MhdPoisSolver :: pois)
    !Call pois%init()
    
    !Do n = 1, 1
    !    Call pois%calc(ga, u, up, flu, f)
    !    Write(*,*) n
    !    Deallocate(pois)
    !End Do
    !Call print_grid4(ga, up, 0)
    !Read(*,*)
    !Stop
    
    Allocate(g(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max))
    Allocate(gp(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max))
    Allocate(fl(n_min:n_max, ga%nface_nodes_min:ga%nface_nodes_max))
    
    !> Sod test case.
    If (MHD) Then
        !> MHD Sod test case.
        g(:, :, :) = 0.0D0
        g(0, 1, :) = 1.0D0
        g(0, 2, :) = 1.0D0/( Gamma1*1.0D0 )
        g(0, 6, :) = 4.0D0!/Sqrt(4.0D0*Pi)
        g(0, 7, :) = 4.0D0!/Sqrt(4.0D0*Pi)
        g(0, 8, :) = 2.0D0!/Sqrt(4.0D0*Pi)
        g(0, 1, 1:100) = 1.08D0;
        g(0, 2, 1:100) = 1.08D0*( 0.95D0/( Gamma1*1.08D0 ) + 0.5D0*(1.2D0**2 + 0.01D0**2 + 0.5D0**2) )
        g(0, 3, 1:100) = 1.08D0*1.2D0;
        g(0, 4, 1:100) = 1.08D0*0.01D0;
        g(0, 5, 1:100) = 1.08D0*0.5D0;
        g(0, 7, 1:100) = 3.6D0!/Sqrt(4.0D0*Pi)
    Else
        g(:, :, :) = 0.0D0
        g(0, 1, :) = 1.0D0
        g(0, 2, :) = 1.0D0/( Gamma1*1.0D0 )
        g(0, 1, 1:100) = 2.0D0
        g(0, 2, 1:100) = 10.0D0/( Gamma1*2.0D0 )
    End If
    
    !g(:, :, :) = 0.0D0
    !g(0, 1, :) = 1.0D0
    !g(0, 2, :) = 1.0D0/( Gamma1*1.0D0 )
    !g(0, 3, :) = 1.0D0
    !g(0, 4, :) = 0.0D0
    !g(0, 7, :) = 2.0D0!/Sqrt(4.0D0*Pi)

    Call solver%init()
    Call print_grid4(ga, g, 0)
    m = 1
    Tstart = omp_get_wtime()
    Do l=1, 1800!300000000
        !f(:) = g(1, :)
        !u(:) = up(:)
        !Call pois%calc(ga, u, up, flu, f, n)
        !m = m + n
        Call solver%calc_step_dg(0.001D0, ga, g, gp)
        If (Mod(l, 100) == 0) Then
            Tend = omp_get_wtime()
            Write(*, *) 'time step:', l, TEnd - Tstart, m, (TEnd - Tstart)/Dble(m)
            Tstart = Tend
            m = 1
            Call print_grid4(ga, g, l)
        End If
        g(:,:,:)=gp(:,:,:)
    End Do
End Program orchid_solver
