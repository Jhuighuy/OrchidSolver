!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.
Module orchid_solver_params
    Implicit None
    
    Integer, Parameter :: dim = 1
    Logical, Parameter :: debug = .TRUE.
    Logical, Parameter :: verbose = .TRUE.
    Logical, Parameter :: mhd = .FALSE.

    Real(8), Parameter :: Mu_hydro = 0.0005D0
    
    Real(8), Parameter :: Pi = 4.0D0*Atan(1.0D0), Gamma = 5.0D0/3.0D0, Gamma1 = Gamma-1.0D0
    Integer, Parameter :: N_funcs = 2
    Integer, Parameter :: m_min = 0, m_max = N_funcs - 1
    Integer, Parameter :: n_min = 1, n_max = 8
    Contains
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
    Write (output, *) 'x,y,z,r,e,u,v,w,bx,by,bz'
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
    Use orchid_solver_hydro2
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
    Do l=1, 1800
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


!> Sod test case (Pure FV).
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
    
    Allocate(MhdHydroSolverDG :: solver)
    Call solver%init()
    Call print_grid4(ga, g, 0)
    Tstart = omp_get_wtime()
    Do l=1, 1800
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
    
Program orchid_solver
    Use orchid_solver_simulation
    Use orchid_solver_hydro_dg
    Use orchid_solver_pois
    use omp_lib
    Implicit None
    
    Class(MhdGridGaussLegendre), Allocatable :: ga
    Real(8) :: Tstart, Tend
    Real(8), Dimension(:,:,:), Allocatable :: g, gp
    Real(8), Dimension(:), Allocatable :: u, up, flu
    Real(8), Dimension(:,:), Allocatable :: fl
    Real(8), Dimension(:), Allocatable :: f

    Integer::l, i, n, m
    Real(8) :: x, y, r
    
    Real(8) :: vxy(2), vrp(2), a(2,2)
    Class(MhdHydroSolverDG), Allocatable :: solver
    Class(MhdPoisSolver), Allocatable :: pois
    
    !>-------------------------------------------------------------------------------
    !> Write the damn cool Logo.
    Write(*,*) ''
    Write(*,*) '          \\\\\\ \\\\\\    \\\\\\\ \\\ \\\ \\\\\\\ \\\\\\\      ' 
    Write(*,*) '         /// /// ////\\\   /// /// /// ///   ///   ///  \\\     '
    Write(*,*) '        //   // //// ///  ///  // ///////   ///   ///   ///     '
    Write(*,*) '       //   // ///\\\//  ///     /// ///   ///   ///   ///      '
    Write(*,*) '      /// /// ///  \\\  ///     /// ///   ///   ///   ///       '
    Write(*,*) '      \\\\\\\ \\\   \\\ \\\\\\\ \\\ \\\ \\\\\\\ \\\\\\\\        '
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
    
    Call test_sod_1D()
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
