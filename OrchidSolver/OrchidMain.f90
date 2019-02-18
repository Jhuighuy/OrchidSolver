!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.
Module orchid_solver_params
    Implicit None
    
    Integer, Parameter :: dim = 2
    Logical, Parameter :: debug = .TRUE.
    Logical, Parameter :: verbose = .TRUE.
    Logical, Parameter :: mhd = .FALSE.
    
    Character(Len=10) :: hydro_flux = 'hllc'
    Character(Len=10) :: hydro_flux_limiter = 'none'

    Character(Len=10) :: R0_bc = 'free' ! 'periodic'
    Character(Len=10) :: R1_bc = 'free'
    
    Character(Len=10) :: MHD_R0_BOUNDARY_COND = 'wall' ! 'periodic'
    Character(Len=10) :: MHD_R1_BOUNDARY_COND = 'free'
    
    
    Real(8), Parameter :: Pi = 4.0D0*Atan(1.0D0), Gamma = 1.4D0, Gamma1 = Gamma-1.0D0
    Real(8), Parameter :: r_0 = 3.0, r_1 = 10.0
    Real(8), Parameter :: r0 = 3.0, r1 = 10.0

    Integer, Parameter :: N_r = 200, N_theta = 1, N_phi = 200, N_funcs=1
    Real(8), Parameter :: L_r = 1.0D0, L_theta = Pi, L_phi = 2.0D0*Pi
    Real(8), Parameter :: h_r = 7.0D0/N_r, h_t = 1+0*L_theta/N_theta, h_tt=2.0D0*Sin(0.5D0*h_t), h_p = L_phi/N_phi
    Integer, Parameter :: m_min = 0, m_max = N_funcs - 1
    Integer, Parameter :: n_min = 1, n_max = 8
    Integer, Parameter :: i_min = 1, i_max = N_r
    Integer, Parameter :: j_min = 1, j_max = N_phi
    Integer, Parameter :: k_min = 1, k_max = 1
    Integer, Parameter :: f_min = 1, f_max = dim
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
    
Module orchid_solver_grid
    Use orchid_solver_params
    Use orchid_solver_helpers
    Implicit None


End Module orchid_solver_grid
    
Module orchid_solver_simulation
Use orchid_solver_grid
Use orchid_solver_opencl
Implicit None

Contains

Subroutine print_grid2(g, l)
    !> {{{
    Integer, Intent(In) :: l
    Real(8), Dimension(n_min:n_max, &
                       i_min:i_max, &
                       j_min:j_max, &
                       k_min:k_max), Intent(In) :: g
    !> }}}
    Integer :: i, j, k
    Integer :: output
    Real(8) :: r, theta, phi
    Real(8) :: x, y, z
    Real(8) :: v_r, v_t, v_p
    Real(8) :: v_x, v_y, v_z, v_l
    Real(8) :: vxy(2), vrp(2), a(2,2), vvv
    Open(NewUnit=output, file='../Results/fields-'//Trim(to_str(l))//'.csv', Status='replace')
    k = 1
    If ( dim==1 ) Then
        j = j_min
        k = k_min
        Do i = 1, N_r
            !x,r,e,u,v,w,bx,by,bz
            x = i*h_r
            Write (output, '(E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A)') &
                x, ',', &
                g(1, i, j, k), ',', g(2, i, j, k)/g(1, i, j, k), ',', &
                g(3, i, j, k)/g(1, i, j, k), ',', g(4, i, j, k)/g(1, i, j, k), ',', &
                g(5, i, j, k)/g(1, i, j, k), ',', &
                g(6, i, j, k)/Sqrt(4.0D0*Pi), ',', g(7, i, j, k)/Sqrt(4.0D0*Pi), ',', g(8, i, j, k)/Sqrt(4.0D0*Pi)
        End Do 
    End If
    If ( dim==2 ) Then
        Write (output,*) 'x,y,r,e,u,v,w,bx,by,bz'
        Do i = 1, N_r
        Do j = 1, N_phi
            r = r_0 + (i - 0.5D0)*h_r
            phi = (j - 0.5D0)*h_p
            x = r*Cos(Phi)
            y = r*Sin(Phi)
                Write (output, '(E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A)') &
                    x, ',', y, ',', &
                    g(1, i, j, k), ',', g(2, i, j, k)/g(1, i, j, k), ',', &
                    g(3, i, j, k)/g(1, i, j, k), ',', g(4, i, j, k)/g(1, i, j, k), ',', &
                    g(5, i, j, k)/g(1, i, j, k), ',', &
                    g(6, i, j, k)/Sqrt(4.0D0*Pi), ',', g(7, i, j, k)/Sqrt(4.0D0*Pi), ',', g(8, i, j, k)/Sqrt(4.0D0*Pi)
            
            !a(1,:) = [Cos(phi), -Sin(Phi)]
            !a(2,:) = [Sin(Phi), Cos(Phi)]
            !a = Transpose(a)
            !!vxy = MatMul(a, [g%v_r(i, j, 1, 0)/g%rho(i, j, 1, 0), g%v_p(i, j, 1, 0)/g%rho(i, j, 1, 0)])*0.5
            !Write (output, '(E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6)') &
            !    x, ',', y, ',', &!r, phi,&!
            !    vxy(1), ',', vxy(2), ',', &
            !    g(1, i, j, k), ',', g(2, i, j, k)/g(1, i, j, k)
        End Do 
        End Do
    Else
    End If
    Close(output)
End Subroutine print_grid2


    
End Module orchid_solver_simulation
    
Program orchid_solver
    Use orchid_solver_simulation
    Use orchid_solver_hydro2
    Use orchid_solver_opencl2
    
    Real(8), Dimension(:,:,:,:), Allocatable :: g2, gp2
    Real(8), Dimension(:,:,:,:,:), Allocatable :: f2

    Integer::l
    
    Real(8) :: vxy(2), vrp(2), a(2,2)
    Class(MhdHydroSolver), Allocatable :: solver
    
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
    
    Call test_opencl
    
    Allocate(solver)
    Allocate(g2(n_min:n_max, &
                       i_min:i_max, &
                       j_min:j_max, &
                       k_min:k_max))
    Allocate(gp2(n_min:n_max, &
                       i_min:i_max, &
                       j_min:j_max, &
                       k_min:k_max))
    Allocate(f2(n_min:n_max, &
                       i_min-1:i_max, &
                       j_min-1:j_max, &
                       k_min-1:k_max, f_min:f_max))
    
    !> Sod test case.
    !g2(1, :, :, :) = 1.0D0
    !g2(2, :, :, :) = 1.0D0/( Gamma1*1.0D0 )
    !g2(6, :, :, :) = 4.0D0!/Sqrt(4.0D0*Pi)
    !g2(7, :, :, :) = 4.0D0!/Sqrt(4.0D0*Pi)
    !g2(8, :, :, :) = 2.0D0!/Sqrt(4.0D0*Pi)
    !g2(1, 1:100, :, :) = 1.08D0;
    !g2(2, 1:100, :, :) = 1.08D0*( 0.95D0/( Gamma1*1.08D0 ) + 0.5D0*(1.2D0**2 + 0.01D0**2 + 0.5D0**2) )
    !g2(3, 1:100, :, :) = 1.08D0*1.2D0;
    !g2(4, 1:100, :, :) = 1.08D0*0.01D0;
    !g2(5, 1:100, :, :) = 1.08D0*0.5D0;
    !g2(7, 1:100, :, :) = 3.6D0!/Sqrt(4.0D0*Pi)
    
    g2(:, :, :, :) = 0.0D0
    g2(1, :, :, :) = 1.0D0
    g2(2, :, :, :) = 1.0D0/( Gamma1*1.0D0 )
    g2(3, :, :, :) = 1.0D0
    !g2(3, :, :, :) = 0.0D0
    g2(4, :, :, :) = 0.0D0
    Call solver%init()
    Call print_grid2(g2, 0)
    Do l=1,2000000000
        Call solver%calc_step(0.001D0, g2, gp2, f2)
        If (Mod(l, 100) == 0) Then
            Write(*, *) 'time step:', l
            Call print_grid2(g2, l)
        End If
        g2(:,:,:,:)=gp2(:,:,:,:)
    End Do
End Program orchid_solver
