!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.
Module orchid_solver_params
    Implicit None
    
    Integer, Parameter :: dim = 2
    Logical, Parameter :: debug = .TRUE.
    Logical, Parameter :: verbose = .TRUE.
    
    Character(Len=10) :: hydro_flux = 'roe'
    Character(Len=10) :: hydro_flux_limiter = 'none'

    Character(Len=10) :: R0_bc = 'wall' ! 'periodic'
    Character(Len=10) :: R1_bc = 'free'
    
    Real(8), Parameter :: Pi = 4.0D0*Atan(1.0D0), Gamma = 5.0/3.0, Gamma1 = Gamma-1.0D0
    Real(8), Parameter :: r_0 = 3.0, r_1 = 10.0

    Integer, Parameter :: N_r = 200, N_theta = 1, N_phi = 200, N_funcs=1
    Real(8), Parameter :: L_r = 1.0D0, L_theta = Pi, L_phi = 2.0D0*Pi
    Real(8), Parameter :: h_r = 7.0D0/N_r, h_t = 1+0*L_theta/N_theta, h_tt=2.0D0*Sin(0.5D0*h_t), h_p = L_phi/N_phi
    Integer, Parameter :: m_min = 0, m_max = N_funcs - 1
    
    Real(8), Dimension(0:0, 0:0), Parameter :: Gauss_x = [0.0D0]
    Real(8), Dimension(0:0, 0:0), Parameter :: Gauss_w = [1.0D0]
    
    Contains
    Pure Function mhd_eval1D(vc, i, x) result(v)
        ! {{{
        Real(8), Intent(In) :: i
        Real(8), Intent(In) :: x
        Real(8), Dimension(m_min:m_max), Intent(In) :: vc
        ! }}}
        Real(8) :: v
        v = vc(m_min)
    End Function mhd_eval1D
    Pure Function mhd_eval2D(vc, i, j, x, y) result(v)
        ! {{{
        Real(8), Intent(In) :: i, j
        Real(8), Intent(In) :: x, y
        Real(8), Dimension(m_min:m_max), Intent(In) :: vc
        ! }}}
        Real(8) :: v
        v = vc(m_min)
    End Function mhd_eval2D
    Pure Function mhd_eval3D(vc, i, j, k, x, y, z) result(v)
        ! {{{
        Real(8), Intent(In) :: i, j, k
        Real(8), Intent(In) :: x, y, z
        Real(8), Dimension(m_min:m_max), Intent(In) :: vc
        ! }}}
        Real(8) :: v
        v = vc(m_min)
    End Function mhd_eval3D
    
    Pure Function minmod2(x, y)
        ! {{{
        Real(8), Intent(In) :: x, y
        ! }}}
        Real(8) :: minmod2
        minmod2 = 0.5D0*( Sign(1.0D0, x) + Sign(1.0D0, y) )*Min(Abs(x), Abs(y))
    End Function minmod2
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

    Type mhd_grid
        !> Velocity components.
        !> Density, pressure and internal energy.
        !> @{
        Real(8), Dimension(0:N_r+1, 0:N_phi+1, 1:1, 0:0, 0:0) :: rho, nrg, p
        Real(8), Dimension(0:N_r+1, 0:N_phi+1, 1:1, 0:0, 0:0) :: v_r, v_t, v_p
        !> @}
    End Type mhd_grid
    
    Type mhd_grid_flux
        Type(mhd_grid) r, p, t
    End Type mhd_grid_flux
    
    Type mhd_grid_dg
        !> Velocity components.
        !> Density, pressure and internal energy.
        !> @{
        Real(8), Dimension(0:N_r+1, 0:N_phi+1, 0:N_theta+1, 0:0) :: rho, nrg, p
        Real(8), Dimension(0:N_r+1, 0:N_phi+1, 0:N_theta+1, 0:0) :: v_r, v_t, v_p
        !> @}
    End Type mhd_grid_dg
End Module orchid_solver_grid
    
Module orchid_solver_simulation
Use orchid_solver_grid
Implicit None

Contains
Subroutine print_grid(g, l)
    Type(mhd_grid_dg), Intent(In) :: g
    Integer, Intent(In) :: l
    Integer :: i, j, k
    Integer :: output
    Real(8) :: r, theta, phi
    Real(8) :: x, y, z
    Real(8) :: v_r, v_t, v_p
    Real(8) :: v_x, v_y, v_z, v_l
    Real(8) :: vxy(2), vrp(2), a(2,2), vvv
    Open(NewUnit=output, file='../Results/fields-'//Trim(to_str(l))//'.csv', Status='replace')
    If ( dim==2 ) Then
        Write (output,*) 'x,y,u,v,r,e'
        Do i = 1, N_r
        Do j = 1, N_phi
            r = r_0 + (i - 0.5D0)*h_r
            phi = (j - 0.5D0)*h_p
            x = r*Cos(Phi)
            y = r*Sin(Phi)
                        a(1,:) = [Cos(phi), -Sin(Phi)]
        a(2,:) = [Sin(Phi), Cos(Phi)]
        a = Transpose(a)
        vxy = MatMul(a, [g%v_r(i, j, 1, 0)/g%rho(i, j, 1, 0), g%v_p(i, j, 1, 0)/g%rho(i, j, 1, 0)])*0.5
            Write (output, '(E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6,A,E12.6)') &
                x, ',', y, ',', &!r, phi,&!
                vxy(1), ',', vxy(2), ',', &
                g%rho(i, j, 1, 0), ',', g%nrg(i, j, 1, 0)/g%rho(i, j, 1, 0)
        End Do 
        End Do
    Else
        j = 1
        Do i = 1, N_r
            r = r_0 + (i - 0.5D0)*h_r
            phi = (j - 0.5D0)*h_p
            x = r*Cos(Phi)
            y = r*Sin(Phi)
            Write (output, '(40e,40e,40e,40e,40e)') &
                r,& !x, y, &
                g%v_r(i, j, 1, 0)/g%rho(i, j, 1, 0),&
                g%rho(i, j, 1, 0), g%nrg(i, j, 1, 0)/g%rho(i, j, 1, 0)
        End Do
    End If
    Close(output)
End Subroutine print_grid
    
End Module orchid_solver_simulation
    
Program orchid_solver
    Use orchid_solver_simulation
    Use orchid_solver_hydro
    
    Type(mhd_grid_dg), Allocatable :: g, gp
    Type(mhd_grid_flux), Allocatable :: flux
    
    Integer::l
    
    Real(8) :: vxy(2), vrp(2), a(2,2)
    
    Allocate(g)
    Allocate(gp)
    Allocate(flux)
    
    l = Sign(1, -10)
    
    g%rho(:, :, :, :) = 1.0D0;
    g%nrg(:, :, :, :) = 1.0D0/( Gamma1*1.0D0 );
    Do i=1,n_r
        Do j = 1,n_phi
            r = r_0 + (i - 0.5D0)*h_r
            phi = (j - 0.5D0)*h_p
            vxy = [1.0D0,0.0D0]
            a(1,:) = [Cos(phi), -Sin(Phi)]
            a(2,:) = [Sin(Phi), Cos(Phi)]
            vrp = MatMul(a, vxy)
            g%v_r(i,j,:,:)=vrp(1)
            g%v_p(i,j,:,:)=vrp(2)
        End Do
    End Do
    
    !g%rho(0:100,:, :, :) = 2.0D0;
    !g%nrg(0:100,:, :, :) = 10.0D0/( Gamma1*2.0D0 );
    !g%rho(0:100, 80:120, :, :) = 2.0D0;
    !g%nrg(0:100, 80:120, :, :) = 10.0D0/( Gamma1*2.0D0 );
    
    
    Call print_grid(g, 0)
    Do l=1,2000000000
        Call mhd_hydro_update(0.001D0, g, gp, flux)
        If (Mod(l, 100) == 0) Then
            Write(*, *) 'time step:', l
            Call print_grid(g, l)
        End If
        g=gp
    End Do
    
    !g%p(:, :, :) = 1.0D0;
    !g%rho(21:101, :, :) = 3.0D0;
    
    !Call print_grid(g, 0)
    !Do l=1,150000
    !    Call simulate_euler_fdm(0.001D0, g, gp)
    !    
    !    
    !    If (Mod(l, 100) == 0) Then
    !        Write(*, *) 'time step:', l, Sqrt(MaxVal(Abs(gp%v_r))**2 + MaxVal(Abs(gp%v_t))**2 + MaxVal(Abs(gp%v_p))**2), &
    !        MaxVal(gp%rho)-1, MaxVal(gp%nrg)-1
    !        Call print_grid(gp, l)
    !    End If
    !    
    !    g=gp
    !End Do
End Program orchid_solver
