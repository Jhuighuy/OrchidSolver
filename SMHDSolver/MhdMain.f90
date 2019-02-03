!> Copyright (C) Butakov Oleg 2019.
!> 3D MagnetoHydroDynamics solver in spherical coorinates.
Module mhd_solver_params
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
    
    Contains
    
    Pure Function minmod2(x, y)
        ! {{{
        Real(8), Intent(In) :: x, y
        ! }}}
        Real(8) :: minmod2
        minmod2 = 0.5D0*( Sign(1.0D0, x) + Sign(1.0D0, y) )*Min(Abs(x), Abs(y))
    End Function minmod2
End Module mhd_solver_params
    
Module mhd_solver_helpers
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
End Module mhd_solver_helpers
    
Module mhd_solver_grid
    Use mhd_solver_params
    Use mhd_solver_helpers
    Implicit None

    Type mhd_grid
        !> Velocity components.
        !> Density, pressure and internal energy.
        !> @{
        Real(8), Dimension(0:N_r+1, 0:N_phi+1, 1:1) :: rho, nrg, p
        Real(8), Dimension(0:N_r+1, 0:N_phi+1, 1:1) :: v_r, v_t, v_p
        !> @}
    End Type mhd_grid
    
    Type mhd_grid_flux
        Type(mhd_grid) r, p, t
    End Type mhd_grid_flux
    
    Type mhd_grid_dg
        !> Velocity components.
        !> Density, pressure and internal energy.
        !> @{
        Real(8), Dimension(0:N_r+1, 0:N_phi+1, 0:N_theta+1, 0:N_funcs) :: rho, nrg, p
        Real(8), Dimension(0:N_r+1, 0:N_phi+1, 0:N_theta+1, 0:N_funcs) :: v_r, v_t, v_p
        !> @}
    End Type mhd_grid_dg
End Module mhd_solver_grid
    
Module mhd_solver_simulation
    Use mhd_solver_grid
    Implicit None
    
    Contains


!    Subroutine simulate_euler_fdm(Tau, g, gp)!, FluxType)
!        ! {{{
!        Real(8), Intent(In) :: Tau
!        Type(mhd_grid), Intent(InOut) :: g, gp
!        ! }}}
!        
!        Character(Len=20) :: FluxType
!        
!        Integer :: fluxes
!        Integer :: i, j, k
!        Real(8) :: r, theta, phi, test
!        
!        ! Reclare the Point.
!        Real(8) :: r_i, r_iph, r_imh
!        Real(8) :: Theta_j, Theta_jph, Theta_jmh
!        Real(8) :: Phi_k, Phi_kph, Phi_kmh
!        
!        ! Declare the Basis function values.
!        Real(8) :: Psi_m, &
!                   Psi_m_iph, Psi_m_jph, Psi_m_kph, &
!                   Psi_m_imh, Psi_m_jmh, Psi_m_kmh
!        
!        ! Declare the Fluxes.
!        Real(8) :: flux_rho_iph, flux_rho_jph, flux_rho_kph, &
!                   flux_rho_imh, flux_rho_jmh, flux_rho_kmh
!        Real(8) :: flux_v_r_iph, flux_v_r_jph, flux_v_r_kph, &
!                   flux_v_r_imh, flux_v_r_jmh, flux_v_r_kmh, &
!                   flux_v_t_iph, flux_v_t_jph, flux_v_t_kph, &
!                   flux_v_t_imh, flux_v_t_jmh, flux_v_t_kmh, &
!                   flux_v_p_iph, flux_v_p_jph, flux_v_p_kph, &
!                   flux_v_p_imh, flux_v_p_jmh, flux_v_p_kmh
!        Real(8) :: flux_nrg_iph, flux_nrg_jph, flux_nrg_kph, &
!                   flux_nrg_imh, flux_nrg_jmh, flux_nrg_kmh
!        ! Declare the Flux coefficients.
!        Real(8) :: coef_sin, coef_cos
!        Real(8) :: coef_iph, coef_jph, coef_kph, &
!                   coef_imp, coef_jmh, coef_kmh 
!        
!        ! Declare the Matrix coefficients.
!        Real(8) :: ap11, ap12, ap13, ap21, ap22, ap23, ap31, ap32, ap33, &
!                   am11, am12, am13, am21, am22, am23, am31, am32, am33, &
!                   bp11, bp12, bp13, bp21, bp22, bp23, bp31, bp32, bp33, &
!                   bm11, bm12, bm13, bm21, bm22, bm23, bm31, bm32, bm33, &
!                   cp11, cp12, cp13, cp21, cp22, cp23, cp31, cp32, cp33, &
!                   cm11, cm12, cm13, cm21, cm22, cm23, cm31, cm32, cm33
!                   
!        ! Velocity at +-R.
!        Real(8) :: u_r_p, u_t_p, u_p_p, u_r_m, u_t_m, u_p_m, u_r_s, u_t_s, u_p_s, q,gg,b,&
!        rho_m, rho_p, rho_s, ent_m, ent_p, ent_s, c2_m, c2_s, c2_p, c_s, p_p, p_m
!        
!        Real(8), Dimension(1:3, 1:3) :: M
!        Real(8), Dimension(1:3) ::u_p_sph, u_m_xyz, u_m_sph, u_p_xyz, u_s_xyz
!        Real(8), Dimension(1:3) ::flux_rho_sph, flux_v_r_sph, flux_v_t_sph, flux_v_p_sph, flux_nrg_sph, &
!                                  flux_rho_xyz, flux_v_r_xyz, flux_v_t_xyz, flux_v_p_xyz, flux_nrg_xyz
!        Real(8), Dimension(1:5) :: q_p, q_m, q_s, f_p, f_m, f_s
!        Real(8), Dimension(1:5, 1:5) :: S,SL,SD,SR
!        
!        g%rho(0, :, :) = 10.0D0
!        
!        !$OMP Parallel Do
!        Do k = 0, N_phi+1
!        Do j = 0, N_theta+1
!        Do i = 0, N_r+1
!            g%p(i, j, k) = (5.0D0/3.0D0 - 1.0D0)*(g%nrg(i, j, k) - (g%v_r(i, j, k)**2 + g%v_t(i, j, k)**2 &
!            + g%v_p(i, j, k)**2)/(2.0D0*g%rho(i, j, k)))
!        End Do
!        End Do
!        End Do
!        !$OMP End Parallel Do
!        g%rho(:, :, 0) = g%rho(:, :, N_phi)
!        g%rho(:, :, N_phi+1) = g%rho(:, :, 1)
!        
!        g%v_r(:, :, 0) = g%v_r(:, :, N_phi)
!        g%v_r(:, :, N_phi+1) = g%v_r(:, :, 1)
!        g%v_t(:, :, 0) = g%v_t(:, :, N_phi)
!        g%v_t(:, :, N_phi+1) = g%v_t(:, :, 1)
!        g%v_p(:, :, 0) = g%v_p(:, :, N_phi)
!        g%v_p(:, :, N_phi+1) = g%v_p(:, :, 1)
!        
!        g%nrg(:, :, 0) = g%nrg(:, :, N_phi)
!        g%nrg(:, :, N_phi+1) = g%nrg(:, :, 1)
!        
!        gp = g
!        
!        FluxType = 'fdm'
!        
!        SL(:,:) = 0.0
!        SR(:,:) = 0.0
!        SD(:,:) = 0.0
!        
!        !$OMP Parallel Do
!        Do k = 1, N_phi
!        Do j = 1, N_theta
!        Do i = 1, N_r
!                    
!            ! >----------------------------------------------------------------------------
!            ! > Calculate the Points.
!            ! >----------------------------------------------------------------------------
!            r_i = (i - 0.5D0)*h_r
!            r_iph = r_i + 0.5D0*h_r
!            r_imh = r_i - 0.5D0*h_r
!            Theta_j = (j - 0.5D0)*h_t
!            Theta_jph = Theta_j + 0.5D0*h_t
!            Theta_jmh = Theta_j - 0.5D0*h_t
!            Phi_k = (k - 0.5D0)*h_p
!            Phi_kph = Phi_k + 0.5D0*h_p
!            Phi_kmh = Phi_k - 0.5D0*h_p
!                      
!            ! >----------------------------------------------------------------------------
!            ! > Calculate Fluxes at +R.
!            ! >----------------------------------------------------------------------------  
!            ! > Calculate parameters at +R+
!            rho_p = g%rho(i+1, j, k)
!            ent_p = ( g%nrg(i+1, j, k) + g%p(i+1, j, k) )/rho_p
!            u_r_p = g%v_r(i+1, j, k)/rho_p
!            u_t_p = g%v_t(i+1, j, k)/rho_p
!            u_p_p = g%v_p(i+1, j, k)/rho_p
!            c2_p  = Gamma*g%p(i+1, j, k)/rho_p
!            q_p = [rho_p, rho_p*u_r_p, rho_p*u_t_p, rho_p*u_p_p, rho_p*ent_p - g%p(i+1, j, k) ]
!            f_p = [rho_p*u_r_p, rho_p*u_r_p*u_r_p+g%p(i+1, j, k), rho_p*u_r_p*u_t_p, rho_p*u_r_p*u_p_p, rho_p*u_r_p*ent_p]
!            ! > Calculate parameters at +R-
!            rho_m = g%rho(i+0, j, k)
!            ent_m = ( g%nrg(i+0, j, k) + g%p(i+0, j, k) )/rho_m
!            u_r_m = g%v_r(i+0, j, k)/rho_m
!            u_t_m = g%v_t(i+0, j, k)/rho_m
!            u_p_m = g%v_p(i+0, j, k)/rho_m
!            c2_m  = Gamma*g%p(i+0, j, k)/rho_m
!            q_m = [rho_m, rho_m*u_r_m, rho_m*u_t_m, rho_m*u_p_m, rho_m*ent_m - g%p(i+0, j, k) ]
!            f_m = [rho_m*u_r_m, rho_m*u_r_m*u_r_m+g%p(i+0, j, k), rho_m*u_r_m*u_t_m, rho_m*u_r_m*u_p_m, rho_m*u_r_m*ent_m]
!            ! > Calculate +R*
!            b = Gamma-1.0D0
!            rho_s = Sqrt(rho_p*rho_m)
!            ent_s = ( Sqrt(rho_p)*ent_p + Sqrt(rho_m)*ent_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_r_s = ( Sqrt(rho_p)*u_r_p + Sqrt(rho_m)*u_r_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_t_s = ( Sqrt(rho_p)*u_t_p + Sqrt(rho_m)*u_t_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_p_s = ( Sqrt(rho_p)*u_p_p + Sqrt(rho_m)*u_p_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            c2_s = ( Sqrt(rho_p)*c2_p + Sqrt(rho_m)*c2_m )/( Sqrt(rho_p) + Sqrt(rho_m) ) &
!            + b*rho_s/(rho_m + 2*rho_s + rho_p)*( (u_r_p-u_r_m)**2 + (u_t_p-u_t_m)**2 + (u_p_p-u_p_m)**2 )
!            c_s = Sqrt(c2_s)
!            ! > Calculate flux matrix.
!            q = u_r_s**2 + u_t_s**2 + u_p_s**2
!            gg = 0.5D0*q**2
!            SR(1,:) = [1.0D0,             0.0D0, 0.0D0, 1.0D0,        1.0D0          ]
!            SR(2,:) = [u_r_s-c_s,         0.0D0, 0.0D0, u_r_s,        u_r_s+c_s      ]
!            SR(3,:) = [u_t_s,             1.0D0, 0.0D0, u_t_s,        u_t_s          ]
!            SR(4,:) = [u_p_s,             0.0D0, 1.0D0, u_p_s,        u_p_s          ]
!            SR(5,:) = [ent_s - u_r_s*c_s, u_t_s, u_p_s, ent_s-c2_s/b, ent_s+u_r_s*c_s]
!            SD(:,:) = 0.0D0
!            SD(1,1) = u_r_s-c_s
!            SD(2,2) = u_r_s
!            SD(3,3) = u_r_s
!            SD(4,4) = u_r_s
!            SD(5,5) = u_r_s+c_s
!            SD = Abs(SD)
!            SL(1,:) = [gg+u_r_s*c_s,           -u_r_s-c_s/b, -u_t_s,       -u_p_s,       1.0D0] 
!            SL(2,:) = [-2.0D0*u_t_s*c2_s/b,    0.0D0,        2.0D0*c2_s/b, 0.0D0,        0.0D0]
!            SL(3,:) = [-2.0D0*u_p_s*c2_s/b,    0.0D0,        0.0D0,        2.0D0*c2_s/b, 0.0D0]
!            SL(4,:) = [2.0D0*ent_s-2.0D0*q**2, 2.0D0*u_r_s,  2.0D0*u_t_s,  2.0D0*u_p_s,  -2.0D0]
!            SL(5,:) = [gg-u_r_s*c_s,           -u_r_s+c_s/b, -u_t_s,       -u_p_s,       1.0D0]
!            SL = SL*b/(2.0D0*c2_s)
!            !f_s = 0.5D0*(f_p+f_m) - 0.5D0*MatMul(SR, MatMul(SD, MatMul(SL, q_p-q_m)))
!            f_s = 0.5D0*(f_p+f_m) - 0.5D0*MaxVal(Abs(SD))*(q_p-q_m) 
!            flux_rho_iph = f_s(1)
!            flux_v_r_iph = f_s(2)
!            flux_v_t_iph = f_s(3)
!            flux_v_p_iph = f_s(4)
!            flux_nrg_iph = f_s(5)
!            
!            ! >----------------------------------------------------------------------------
!            ! > Calculate Fluxes at -R.
!            ! >----------------------------------------------------------------------------  
!            ! > Calculate parameters at -R+
!            rho_p = g%rho(i-0, j, k)
!            ent_p = ( g%nrg(i-0, j, k) + g%p(i-0, j, k) )/rho_p
!            u_r_p = g%v_r(i-0, j, k)/rho_p
!            u_t_p = g%v_t(i-0, j, k)/rho_p
!            u_p_p = g%v_p(i-0, j, k)/rho_p
!            c2_p  = Gamma*g%p(i-0, j, k)/rho_p
!            q_p = [rho_p, rho_p*u_r_p, rho_p*u_t_p, rho_p*u_p_p, rho_p*ent_p - g%p(i-0, j, k) ]
!            f_p = [rho_p*u_r_p, rho_p*u_r_p*u_r_p+g%p(i-0, j, k), rho_p*u_r_p*u_t_p, rho_p*u_r_p*u_p_p, rho_p*u_r_p*ent_p]
!            ! > Calculate parameters at -R-
!            rho_m = g%rho(i-1, j, k)
!            ent_m = ( g%nrg(i-1, j, k) + g%p(i-1, j, k) )/rho_m
!            u_r_m = g%v_r(i-1, j, k)/rho_m
!            u_t_m = g%v_t(i-1, j, k)/rho_m
!            u_p_m = g%v_p(i-1, j, k)/rho_m
!            c2_m  = Gamma*g%p(i-1, j, k)/rho_m
!            q_m = [rho_m, rho_m*u_r_m, rho_m*u_t_m, rho_m*u_p_m, rho_m*ent_m - g%p(i-1, j, k) ]
!            f_m = [rho_m*u_r_m, rho_m*u_r_m*u_r_m+g%p(i-1, j, k), rho_m*u_r_m*u_t_m, rho_m*u_r_m*u_p_m, rho_m*u_r_m*ent_m]
!            ! > Calculate +R*
!            b = Gamma-1.0D0
!            rho_s = Sqrt(rho_p*rho_m)
!            ent_s = ( Sqrt(rho_p)*ent_p + Sqrt(rho_m)*ent_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_r_s = ( Sqrt(rho_p)*u_r_p + Sqrt(rho_m)*u_r_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_t_s = ( Sqrt(rho_p)*u_t_p + Sqrt(rho_m)*u_t_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_p_s = ( Sqrt(rho_p)*u_p_p + Sqrt(rho_m)*u_p_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            c2_s = ( Sqrt(rho_p)*c2_p + Sqrt(rho_m)*c2_m )/( Sqrt(rho_p) + Sqrt(rho_m) ) &
!            + b*rho_s/(rho_m + 2*rho_s + rho_p)*( (u_r_p-u_r_m)**2 + (u_t_p-u_t_m)**2 + (u_p_p-u_p_m)**2 )
!            c_s = Sqrt(c2_s)
!            ! > Calculate flux matrix.
!            q = u_r_s**2 + u_t_s**2 + u_p_s**2
!            gg = 0.5D0*q**2
!            SR(1,:) = [1.0D0,             0.0D0, 0.0D0, 1.0D0,        1.0D0          ]
!            SR(2,:) = [u_r_s-c_s,         0.0D0, 0.0D0, u_r_s,        u_r_s+c_s      ]
!            SR(3,:) = [u_t_s,             1.0D0, 0.0D0, u_t_s,        u_t_s          ]
!            SR(4,:) = [u_p_s,             0.0D0, 1.0D0, u_p_s,        u_p_s          ]
!            SR(5,:) = [ent_s - u_r_s*c_s, u_t_s, u_p_s, ent_s-c2_s/b, ent_s+u_r_s*c_s]
!            SD(:,:) = 0.0D0
!            SD(1,1) = u_r_s-c_s
!            SD(2,2) = u_r_s
!            SD(3,3) = u_r_s
!            SD(4,4) = u_r_s
!            SD(5,5) = u_r_s+c_s
!            SD = Abs(SD)
!            SL(1,:) = [gg+u_r_s*c_s,           -u_r_s-c_s/b, -u_t_s,       -u_p_s,       1.0D0] 
!            SL(2,:) = [-2.0D0*u_t_s*c2_s/b,    0.0D0,        2.0D0*c2_s/b, 0.0D0,        0.0D0]
!            SL(3,:) = [-2.0D0*u_p_s*c2_s/b,    0.0D0,        0.0D0,        2.0D0*c2_s/b, 0.0D0]
!            SL(4,:) = [2.0D0*ent_s-2.0D0*q**2, 2.0D0*u_r_s,  2.0D0*u_t_s,  2.0D0*u_p_s,  -2.0D0]
!            SL(5,:) = [gg-u_r_s*c_s,           -u_r_s+c_s/b, -u_t_s,       -u_p_s,       1.0D0]
!            SL = SL*b/(2.0D0*c2_s)
!            f_s = 0.5D0*(f_p+f_m) - 0.5D0*MatMul(SR, MatMul(SD, MatMul(SL, q_p-q_m)))
!            !f_s = 0.5D0*(f_p+f_m) - 0.5D0*MaxVal(Abs(SD))*(q_p-q_m) 
!            flux_rho_imh = f_s(1)
!            flux_v_r_imh = f_s(2)
!            flux_v_t_imh = f_s(3)
!            flux_v_p_imh = f_s(4)
!            flux_nrg_imh = f_s(5)
!            
!            ! >----------------------------------------------------------------------------
!            ! > Calculate Fluxes at +Theta.
!            ! >----------------------------------------------------------------------------  
!            ! > Calculate parameters at +Theta+
!            rho_p = g%rho(i, j+1, k)
!            ent_p = ( g%nrg(i, j+1, k) + g%p(i, j+1, k) )/rho_p
!            u_r_p = g%v_r(i, j+1, k)/rho_p
!            u_t_p = g%v_t(i, j+1, k)/rho_p
!            u_p_p = g%v_p(i, j+1, k)/rho_p
!            c2_p  = Gamma*g%p(i, j+1, k)/rho_p
!            q_p = [rho_p, rho_p*u_r_p, rho_p*u_t_p, rho_p*u_p_p, rho_p*ent_p - g%p(i, j+1, k) ]
!            f_p = [rho_p*u_t_p, rho_p*u_r_p*u_t_p, rho_p*u_t_p*u_t_p+g%p(i, j+1, k), rho_p*u_t_p*u_p_p, rho_p*u_t_p*ent_p]
!            ! > Calculate parameters at +Theta-
!            rho_m = g%rho(i, j+0, k)
!            ent_m = ( g%nrg(i, j+0, k) + g%p(i, j+0, k) )/rho_m
!            u_r_m = g%v_r(i, j+0, k)/rho_m
!            u_t_m = g%v_t(i, j+0, k)/rho_m
!            u_p_m = g%v_p(i, j+0, k)/rho_m
!            c2_m  = Gamma*g%p(i, j+0, k)/rho_m
!            q_m = [rho_m, rho_m*u_r_m, rho_m*u_t_m, rho_m*u_p_m, rho_m*ent_m - g%p(i, j+0, k) ]
!            f_m = [rho_m*u_t_m, rho_m*u_r_m*u_t_m, rho_m*u_t_m*u_t_m+g%p(i, j+0, k), rho_m*u_t_m*u_p_m, rho_m*u_t_m*ent_m]
!            ! > Calculate +Theta*
!            b = Gamma-1.0D0
!            rho_s = Sqrt(rho_p*rho_m)
!            ent_s = ( Sqrt(rho_p)*ent_p + Sqrt(rho_m)*ent_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_r_s = ( Sqrt(rho_p)*u_r_p + Sqrt(rho_m)*u_r_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_t_s = ( Sqrt(rho_p)*u_t_p + Sqrt(rho_m)*u_t_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_p_s = ( Sqrt(rho_p)*u_p_p + Sqrt(rho_m)*u_p_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            c2_s = ( Sqrt(rho_p)*c2_p + Sqrt(rho_m)*c2_m )/( Sqrt(rho_p) + Sqrt(rho_m) ) &
!            + b*rho_s/(rho_m + 2*rho_s + rho_p)*( (u_r_p-u_r_m)**2 + (u_t_p-u_t_m)**2 + (u_p_p-u_p_m)**2 )
!            c_s = Sqrt(c2_s)
!            ! > Calculate flux matrix.
!            q = u_r_s**2 + u_t_s**2 + u_p_s**2
!            gg = 0.5D0*q**2
!            !SR(1,:) = [1.0D0,     0.0D0,             0.0D0, 1.0D0,        1.0D0          ]
!            !SR(2,:) = [u_r_s,     1.0D0,             0.0D0, u_r_s,        u_r_s          ]
!            !SR(3,:) = [u_t_s-c_s, 0.0D0,             0.0D0, u_t_s,        u_t_s+c_s      ]
!            !SR(4,:) = [u_p_s,     0.0D0,             1.0D0, u_p_s,        u_p_s          ]
!            !SR(5,:) = [u_r_s,     ent_s - u_t_s*c_s, u_p_s, ent_s-c2_s/b, ent_s+u_t_s*c_s]
!            SD(:,:) = 0.0D0
!            SD(1,1) = u_t_s-c_s
!            SD(2,2) = u_t_s
!            SD(3,3) = u_t_s
!            SD(4,4) = u_t_s
!            SD(5,5) = u_t_s+c_s
!            SD = Abs(SD)
!            !SL(1,:) = [gg+u_r_s*c_s,           -u_r_s-c_s/b, -u_t_s,       -u_p_s,       1.0D0] 
!            !SL(2,:) = [-2.0D0*u_t_s*c2_s/b,    0.0D0,        2.0D0*c2_s/b, 0.0D0,        0.0D0]
!            !SL(3,:) = [-2.0D0*u_p_s*c2_s/b,    0.0D0,        0.0D0,        2.0D0*c2_s/b, 0.0D0]
!            !SL(4,:) = [2.0D0*ent_s-2.0D0*q**2, 2.0D0*u_r_s,  2.0D0*u_t_s,  2.0D0*u_p_s,  -2.0D0]
!            !SL(5,:) = [gg-u_r_s*c_s,           -u_r_s+c_s/b, -u_t_s,       -u_p_s,       1.0D0]
!            SL = SL*b/(2.0D0*c2_s)
!            f_s = 0.5D0*(f_p+f_m) - 0.5D0*MatMul(SR, MatMul(SD, MatMul(SL, q_p-q_m)))
!            !f_s = 0.5D0*(f_p+f_m) - 0.5D0*MaxVal(Abs(SD))*(q_p-q_m) 
!            flux_rho_jph = f_s(1)
!            flux_v_r_jph = f_s(2)
!            flux_v_t_jph = f_s(3)
!            flux_v_p_jph = f_s(4)
!            flux_nrg_jph = f_s(5)
!            
!            ! >----------------------------------------------------------------------------
!            ! > Calculate Fluxes at -Theta.
!            ! >----------------------------------------------------------------------------  
!            ! > Calculate parameters at -Theta+
!            rho_p = g%rho(i, j-0, k)
!            ent_p = ( g%nrg(i, j-0, k) + g%p(i, j-0, k) )/rho_p
!            u_r_p = g%v_r(i, j-0, k)/rho_p
!            u_t_p = g%v_t(i, j-0, k)/rho_p
!            u_p_p = g%v_p(i, j-0, k)/rho_p
!            c2_p  = Gamma*g%p(i, j-0, k)/rho_p
!            q_p = [rho_p, rho_p*u_r_p, rho_p*u_t_p, rho_p*u_p_p, rho_p*ent_p - g%p(i, j-0, k) ]
!            f_p = [rho_p*u_t_p, rho_p*u_r_p*u_t_p, rho_p*u_t_p*u_t_p+g%p(i, j-0, k), rho_p*u_t_p*u_p_p, rho_p*u_t_p*ent_p]
!            ! > Calculate parameters at -Theta-
!            rho_m = g%rho(i, j-1, k)
!            ent_m = ( g%nrg(i, j-1, k) + g%p(i, j-1, k) )/rho_m
!            u_r_m = g%v_r(i, j-1, k)/rho_m
!            u_t_m = g%v_t(i, j-1, k)/rho_m
!            u_p_m = g%v_p(i, j-1, k)/rho_m
!            c2_m  = Gamma*g%p(i, j-1, k)/rho_m
!            q_m = [rho_m, rho_m*u_r_m, rho_m*u_t_m, rho_m*u_p_m, rho_m*ent_m - g%p(i, j-1, k) ]
!            f_m = [rho_m*u_t_m, rho_m*u_r_m*u_t_m, rho_m*u_t_m*u_t_m+g%p(i, j-1, k), rho_m*u_t_m*u_p_m, rho_m*u_t_m*ent_m]
!            ! > Calculate -Theta*
!            b = Gamma-1.0D0
!            rho_s = Sqrt(rho_p*rho_m)
!            ent_s = ( Sqrt(rho_p)*ent_p + Sqrt(rho_m)*ent_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_r_s = ( Sqrt(rho_p)*u_r_p + Sqrt(rho_m)*u_r_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_t_s = ( Sqrt(rho_p)*u_t_p + Sqrt(rho_m)*u_t_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_p_s = ( Sqrt(rho_p)*u_p_p + Sqrt(rho_m)*u_p_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            c2_s = ( Sqrt(rho_p)*c2_p + Sqrt(rho_m)*c2_m )/( Sqrt(rho_p) + Sqrt(rho_m) ) &
!            + b*rho_s/(rho_m + 2*rho_s + rho_p)*( (u_r_p-u_r_m)**2 + (u_t_p-u_t_m)**2 + (u_p_p-u_p_m)**2 )
!            c_s = Sqrt(c2_s)
!            ! > Calculate flux matrix.
!            q = u_r_s**2 + u_t_s**2 + u_p_s**2
!            gg = 0.5D0*q**2
!            !SR(1,:) = [1.0D0,     0.0D0,             0.0D0, 1.0D0,        1.0D0          ]
!            !SR(2,:) = [u_r_s,     1.0D0,             0.0D0, u_r_s,        u_r_s          ]
!            !SR(3,:) = [u_t_s-c_s, 0.0D0,             0.0D0, u_t_s,        u_t_s+c_s      ]
!            !SR(4,:) = [u_p_s,     0.0D0,             1.0D0, u_p_s,        u_p_s          ]
!            !SR(5,:) = [u_r_s,     ent_s - u_t_s*c_s, u_p_s, ent_s-c2_s/b, ent_s+u_t_s*c_s]
!            SD(:,:) = 0.0D0
!            SD(1,1) = u_t_s-c_s
!            SD(2,2) = u_t_s
!            SD(3,3) = u_t_s
!            SD(4,4) = u_t_s
!            SD(5,5) = u_t_s+c_s
!            SD = Abs(SD)
!            !SL(1,:) = [gg+u_r_s*c_s,           -u_r_s-c_s/b, -u_t_s,       -u_p_s,       1.0D0] 
!            !SL(2,:) = [-2.0D0*u_t_s*c2_s/b,    0.0D0,        2.0D0*c2_s/b, 0.0D0,        0.0D0]
!            !SL(3,:) = [-2.0D0*u_p_s*c2_s/b,    0.0D0,        0.0D0,        2.0D0*c2_s/b, 0.0D0]
!            !SL(4,:) = [2.0D0*ent_s-2.0D0*q**2, 2.0D0*u_r_s,  2.0D0*u_t_s,  2.0D0*u_p_s,  -2.0D0]
!            !SL(5,:) = [gg-u_r_s*c_s,           -u_r_s+c_s/b, -u_t_s,       -u_p_s,       1.0D0]
!            SL = SL*b/(2.0D0*c2_s)
!            !f_s = 0.5D0*(f_p+f_m) - 0.5D0*MatMul(SR, MatMul(SD, MatMul(SL, q_p-q_m)))
!            f_s = 0.5D0*(f_p+f_m) - 0.5D0*MaxVal(Abs(SD))*(q_p-q_m) 
!            flux_rho_jmh = f_s(1)
!            flux_v_r_jmh = f_s(2)
!            flux_v_t_jmh = f_s(3)
!            flux_v_p_jmh = f_s(4)
!            flux_nrg_jmh = f_s(5)
!            
!            ! >----------------------------------------------------------------------------
!            ! > Calculate Fluxes at +Phi.
!            ! >----------------------------------------------------------------------------  
!            ! > Calculate parameters at +Phi+
!            rho_p = g%rho(i, j, k+1)
!            ent_p = ( g%nrg(i, j, k+1) + g%p(i, j, k+1) )/rho_p
!            u_r_p = g%v_r(i, j, k+1)/rho_p
!            u_t_p = g%v_t(i, j, k+1)/rho_p
!            u_p_p = g%v_p(i, j, k+1)/rho_p
!            c2_p  = Gamma*g%p(i, j, k+1)/rho_p
!            q_p = [rho_p, rho_p*u_r_p, rho_p*u_t_p, rho_p*u_p_p, rho_p*ent_p - g%p(i, j, k+1) ]
!            f_p = [rho_p*u_p_p, rho_p*u_r_p*u_p_p, rho_p*u_t_p*u_p_p, rho_p*u_p_p*u_p_p+g%p(i, j, k+1), rho_p*u_p_p*ent_p]
!            ! > Calculate parameters at +Phi-
!            rho_m = g%rho(i, j, k+0)
!            ent_m = ( g%nrg(i, j, k+0) + g%p(i, j, k+0) )/rho_m
!            u_r_m = g%v_r(i, j, k+0)/rho_m
!            u_t_m = g%v_t(i, j, k+0)/rho_m
!            u_p_m = g%v_p(i, j, k+0)/rho_m
!            c2_m  = Gamma*g%p(i, j, k+0)/rho_m
!            q_m = [rho_m, rho_m*u_r_m, rho_m*u_t_m, rho_m*u_p_m, rho_m*ent_m - g%p(i, j, k+0) ]
!            f_m = [rho_m*u_t_m, rho_m*u_r_m*u_p_m, rho_m*u_t_m*u_p_m, rho_m*u_p_m*u_p_m+g%p(i, j, k+0), rho_m*u_p_m*ent_m]
!            ! > Calculate +Phi*
!            b = Gamma-1.0D0
!            rho_s = Sqrt(rho_p*rho_m)
!            ent_s = ( Sqrt(rho_p)*ent_p + Sqrt(rho_m)*ent_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_r_s = ( Sqrt(rho_p)*u_r_p + Sqrt(rho_m)*u_r_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_t_s = ( Sqrt(rho_p)*u_t_p + Sqrt(rho_m)*u_t_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_p_s = ( Sqrt(rho_p)*u_p_p + Sqrt(rho_m)*u_p_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            c2_s = ( Sqrt(rho_p)*c2_p + Sqrt(rho_m)*c2_m )/( Sqrt(rho_p) + Sqrt(rho_m) ) &
!            + b*rho_s/(rho_m + 2*rho_s + rho_p)*( (u_r_p-u_r_m)**2 + (u_t_p-u_t_m)**2 + (u_p_p-u_p_m)**2 )
!            c_s = Sqrt(c2_s)
!            ! > Calculate flux matrix.
!            q = u_r_s**2 + u_t_s**2 + u_p_s**2
!            gg = 0.5D0*q**2
!            !SR(1,:) = [1.0D0,     0.0D0,             0.0D0, 1.0D0,        1.0D0          ]
!            !SR(2,:) = [u_r_s,     1.0D0,             0.0D0, u_r_s,        u_r_s          ]
!            !SR(3,:) = [u_t_s-c_s, 0.0D0,             0.0D0, u_t_s,        u_t_s+c_s      ]
!            !SR(4,:) = [u_p_s,     0.0D0,             1.0D0, u_p_s,        u_p_s          ]
!            !SR(5,:) = [u_r_s,     ent_s - u_t_s*c_s, u_p_s, ent_s-c2_s/b, ent_s+u_t_s*c_s]
!            SD(:,:) = 0.0D0
!            SD(1,1) = u_p_s-c_s
!            SD(2,2) = u_p_s
!            SD(3,3) = u_p_s
!            SD(4,4) = u_p_s
!            SD(5,5) = u_p_s+c_s
!            SD = Abs(SD)
!            !SL(1,:) = [gg+u_r_s*c_s,           -u_r_s-c_s/b, -u_t_s,       -u_p_s,       1.0D0] 
!            !SL(2,:) = [-2.0D0*u_t_s*c2_s/b,    0.0D0,        2.0D0*c2_s/b, 0.0D0,        0.0D0]
!            !SL(3,:) = [-2.0D0*u_p_s*c2_s/b,    0.0D0,        0.0D0,        2.0D0*c2_s/b, 0.0D0]
!            !SL(4,:) = [2.0D0*ent_s-2.0D0*q**2, 2.0D0*u_r_s,  2.0D0*u_t_s,  2.0D0*u_p_s,  -2.0D0]
!            !SL(5,:) = [gg-u_r_s*c_s,           -u_r_s+c_s/b, -u_t_s,       -u_p_s,       1.0D0]
!            SL = SL*b/(2.0D0*c2_s)
!            !f_s = 0.5D0*(f_p+f_m) - 0.5D0*MatMul(SR, MatMul(SD, MatMul(SL, q_p-q_m)))
!            f_s = 0.5D0*(f_p+f_m) - 0.5D0*MaxVal(Abs(SD))*(q_p-q_m) 
!            flux_rho_kph = f_s(1)
!            flux_v_r_kph = f_s(2)
!            flux_v_t_kph = f_s(3)
!            flux_v_p_kph = f_s(4)
!            flux_nrg_kph = f_s(5)
!            
!            ! >----------------------------------------------------------------------------
!            ! > Calculate Fluxes at -Phi.
!            ! >----------------------------------------------------------------------------  
!            ! > Calculate parameters at -Phi+
!            rho_p = g%rho(i, j, k-0)
!            ent_p = ( g%nrg(i, j, k-0) + g%p(i, j, k-0) )/rho_p
!            u_r_p = g%v_r(i, j, k-0)/rho_p
!            u_t_p = g%v_t(i, j, k-0)/rho_p
!            u_p_p = g%v_p(i, j, k-0)/rho_p
!            c2_p  = Gamma*g%p(i, j, k-0)/rho_p
!            q_p = [rho_p, rho_p*u_r_p, rho_p*u_t_p, rho_p*u_p_p, rho_p*ent_p - g%p(i, j, k-0) ]
!            f_p = [rho_p*u_p_p, rho_p*u_r_p*u_p_p, rho_p*u_t_p*u_p_p, rho_p*u_p_p*u_p_p+g%p(i, j, k-0), rho_p*u_p_p*ent_p]
!            ! > Calculate parameters at -Phi-
!            rho_m = g%rho(i, j, k-1)
!            ent_m = ( g%nrg(i, j, k-1) + g%p(i, j, k-1) )/rho_m
!            u_r_m = g%v_r(i, j, k-1)/rho_m
!            u_t_m = g%v_t(i, j, k-1)/rho_m
!            u_p_m = g%v_p(i, j, k-1)/rho_m
!            c2_m  = Gamma*g%p(i, j, k-1)/rho_m
!            q_m = [rho_m, rho_m*u_r_m, rho_m*u_t_m, rho_m*u_p_m, rho_m*ent_m - g%p(i, j, k-1) ]
!            f_m = [rho_m*u_t_m, rho_m*u_r_m*u_p_m, rho_m*u_t_m*u_p_m, rho_m*u_p_m*u_p_m+g%p(i, j, k-1), rho_m*u_p_m*ent_m]
!            ! > Calculate -Phi*
!            b = Gamma-1.0D0
!            rho_s = Sqrt(rho_p*rho_m)
!            ent_s = ( Sqrt(rho_p)*ent_p + Sqrt(rho_m)*ent_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_r_s = ( Sqrt(rho_p)*u_r_p + Sqrt(rho_m)*u_r_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_t_s = ( Sqrt(rho_p)*u_t_p + Sqrt(rho_m)*u_t_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            u_p_s = ( Sqrt(rho_p)*u_p_p + Sqrt(rho_m)*u_p_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
!            c2_s = ( Sqrt(rho_p)*c2_p + Sqrt(rho_m)*c2_m )/( Sqrt(rho_p) + Sqrt(rho_m) ) &
!            + b*rho_s/(rho_m + 2*rho_s + rho_p)*( (u_r_p-u_r_m)**2 + (u_t_p-u_t_m)**2 + (u_p_p-u_p_m)**2 )
!            c_s = Sqrt(c2_s)
!            ! > Calculate flux matrix.
!            q = u_r_s**2 + u_t_s**2 + u_p_s**2
!            gg = 0.5D0*q**2
!            !SR(1,:) = [1.0D0,     0.0D0,             0.0D0, 1.0D0,        1.0D0          ]
!            !SR(2,:) = [u_r_s,     1.0D0,             0.0D0, u_r_s,        u_r_s          ]
!            !SR(3,:) = [u_t_s-c_s, 0.0D0,             0.0D0, u_t_s,        u_t_s+c_s      ]
!            !SR(4,:) = [u_p_s,     0.0D0,             1.0D0, u_p_s,        u_p_s          ]
!            !SR(5,:) = [u_r_s,     ent_s - u_t_s*c_s, u_p_s, ent_s-c2_s/b, ent_s+u_t_s*c_s]
!            SD(:,:) = 0.0D0
!            SD(1,1) = u_p_s-c_s
!            SD(2,2) = u_p_s
!            SD(3,3) = u_p_s
!            SD(4,4) = u_p_s
!            SD(5,5) = u_p_s+c_s
!            SD = Abs(SD)
!            !SL(1,:) = [gg+u_r_s*c_s,           -u_r_s-c_s/b, -u_t_s,       -u_p_s,       1.0D0] 
!            !SL(2,:) = [-2.0D0*u_t_s*c2_s/b,    0.0D0,        2.0D0*c2_s/b, 0.0D0,        0.0D0]
!            !SL(3,:) = [-2.0D0*u_p_s*c2_s/b,    0.0D0,        0.0D0,        2.0D0*c2_s/b, 0.0D0]
!            !SL(4,:) = [2.0D0*ent_s-2.0D0*q**2, 2.0D0*u_r_s,  2.0D0*u_t_s,  2.0D0*u_p_s,  -2.0D0]
!            !SL(5,:) = [gg-u_r_s*c_s,           -u_r_s+c_s/b, -u_t_s,       -u_p_s,       1.0D0]
!            SL = SL*b/(2.0D0*c2_s)
!            !f_s = 0.5D0*(f_p+f_m) - 0.5D0*MatMul(SR, MatMul(SD, MatMul(SL, q_p-q_m)))
!            f_s = 0.5D0*(f_p+f_m) - 0.5D0*MaxVal(Abs(SD))*(q_p-q_m) 
!            flux_rho_kmh = f_s(1)
!            flux_v_r_kmh = f_s(2)
!            flux_v_t_kmh = f_s(3)
!            flux_v_p_kmh = f_s(4)
!            flux_nrg_kmh = f_s(5)
!            
!#if MHD_DEBUG
!            ! >----------------------------------------------------------------------------
!            ! > Verify the Flux coefficients.
!            ! >----------------------------------------------------------------------------
!            If ( IsNan(flux_rho_iph + flux_rho_jph + flux_rho_kph + &
!                       flux_rho_imh + flux_rho_jmh + flux_rho_kmh) ) Then
!                Write (0,*) 'NaN density flux was detected.'
!                Stop
!            End If
!            If ( IsNan(flux_nrg_iph + flux_nrg_jph + flux_nrg_kph + &
!                       flux_nrg_imh + flux_nrg_jmh + flux_nrg_kmh) ) Then
!                Write (0,*) 'NaN energy flux was detected.'
!                Stop
!            End If
!            If ( IsNan(flux_v_r_iph + flux_v_r_jph + flux_v_r_kph + &
!                       flux_v_r_imh + flux_v_r_jmh + flux_v_r_kmh) ) Then
!                Write (0,*) 'NaN impulse flux was detected.'
!                Stop
!            End If
!            If ( IsNan(flux_v_t_iph + flux_v_t_jph + flux_v_t_kph + &
!                       flux_v_t_imh + flux_v_t_jmh + flux_v_t_kmh) ) Then
!                Write (0,*) 'NaN impulse flux was detected.'
!                Stop
!            End If
!            If ( IsNan(flux_v_p_iph + flux_v_p_jph + flux_v_p_kph + &
!                       flux_v_p_imh + flux_v_p_jmh + flux_v_p_kmh) ) Then
!                Write (0,*) 'NaN impulse flux was detected.'
!                Stop
!            End If
!#endif
!            
!            ! >----------------------------------------------------------------------------
!            ! > Calculate the Flux coefficients.
!            ! >----------------------------------------------------------------------------
!            coef_iph = ( r_iph**2 ) * 0.5D0*( Sin(Theta_jph)+Sin(Theta_jmh) )
!            coef_imp = ( r_imh**2 ) * 0.5D0*( Sin(Theta_jph)+Sin(Theta_jmh) )
!            coef_jph = ( r_i ) * Sin(Theta_jph)
!            coef_jmh = ( r_i ) * Sin(Theta_jmh)
!            coef_kph = ( r_i )
!            coef_kmh = ( r_i )
!            
!            ! >----------------------------------------------------------------------------
!            ! > Calculate the Basis function values.
!            ! >----------------------------------------------------------------------------
!            Psi_m = 1.0D0/(h_r*h_tt*h_p*Sin(Theta_j)*(r_i**2+(h_r**2)/12.0D0))
!            Psi_m_iph = Psi_m; Psi_m_jph = Psi_m; Psi_m_kph = Psi_m
!            Psi_m_imh = Psi_m; Psi_m_jmh = Psi_m; Psi_m_kmh = Psi_m
!            
!            ! >----------------------------------------------------------------------------
!            ! > Update the Density field.
!            ! >----------------------------------------------------------------------------
!            gp%rho(i, j, k) = 0.0D0
!            gp%rho(i, j, k) = gp%rho(i, j, k) + h_t*h_p*( &
!                                coef_iph*Psi_m_iph*( flux_rho_iph ) - &
!                                coef_imp*Psi_m_imh*( flux_rho_imh ) )
!            gp%rho(i, j, k) = gp%rho(i, j, k) + h_r*h_p*( &
!                                coef_jph*Psi_m_jph*( flux_rho_jph ) - &
!                                coef_jmh*Psi_m_jmh*( flux_rho_jmh ) )
!            gp%rho(i, j, k) = gp%rho(i, j, k) + h_r*h_t*( &
!                                coef_kph*Psi_m_kph*( flux_rho_kph ) - &
!                                coef_kmh*Psi_m_kph*( flux_rho_kmh ) )
!            gp%rho(i, j, k) = g%rho(i, j, k) - Tau*gp%rho(i, j, k)
!            If ( (gp%rho(i, j, k) < 0.0D0) .OR. IsNan(gp%rho(i, j, k)) ) Then
!                Write (0,*) 'Negative or NaN density was detected: ', gp%rho(i, j, k), ' at: ', i, j, k
!                Stop
!            End If
!            
!            ! >----------------------------------------------------------------------------
!            ! > Update the Energy field.
!            ! >----------------------------------------------------------------------------
!            gp%nrg(i, j, k) = 0.0D0
!            gp%nrg(i, j, k) = gp%nrg(i, j, k) + h_t*h_p*( &
!                                coef_iph*Psi_m_iph*( flux_nrg_iph ) - &
!                                coef_imp*Psi_m_imh*( flux_nrg_imh ) )
!            gp%nrg(i, j, k) = gp%nrg(i, j, k) + h_r*h_p*( &
!                                coef_jph*Psi_m_jph*( flux_nrg_jph ) - &
!                                coef_jmh*Psi_m_jmh*( flux_nrg_jmh ) )
!            gp%nrg(i, j, k) = gp%nrg(i, j, k) + h_r*h_t*( &
!                                coef_kph*Psi_m_kph*( flux_nrg_kph ) - &
!                                coef_kmh*Psi_m_kph*( flux_nrg_kmh ) )
!            gp%nrg(i, j, k) = g%nrg(i, j, k) - Tau*gp%nrg(i, j, k)
!            If ( (gp%nrg(i, j, k) < 0.0D0) .OR. IsNan(gp%nrg(i, j, k)) ) Then
!                Write (0,*) 'Negative or NaN energy was detected: ', gp%nrg(i, j, k), ' at: ', i, j, k
!                Stop
!            End If
!            
!            ! >----------------------------------------------------------------------------
!            ! > Update the Impulse fields.
!            ! >----------------------------------------------------------------------------
!            ! > Update the radial component.
!            gp%v_r(i, j, k) = 0.0D0
!            gp%v_r(i, j, k) = gp%v_r(i, j, k) + h_t*h_p*( &
!                                coef_iph*Psi_m_iph*( flux_v_r_iph ) - &
!                                coef_imp*Psi_m_imh*( flux_v_r_imh ) )
!            gp%v_r(i, j, k) = gp%v_r(i, j, k) + h_r*h_p*( &
!                                coef_jph*Psi_m_jph*( flux_v_r_jph - 0.5D0*h_t*flux_v_t_jph ) - &
!                                coef_jmh*Psi_m_jmh*( flux_v_r_jmh + 0.5D0*h_t*flux_v_t_jmh ) )
!            gp%v_r(i, j, k) = gp%v_r(i, j, k) + h_r*h_t*( &
!                                coef_kph*Psi_m_kph*( flux_v_r_kph - 0.5D0*h_p*flux_v_p_kph* &
!                                                                    0.5D0*( Sin(Theta_jph) + Sin(Theta_jmh) ) ) - &
!                                coef_kmh*Psi_m_kmh*( flux_v_r_kmh + 0.5D0*h_p*flux_v_p_kmh* &
!                                                                    0.5D0*( Sin(Theta_jph) + Sin(Theta_jmh) ) ) )
!            gp%v_r(i, j, k) = g%v_r(i, j, k) - Tau*gp%v_r(i, j, k)
!            If ( IsNan(gp%v_r(i, j, k)) ) Then
!                Write (0,*) 'NaN radial impulse component was detected: ', gp%v_r(i, j, k), ' at: ', i, j, k
!                Stop
!            End If
!            ! >----------------------------------------------------------------------------
!            ! > Update the azimuthal component.
!            gp%v_t(i, j, k) = 0.0D0
!            gp%v_t(i, j, k) = gp%v_t(i, j, k) + h_t*h_p*( &
!                                coef_iph*Psi_m_iph*( flux_v_t_iph ) - &
!                                coef_imp*Psi_m_imh*( flux_v_t_imh ) )
!            gp%v_t(i, j, k) = gp%v_t(i, j, k) + h_r*h_p*( &
!                                coef_jph*Psi_m_jph*( flux_v_t_jph + 0.5D0*h_t*flux_v_r_jph ) - &
!                                coef_jmh*Psi_m_jmh*( flux_v_t_jmh - 0.5D0*h_t*flux_v_r_jmh ) )
!            gp%v_t(i, j, k) = gp%v_t(i, j, k) + h_r*h_tt*( &
!                                coef_kph*Psi_m_kph*( flux_v_t_kph - 0.5D0*h_p*flux_v_p_kph*Cos(Theta_j) ) - &
!                                coef_kmh*Psi_m_kph*( flux_v_t_kmh + 0.5D0*h_p*flux_v_p_kmh*Cos(Theta_j) ) )
!            gp%v_t(i, j, k) = g%v_t(i, j, k) - Tau*gp%v_t(i, j, k)
!            If ( IsNan(gp%v_t(i, j, k)) ) Then
!                Write (0,*) 'NaN azimuthal impulse component was detected: ', gp%v_t(i, j, k), ' at: ', i, j, k
!                Stop
!            End If
!            ! >----------------------------------------------------------------------------
!            ! > Update the polar component.
!            gp%v_p(i, j, k) = 0.0D0
!            gp%v_p(i, j, k) = gp%v_p(i, j, k) + h_t*h_p*( &
!                                coef_iph*Psi_m_iph*( flux_v_p_iph ) - & 
!                                coef_iph*Psi_m_imh*( flux_v_p_imh ) )
!            gp%v_p(i, j, k) = gp%v_p(i, j, k) + h_r*h_p*( &
!                                coef_jph*Psi_m_jph*( flux_v_p_jph ) - &
!                                coef_jmh*Psi_m_jmh*( flux_v_p_jmh ) )*r_i
!            gp%v_p(i, j, k) = gp%v_p(i, j, k) + h_r*h_t*( &
!                                coef_kph*Psi_m_kph*( flux_v_p_kph + 0.5D0*h_p*flux_v_r_kph*0.5D0*( Sin(Theta_jph)+Sin(Theta_jmh) ) &
!                                                                  - 0.5D0*h_p*flux_v_t_kph*Cos(Theta_j) ) - &
!                                coef_kmh*Psi_m_kph*( flux_v_p_kmh - 0.5D0*h_p*flux_v_r_kmh*0.5D0*( Sin(Theta_jph)+Sin(Theta_jmh) ) &
!                                                                  + 0.5D0*h_p*flux_v_t_kmh*Cos(Theta_j) ) )
!            gp%v_p(i, j, k) = g%v_p(i, j, k) - Tau*gp%v_p(i, j, k)
!            If ( IsNan(gp%v_p(i, j, k)) ) Then
!                Write (0,*) 'NaN polar impulse component was detected: ', gp%v_p(i, j, k), ' at: ', i, j, k
!                Stop
!            End If
!            
!            ! >----------------------------------------------------------------------------
!            ! > Update the Pressure field.
!            ! >----------------------------------------------------------------------------
!            gp%p(i, j, k) = ( Gamma - 1.0D0 )*( gp%nrg(i, j, k) - &
!                            ( gp%v_r(i, j, k)**2 + gp%v_t(i, j, k)**2 + gp%v_p(i, j, k)**2 )/( 2.0D0*gp%rho(i, j, k)) )
!            If ( (gp%p(i, j, k) < 0.0D0) .OR. IsNan(gp%p(i, j, k)) ) Then
!                Write (0,*) 'Negative or NaN pressure was detected: ', gp%p(i, j, k), ' at: ', i, j, k
!                Stop
!            End If
!                    
!        End Do
!        End Do
!        End Do
!        !$OMP End Parallel Do
!    End Subroutine simulate_euler_fdm
!    
!    Subroutine print_grid(g, l)
!        Type(mhd_grid), Intent(In) :: g
!        Integer, Intent(In) :: l
!        Integer :: i, j, k
!        Integer :: output
!        Real(8) :: r, theta, phi
!        Real(8) :: x, y, z
!        Real(8) :: v_r, v_t, v_p
!        Real(8) :: v_x, v_y, v_z, v_l
!        j = N_theta/2
!        Open(NewUnit=output, file='../Results/fields-'//Trim(to_str(l))//'.dat', Status='replace')
!        Do k = 1, N_phi
!            Do i = 1, N_r
!                r = r_0 + (i - 0.5D0)*h_r
!                Theta = (j - 0.5D0)*h_t
!                Phi = (k - 0.5D0)*h_p
!                x = r*Sin(Theta)*Cos(Phi)
!                y = r*Sin(Theta)*Sin(Phi)
!                z = r*Cos(Theta)
!                v_r = g%v_r(i, j, k)/g%rho(i, j, k)
!                v_t = g%v_t(i, j, k)/g%rho(i, j, k)
!                v_p = g%v_p(i, j, k)/g%rho(i, j, k)
!                v_x = Sin(Theta)*Cos(Phi)*v_r + Sin(Theta)*Sin(Phi)*v_t + Cos(Theta)*v_p
!                v_y = Cos(Theta)*Cos(Phi)*v_r + Cos(Theta)*Sin(Phi)*v_t - Sin(Theta)*v_p
!                v_z = -Sin(Phi)*v_r + Cos(Phi)*v_t
!                v_l = Sqrt(v_x**2 + v_y**2 + v_z**2)
!                If (v_l == 0.0) Then
!                    v_l = 1.0
!                End If
!                Write (output, '(40e,40e,40e,40e,40e,40e,40e)') &
!                    x, y, v_x/v_l, v_y/v_l, g%rho(i, j, k), g%p(i, j, k), g%nrg(i, j, k)
!            End Do
!        End Do
!        Close(output)
!    End Subroutine print_grid
    
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
        Open(NewUnit=output, file='../Results/fields-'//Trim(to_str(l))//'.dat', Status='replace')
        If ( dim==2 ) Then
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
                Write (output, '(40e,40e,40e,40e,40e,40e,40e)') &
                    x, y, &!r, phi,&!
                    vxy(1), vxy(2), &
                    g%rho(i, j, 1, 0), g%nrg(i, j, 1, 0)/g%rho(i, j, 1, 0)
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
    
End Module mhd_solver_simulation
    
Program mhd_solver
    Use mhd_solver_simulation
    Use mhd_solver_hydro
    
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
End Program mhd_solver
