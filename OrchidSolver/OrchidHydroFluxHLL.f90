!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_flux_hll
Use orchid_solver_params
Use orchid_solver_hydro_flux_godunov
Implicit None
Type, Extends(MhdHydroFlux) :: MhdHydroFluxHLL
    Contains
    Procedure, Public, Non_Overridable :: calc1D => mhd_hydro_calc_flux_hll1D
    Procedure, Public, Non_Overridable :: calc2D => mhd_hydro_calc_flux_hll2D
    Procedure, Public, Non_Overridable :: calc3D => mhd_hydro_calc_flux_hll3D
    Procedure, Public, Non_Overridable :: calc3D_mhd => mhd_hydro_calc_flux_hll3D_mhd
End Type MhdHydroFluxHLL
Private :: mhd_hydro_calc_flux_hll1D, &
           mhd_hydro_calc_flux_hll2D, &
           mhd_hydro_calc_flux_hll3D, &
           mhd_hydro_calc_flux_hll3D_mhd
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hll1D(This, &
                                     nx, &
                                     rho_p, nrg_p, u_p, &
                                     rho_m, nrg_m, u_m, &
                                     flux_rho, flux_nrg, flux_u)
    !> Calculate the HLL Fluxes in 1D.
    !> {{{
    Class(MhdHydroFluxHLL), Intent(In) :: This
    Real(8), Intent(In) :: nx
    Real(8), Intent(In) :: rho_p, nrg_p, u_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, s_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, s_m
    Real(8), Dimension(1:3) :: q_p, f_p, &
                               q_m, f_m, &
                               f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    s_p = Max(a_p + c_p, a_m + c_m) 
    s_m = Min(a_m - c_m, a_p - c_p)
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        q_p = [ rho_p, rho_p*u_p, rho_p*nrg_p ]
        q_m = [ rho_m, rho_m*u_m, rho_m*nrg_m ]
        f_s = ( s_p*f_m - s_m*f_p + s_p*s_m*( q_p - q_m ) )/( s_p - s_m  )
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_nrg = f_s(3)        
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hll1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hll2D(This, &
                                     nx, ny, &
                                     rho_p, nrg_p, u_p, v_p, &
                                     rho_m, nrg_m, u_m, v_m, &
                                     flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the HLL Fluxes in 2D.
    !> {{{ 
    Class(MhdHydroFluxHLL), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, s_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, s_m
    Real(8), Dimension(1:4) :: q_p, f_p, &
                               q_m, f_m, &
                               f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    s_p = Max(a_p + c_p, a_m + c_m) 
    s_m = Min(a_m - c_m, a_p - c_p)
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        q_p = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*nrg_p ]
        q_m = [ rho_m, rho_m*u_m, rho_p*v_m, rho_m*nrg_m ]
        f_s = ( s_p*f_m - s_m*f_p + s_p*s_m*( q_p - q_m ) )/( s_p - s_m  )
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_nrg = f_s(4)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hll2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hll3D(This, &
                                     nx, ny, nz, &
                                     rho_p, nrg_p, u_p, v_p, w_p, &
                                     rho_m, nrg_m, u_m, v_m, w_m, &
                                     flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the HLL Fluxes in 3D.
    !> {{{
    Class(MhdHydroFluxHLL), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, s_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, s_m
    Real(8), Dimension(1:5) :: q_p, f_p, &
                               q_m, f_m, &
                               f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 + w_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, &
                        rho_p*w_p*a_p + p_p*nz, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 + w_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny + w_m*nz
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, &
                        rho_m*w_m*a_m + p_m*nz, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    s_p = Max(a_p + c_p, a_m + c_m) 
    s_m = Min(a_m - c_m, a_p - c_p)
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        q_p = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*w_p, rho_p*nrg_p ]
        q_m = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*w_m, rho_m*nrg_m ]
        f_s = ( s_p*f_m - s_m*f_p + s_p*s_m*( q_p - q_m ) )/( s_p - s_m  )
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_w   = f_s(4)
    flux_nrg = f_s(5)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hll3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hll3D_mhd(This, &
                                         nx, ny, nz, &
                                         rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p, &
                                         rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m, &
                                         flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz)
    !> Calculate the HLL Fluxes in 3D for the MHD Equations.
    !> {{{
    Class(MhdHydroFluxHLL), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz
    !> }}}
    Real(8) :: e_p, p_p, pt_p, ent_p, a_p, b_p, cf_p, c2_p, ca2_p, ca2n_p, cf2_p, s_p, &
               e_m, p_m, pt_m, ent_m, a_m, b_m, cf_m, c2_m, ca2_m, ca2n_m, cf2_m, s_m
    Real(8), Dimension(1:8) :: q_p, f_p, &
                               q_m, f_m, &
                               q_s, f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 + w_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    pt_p = p_p + 0.125D0/Pi*( bx_p**2 + by_p**2 + bz_p**2 )
    ent_p = nrg_p + ( pt_p + 0.125D0/Pi*( bx_p**2 + by_p**2 + bz_p**2 ) )/rho_p
    a_p  = u_p*nx + v_p*ny + w_p*nz
    b_p  = bx_p*nx + by_p*ny + bz_p*nz
    c2_p = Gamma*p_p/rho_p
    ca2n_p = 0.25D0/Pi*b_p**2/rho_p
    ca2_p = 0.25D0/Pi*( bx_p**2 + by_p**2 + bz_p**2 )/rho_p
    cf2_p = 0.5D0*( c2_p + ca2_p ) + 0.5D0*Sqrt(( c2_p + ca2_p )**2 - 4.0D0*c2_p*ca2n_p )
    cf_p  = Sqrt(Max(cf2_p, 1D-10))
    f_p  = [ rho_p*a_p, &
             rho_p*u_p*a_p - 0.25D0/Pi*b_p*bx_p + pt_p*nx, &
             rho_p*v_p*a_p - 0.25D0/Pi*b_p*by_p + pt_p*ny, &
             rho_p*w_p*a_p - 0.25D0/Pi*b_p*by_p + pt_p*nz, &
             rho_p*a_p*ent_p - 0.25D0/Pi*b_p*( bx_p*u_p + by_p*v_p + bz_p*w_p ), &
             bx_p*a_p - b_p*u_p, &
             by_p*a_p - b_p*v_p, &
             bz_p*a_p - b_p*w_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 + w_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    pt_m  = p_m + 0.125D0/Pi*( bx_m**2 + by_m**2 + bz_m**2 )
    ent_m = nrg_m + ( pt_m + 0.125D0/Pi*( bx_m**2 + by_m**2 + bz_m**2 ) )/rho_m
    a_m  = u_m*nx + v_m*ny + w_m*nz
    b_m  = bx_m*nx + by_m*ny + bz_m*nz
    c2_m = Gamma*p_m/rho_m
    ca2n_m = 0.25D0/Pi*b_m**2/rho_m
    ca2_m = 0.25D0/Pi*( bx_m**2 + by_m**2 + bz_m**2 )/rho_m
    cf2_m = 0.5D0*( c2_m + ca2_m ) + 0.5D0*Sqrt(( c2_m + ca2_m )**2 - 4.0D0*c2_m*ca2n_m )
    cf_m  = Sqrt(Max(cf2_m, 1D-10))
    f_m  = [ rho_m*a_m, &
             rho_m*u_m*a_m - 0.25D0/Pi*b_m*bx_m + pt_m*nx, &
             rho_m*v_m*a_m - 0.25D0/Pi*b_m*by_m + pt_m*ny, &
             rho_m*w_m*a_m - 0.25D0/Pi*b_m*by_m + pt_m*nz, &
             rho_m*a_m*ent_m - 0.25D0/Pi*b_m*( bx_m*u_m + by_m*v_m + bz_m*w_m ), &
             bx_m*a_m - b_m*u_m, &
             by_m*a_m - b_m*v_m, &
             bz_m*a_m - b_m*w_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    s_p = Max(a_p, a_m) + Max(cf_p, cf_m) 
    s_m = Min(a_m, a_p) - Max(cf_p, cf_m)
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        q_p  = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*w_p, &
                 rho_p*nrg_p + 0.125D0/Pi*( bx_p**2 + by_p**2 + bz_p**2 ), bx_p, by_p, bz_p ]
        q_m  = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*w_m, &
                 rho_m*nrg_m + 0.125D0/Pi*( bx_m**2 + by_m**2 + bz_m**2 ), bx_m, by_m, bz_m ]
        f_s = ( s_p*f_m - s_m*f_p + s_p*s_m*( q_p - q_m ) )/( s_p - s_m  )
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_w   = f_s(4)
    flux_nrg = f_s(5)
    flux_bx  = f_s(6)
    flux_by  = f_s(7)
    flux_bz  = f_s(8)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hll3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_hll



Module orchid_solver_hydro_flux_hllc
Use orchid_solver_params
Use orchid_solver_hydro_flux_godunov
Implicit None
Type, Extends(MhdHydroFlux) :: MhdHydroFluxHLLC
    Contains
    Procedure, Public, Non_Overridable :: calc1D => mhd_hydro_calc_flux_hllc1D
    Procedure, Public, Non_Overridable :: calc2D => mhd_hydro_calc_flux_hllc2D
    Procedure, Public, Non_Overridable :: calc3D => mhd_hydro_calc_flux_hllc3D
    Procedure, Public :: calc3D_mhd => mhd_hydro_calc_flux_hllc3D_mhd
End Type MhdHydroFluxHLLC
Private :: mhd_hydro_calc_flux_hllc1D, &
           mhd_hydro_calc_flux_hllc2D, &
           mhd_hydro_calc_flux_hllc3D, &
           mhd_hydro_calc_flux_hllc3D_mhd
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc1D(This, &
                                      nx, &
                                      rho_p, nrg_p, u_p, &
                                      rho_m, nrg_m, u_m, &
                                      flux_rho, flux_nrg, flux_u)
    !> Calculate the HLLC Fluxes in 1D.
    !> {{{
    Class(MhdHydroFluxHLLC), Intent(In) :: This
    Real(8), Intent(In) :: nx
    Real(8), Intent(In) :: rho_p, nrg_p, u_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, g_p, s_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, g_m, s_m, &
               rho_s, p_s, c_s, s_s
    Real(8), Dimension(1:3) :: q_p, f_p, &
                               q_m, f_m, &
                               f_s, d_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Pressure-based Wave speeds.
    rho_s = 0.5D0*( rho_p + rho_m )
    c_s  = 0.5D0*( c_p + c_m )
    p_s  = Max(0.5D0*( p_p + p_m ) - 0.5D0*( a_p - a_m )*rho_s*c_s, 0.0D0)
    If ( p_s > p_p ) Then
        g_p = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_p - 1.0D0 )
        g_p = Sqrt(Max(g_p, 1D-10))
    Else
        g_p = 1.0D0
    End If
    If ( p_s > p_m ) Then
        g_m = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_m - 1.0D0 )
        g_m = Sqrt(Max(g_m, 1D-10))
    Else
        g_m = 1.0D0
    End If
    s_p = a_p + c_p*g_p
    s_m = a_m - c_m*g_m
    !> Calculate Fluxes.
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        s_s = ( ( rho_p*a_p*( s_p - a_p ) - p_p ) - ( rho_m*a_m*( s_m - a_m ) - p_m ) )/&
              ( rho_p*( s_p - a_p ) - rho_m*( s_m - a_m ) )
        p_s = ( (p_p + rho_p*( s_p - a_p )*( s_s - a_p ) ) +&
                (p_m + rho_m*( s_m - a_m )*( s_s - a_m ) ) )*0.5D0
        d_s = [ 0.0D0, nx, s_s ]
        If ( s_s <= 0.0D0 .AND. 0.0D0 <= s_p ) Then
            q_p = [ rho_p, rho_p*u_p, rho_p*nrg_p ]
            f_s = ( s_s*( s_p*q_p - f_p ) + s_p*p_s*d_s )/( s_p - s_s )
        Else If ( s_m <= 0.0D0 .AND. 0.0D0 <= s_s ) Then
            q_m = [ rho_m, rho_m*u_m, rho_m*nrg_m ]
            f_s = ( s_s*( s_m*q_m - f_m ) + s_m*p_s*d_s )/( s_m - s_s )
        End If
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_nrg = f_s(3)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hllc1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc2D(This, &
                                      nx, ny, &
                                      rho_p, nrg_p, u_p, v_p, &
                                      rho_m, nrg_m, u_m, v_m, &
                                      flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the HLLC Fluxes in 2D.
    !> {{{ 
    Class(MhdHydroFluxHLLC), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, g_p, s_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, g_m, s_m, &
               rho_s, p_s, c_s, s_s
    Real(8), Dimension(1:4) :: q_p, f_p, &
                               q_m, f_m, &
                               f_s, d_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Pressure-based Wave speeds.
    rho_s = 0.5D0*( rho_p + rho_m )
    c_s  = 0.5D0*( c_p + c_m )
    p_s  = Max(0.5D0*( p_p + p_m ) - 0.5D0*( a_p - a_m )*rho_s*c_s, 0.0D0)
    If ( p_s > p_p ) Then
        g_p = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_p - 1.0D0 )
        g_p = Sqrt(Max(g_p, 1D-10))
    Else
        g_p = 1.0D0
    End If
    If ( p_s > p_m ) Then
        g_m = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_m - 1.0D0 )
        g_m = Sqrt(Max(g_m, 1D-10))
    Else
        g_m = 1.0D0
    End If
    s_p = a_p + c_p*g_p
    s_m = a_m - c_m*g_m
    !> Calculate Fluxes.
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        s_s = ( ( rho_p*a_p*( s_p - a_p ) - p_p ) - ( rho_m*a_m*( s_m - a_m ) - p_m ) )/&
              ( rho_p*( s_p - a_p ) - rho_m*( s_m - a_m ) )
        p_s = ( (p_p + rho_p*( s_p - a_p )*( s_s - a_p ) ) +&
                (p_m + rho_m*( s_m - a_m )*( s_s - a_m ) ) )*0.5D0
        d_s = [ 0.0D0, nx, ny, s_s ]
        If ( s_s <= 0.0D0 .AND. 0.0D0 <= s_p ) Then
            q_p = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*nrg_p ]
            f_s = ( s_s*( s_p*q_p - f_p ) + s_p*p_s*d_s )/( s_p - s_s )
        Else If ( s_m <= 0.0D0 .AND. 0.0D0 <= s_s ) Then
            q_m = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*nrg_m ]
            f_s = ( s_s*( s_m*q_m - f_m ) + s_m*p_s*d_s )/( s_m - s_s )
        End If
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_nrg = f_s(4)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hllc2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc3D(This, &
                                      nx, ny, nz, &
                                      rho_p, nrg_p, u_p, v_p, w_p, &
                                      rho_m, nrg_m, u_m, v_m, w_m, &
                                      flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the HLLC Fluxes in 3D.
    !> {{{
    Class(MhdHydroFluxHLLC), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, g_p, s_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, g_m, s_m, &
               rho_s, p_s, c_s, s_s
    Real(8), Dimension(1:5) :: q_p, f_p, &
                               q_m, f_m, &
                               f_s, d_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 + w_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, &
                        rho_p*w_p*a_p + p_p*nz, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 + w_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny + w_m*nz
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, &
                        rho_m*w_m*a_m + p_m*nz, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Pressure-based Wave speeds.
    rho_s = 0.5D0*( rho_p + rho_m )
    c_s  = 0.5D0*( c_p + c_m )
    p_s  = Max(0.5D0*( p_p + p_m ) - 0.5D0*( a_p - a_m )*rho_s*c_s, 0.0D0)
    If ( p_s > p_p ) Then
        g_p = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_p - 1.0D0 )
        g_p = Sqrt(Max(g_p, 1D-10))
    Else
        g_p = 1.0D0
    End If
    If ( p_s > p_m ) Then
        g_m = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_m - 1.0D0 )
        g_m = Sqrt(Max(g_m, 1D-10))
    Else
        g_m = 1.0D0
    End If
    s_p = a_p + c_p*g_p
    s_m = a_m - c_m*g_m
    !> Calculate Fluxes.
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        s_s = ( ( rho_p*a_p*( s_p - a_p ) - p_p ) - ( rho_m*a_m*( s_m - a_m ) - p_m ) )/&
              ( rho_p*( s_p - a_p ) - rho_m*( s_m - a_m ) )
        p_s = ( (p_p + rho_p*( s_p - a_p )*( s_s - a_p ) ) +&
                (p_m + rho_m*( s_m - a_m )*( s_s - a_m ) ) )*0.5D0
        d_s = [ 0.0D0, nx, ny, nz, s_s ]
        If ( s_s <= 0.0D0 .AND. 0.0D0 <= s_p ) Then
            q_p = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*w_p, rho_p*nrg_p ]
            f_s = ( s_s*( s_p*q_p - f_p ) + s_p*p_s*d_s )/( s_p - s_s )
        Else If ( s_m <= 0.0D0 .AND. 0.0D0 <= s_s ) Then
            q_m = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*w_m, rho_m*nrg_m ]
            f_s = ( s_s*( s_m*q_m - f_m ) + s_m*p_s*d_s )/( s_m - s_s )
        End If
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_w   = f_s(4)
    flux_nrg = f_s(5)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hllc3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc3D_mhd(This, &
                                          nx, ny, nz, &
                                          rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p, &
                                          rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m, &
                                          flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz)
    !> Calculate the HLLC Fluxes in 3D for the MHD Equations.
    !> {{{
    Class(MhdHydroFluxHLLC), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz
    !> }}}
    Error Stop 'Not implemented'
End Subroutine mhd_hydro_calc_flux_hllc3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_hllc



Module orchid_solver_hydro_flux_hlld
Use orchid_solver_params
Use orchid_solver_hydro_flux_hllc
Implicit None
Type, Extends(MhdHydroFluxHLLC) ::  MhdHydroFluxHLLD
    Contains
    Procedure, Public, Non_Overridable :: calc3D_mhd => mhd_hydro_calc_flux_hlld3D_mhd
End Type MhdHydroFluxHLLD
Private :: mhd_hydro_calc_flux_hlld3D_mhd
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hlld3D_mhd(This, &
                                          nx, ny, nz, &
                                          rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p, &
                                          rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m, &
                                          flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz)
    !> Calculate the HLLD Fluxes in 3D for the MHD Equations.
    !> {{{
    Class(MhdHydroFluxHLLD), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz
    !> }}}
    Error Stop 'Not implemented'
End Subroutine mhd_hydro_calc_flux_hlld3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_hlld


