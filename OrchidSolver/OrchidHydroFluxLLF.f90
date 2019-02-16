!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_flux_godunov
Use orchid_solver_params
Use orchid_solver_grid
Implicit None
Type, Abstract :: MhdHydroFlux
    Contains
    Procedure, Public, Non_Overridable :: calc => mhd_hydro_calc_flux
    Procedure(mhd_hydro_calc_flux1D_t), Public, Deferred :: calc1D
    Procedure(mhd_hydro_calc_flux2D_t), Public, Deferred :: calc2D
    Procedure(mhd_hydro_calc_flux3D_t), Public, Deferred :: calc3D
    Procedure(mhd_hydro_calc_flux3D_mhd_t), Public, Deferred :: calc3D_mhd
End Type MhdHydroFlux
Private :: mhd_hydro_calc_flux, &
           mhd_hydro_calc_flux1D_t, &
           mhd_hydro_calc_flux2D_t, &
           mhd_hydro_calc_flux3D_t
Interface
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux1D_t(This, &
                                   nx, &
                                   rho_p, nrg_p, u_p, &
                                   rho_m, nrg_m, u_m, &
                                   flux_rho, flux_nrg, flux_u)
    !> Calculate the Godunov Fluxes in 1D for the Euler Equations.
    !> {{{
    Import :: MhdHydroFlux
    Class(MhdHydroFlux), Intent(In) :: This
    Real(8), Intent(In) :: nx
    Real(8), Intent(In) :: rho_p, nrg_p, u_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
End Subroutine mhd_hydro_calc_flux1D_t
Pure &
Subroutine mhd_hydro_calc_flux2D_t(This, &
                                   nx, ny, &
                                   rho_p, nrg_p, u_p, v_p, &
                                   rho_m, nrg_m, u_m, v_m, &
                                   flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the Godunov Fluxes in 2D for the Euler Equations.
    !> {{{
    Import :: MhdHydroFlux
    Class(MhdHydroFlux), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v
    !> }}}
End Subroutine mhd_hydro_calc_flux2D_t
Pure &
Subroutine mhd_hydro_calc_flux3D_t(This, &
                                   nx, ny, nz, &
                                   rho_p, nrg_p, u_p, v_p, w_p, &
                                   rho_m, nrg_m, u_m, v_m, w_m, &
                                   flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the Godunov Fluxes in 3D for the Euler Equations.
    !> {{{
    Import :: MhdHydroFlux
    Class(MhdHydroFlux), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
End Subroutine mhd_hydro_calc_flux3D_t
Subroutine mhd_hydro_calc_flux3D_mhd_t(This, &
                                       nx, ny, nz, &
                                       rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p, &
                                       rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m, &
                                       flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz)
    !> Calculate the Godunov Fluxes in 3D for the MHD Equations.
    !> {{{
    Import :: MhdHydroFlux
    Class(MhdHydroFlux), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz
    !> }}}
End Subroutine mhd_hydro_calc_flux3D_mhd_t
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Interface
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux(This, &
                               fields_p, fields_m, flux, &
                               nx, ny, nz)
    !> Calculate the Godunov Fluxes in 1D/2D/3D.
    !> {{{
    Class(MhdHydroFlux), Intent(In) :: This
    Real(8), Dimension(1:), Intent(In) :: fields_p, fields_m
    Real(8), Dimension(1:), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Real(8) :: nl
    Real(8) :: rho_p, nrg_p, u_p, v_p, w_p, &
               rho_m, nrg_m, u_m, v_m, w_m
    rho_p = fields_p(1)    
    nrg_p = fields_p(2)/rho_p
    u_p   = fields_p(3)/rho_p
    rho_m = fields_m(1)    
    nrg_m = fields_m(2)/rho_m
    u_m   = fields_m(3)/rho_m
    !> @todo Add the MHD case.
    If ( dim == 1 ) Then
        !> 1D Case.
        nl = Abs(nx)
        Call This%calc1D(nx/nl, &
                         rho_p, nrg_p, u_p, &
                         rho_m, nrg_m, u_m, &
                         flux(1), flux(2), flux(3))
    Else
        v_p = fields_p(4)/rho_p
        v_m = fields_m(4)/rho_m
        If ( dim == 2 ) Then
            !> Polar Case.
            nl = Sqrt(nx**2 + ny**2)
            Call This%calc2D(nx/nl, ny/nl, &
                             rho_p, nrg_p, u_p, v_p, &
                             rho_m, nrg_m, u_m, v_m, &
                             flux(1), flux(2), flux(3), flux(4))
        Else
            !> Spherical Case.
            w_p = fields_p(5)/rho_p
            w_m = fields_m(5)/rho_m
            Call This%calc3D(nx, ny, nz, &
                             rho_p, nrg_p, u_p, v_p, w_p, &
                             rho_m, nrg_m, u_m, v_m, w_m, &
                             flux(1), flux(2), flux(3), flux(4), flux(5))
        End If
    End If
End Subroutine mhd_hydro_calc_flux
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_godunov



Module orchid_solver_hydro_flux_llf
Use orchid_solver_params
Use orchid_solver_hydro_flux_godunov
Implicit None
Type, Extends(MhdHydroFlux) :: MhdHydroFluxLLF
    Contains
    Procedure, Public, Non_Overridable :: calc1D => mhd_hydro_calc_flux_llf1D
    Procedure, Public, Non_Overridable :: calc2D => mhd_hydro_calc_flux_llf2D
    Procedure, Public, Non_Overridable :: calc3D => mhd_hydro_calc_flux_llf3D
    Procedure, Public, Non_Overridable :: calc3D_mhd => mhd_hydro_calc_flux_llf3D_mhd
End Type MhdHydroFluxLLF
Private :: mhd_hydro_calc_flux_llf1D, &
           mhd_hydro_calc_flux_llf2D, &
           mhd_hydro_calc_flux_llf3D, &
           mhd_hydro_calc_flux_llf3D_mhd
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_llf1D(This, &
                                     nx, &
                                     rho_p, nrg_p, u_p, &
                                     rho_m, nrg_m, u_m, &
                                     flux_rho, flux_nrg, flux_u)
    !> Calculate the LLF (Rusanov) Fluxes in 1D.
    !> {{{
    Class(MhdHydroFluxLLF), Intent(In) :: This
    Real(8), Intent(In) :: nx
    Real(8), Intent(In) :: rho_p, nrg_p, u_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m
    Real(8), Dimension(1:3) :: q_p, f_p, &
                               q_m, f_m, &
                               q_s, f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx
    q_p  = [ rho_p, rho_p*u_p, rho_p*nrg_p ]
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx
    q_m  = [ rho_m, rho_m*u_m, rho_m*nrg_m ]
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------

    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    q_s = 0.5D0*( q_p - q_m )
    q_s = Max(Abs(a_p - c_p), Abs(a_m - c_m), Abs(a_p), Abs(a_m), &
              Abs(a_p + c_p), Abs(a_m + c_m)) * q_s
    f_s = 0.5D0*( f_p + f_m ) - q_s
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_nrg = f_s(3)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_llf1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_llf2D(This, &
                                     nx, ny, &
                                     rho_p, nrg_p, u_p, v_p, &
                                     rho_m, nrg_m, u_m, v_m, &
                                     flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the LLF (Rusanov) Fluxes in 2D.
    !> {{{
    Class(MhdHydroFluxLLF), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m
    Real(8), Dimension(1:4) :: q_p, f_p, &
                               q_m, f_m, &
                               q_s, f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny
    q_p  = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*nrg_p ]
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny
    q_m  = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*nrg_m ]
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------

    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    q_s = 0.5D0*( q_p - q_m )
    q_s = Max(Abs(a_p - c_p), Abs(a_m - c_m), Abs(a_p), Abs(a_m), &
              Abs(a_p + c_p), Abs(a_m + c_m)) * q_s
    f_s = 0.5D0*( f_p + f_m ) - q_s
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_nrg = f_s(4)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_llf2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_llf3D(This, &
                                     nx, ny, nz, &
                                     rho_p, nrg_p, u_p, v_p, w_p, &
                                     rho_m, nrg_m, u_m, v_m, w_m, &
                                     flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the LLF (Rusanov) Fluxes in 3D.
    !> {{{
    Class(MhdHydroFluxLLF), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m
    Real(8), Dimension(1:5) :: q_p, f_p, &
                               q_m, f_m, &
                               q_s, f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 + w_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny + w_p*nz
    q_p  = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*w_p, rho_p*nrg_p ]
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, &
                        rho_p*w_p*a_p + p_p*nz, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 + w_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny + w_m*nz
    q_m  = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*w_m, rho_m*nrg_m ]
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, &
                        rho_m*w_m*a_m + p_m*nz, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------

    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    q_s = 0.5D0*( q_p - q_m )
    q_s = Max(Abs(a_p - c_p), Abs(a_m - c_m), Abs(a_p), Abs(a_m), &
              Abs(a_p + c_p), Abs(a_m + c_m)) * q_s
    f_s = 0.5D0*( f_p + f_m ) - q_s
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_w   = f_s(4)
    flux_nrg = f_s(5)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_llf3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_flux_llf3D_mhd(This, &
                                         nx, ny, nz, &
                                         rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p, &
                                         rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m, &
                                         flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz)
    !> Calculate the LLF (Rusanov) Fluxes in 3D for the MHD Equations.
    !> {{{
    Class(MhdHydroFluxLLF), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p, bx_p, by_p, bz_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m, bx_m, by_m, bz_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz
    !> }}}
    Error Stop 'Not implemented'
End Subroutine mhd_hydro_calc_flux_llf3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_llf


