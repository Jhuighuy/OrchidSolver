!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_flux_viscous
Use orchid_solver_params
Use orchid_solver_grid
Implicit None
Type, Abstract :: MhdHydroViscousFlux
    Contains
    Procedure, Public, Non_Overridable :: calc => mhd_hydro_calc_viscous_flux
    Procedure(mhd_hydro_calc_viscous_flux1D_t), Public, Deferred :: calc1D
    Procedure(mhd_hydro_calc_viscous_flux2D_t), Public, Deferred :: calc2D
    Procedure(mhd_hydro_calc_viscous_flux3D_t), Public, Deferred :: calc3D
    Procedure(mhd_hydro_calc_viscous_flux3D_mhd_t), Public, Deferred :: calc3D_mhd
End Type MhdHydroViscousFlux
Private :: mhd_hydro_calc_viscous_flux, &
           mhd_hydro_calc_viscous_flux1D_t, &
           mhd_hydro_calc_viscous_flux2D_t, &
           mhd_hydro_calc_viscous_flux3D_t, &
           mhd_hydro_calc_viscous_flux3D_mhd_t
Interface
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_viscous_flux1D_t(This, &
                                           rho, nrg, u, &
                                           grad_rho, grad_nrg, grad_u, &
                                           flux_rho, flux_nrg, flux_u)
    !> Calculate the Viscous Fluxes in 1D.
    !> {{{
    Import :: MhdHydroViscousFlux
    Class(MhdHydroViscousFlux), Intent(In) :: This
    Real(8), Intent(In) :: rho, nrg, u
    Real(8), Intent(In) :: grad_rho, grad_nrg, grad_u
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
End Subroutine mhd_hydro_calc_viscous_flux1D_t
Pure &
Subroutine mhd_hydro_calc_viscous_flux2D_t(This, &
                                           rho, nrg, u, v, &
                                           grad_rho, grad_nrg, grad_u, grad_v, &
                                           flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the Viscous Fluxes in 2D.
    !> {{{
    Import :: MhdHydroViscousFlux
    Class(MhdHydroViscousFlux), Intent(In) :: This
    Real(8), Intent(In) :: rho, nrg, u, v
    Real(8), Intent(In) :: grad_rho, grad_nrg, grad_u, grad_v
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v
    !> }}}
End Subroutine mhd_hydro_calc_viscous_flux2D_t
Pure &
Subroutine mhd_hydro_calc_viscous_flux3D_t(This, &
                                           rho, nrg, u, v, w, &
                                           grad_rho, grad_nrg, grad_u, grad_v, grad_w, &
                                           flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the Viscous Fluxes in 3D.
    !> {{{
    Import :: MhdHydroViscousFlux
    Class(MhdHydroViscousFlux), Intent(In) :: This
    Real(8), Intent(In) :: rho, nrg, u, v, w
    Real(8), Intent(In) :: grad_rho, grad_nrg, grad_u, grad_v, grad_w
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
End Subroutine mhd_hydro_calc_viscous_flux3D_t
Pure &
Subroutine mhd_hydro_calc_viscous_flux3D_mhd_t(This, &
                                               rho, nrg, u, v, w, bx, by, bz, &
                                               grad_rho, grad_nrg, grad_u, grad_v, grad_w, grad_bx, grad_by, grad_bz, &
                                               flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz)
    !> Calculate the Viscous Fluxes in 3D for the MHD Equations.
    !> {{{
    Import :: MhdHydroViscousFlux
    Class(MhdHydroViscousFlux), Intent(In) :: This
    Real(8), Intent(In) :: rho, nrg, u, v, w, bx, by, bz
    Real(8), Intent(In) :: grad_rho, grad_nrg, grad_u, grad_v, grad_w, grad_bx, grad_by, grad_bz
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz
    !> }}}
End Subroutine mhd_hydro_calc_viscous_flux3D_mhd_t
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Interface
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_viscous_flux(This, &
                                       fields, fields_grad, flux)
    !> Calculate the Viscous Fluxes in 1D/2D/3D.
    !> {{{
    Class(MhdHydroViscousFlux), Intent(In) :: This
    Real(8), Dimension(1:), Intent(In) :: fields, fields_grad
    Real(8), Dimension(1:), Intent(Out) :: flux
    !> }}}
    Real(8) :: rho, nrg, u, v, w, bx, by, bz, & 
               grad_rho, grad_nrg, grad_u, grad_v, grad_w, grad_bx, grad_by, grad_bz
    rho = fields(1)
    nrg = fields(2)/rho
    u   = fields(3)/rho
    grad_rho = fields_grad(1)
    grad_nrg = ( fields_grad(2) - nrg*grad_rho )/rho
    grad_u   = ( fields_grad(3) - u*grad_rho )/rho
    If ( dim >= 2 .OR. mhd ) Then
        v = fields(4)/rho
        grad_v = ( fields_grad(4) - v*grad_rho )/rho
        If ( dim >= 3 .OR. mhd ) Then
            w = fields(5)/rho 
            grad_w = ( fields_grad(5) - w*grad_rho )/rho
       End If
    End If
    If ( mhd ) Then
        !> MHD case.
        bx = fields(6)
        by = fields(7)
        bz = fields(8)
        grad_bx = fields_grad(6)
        grad_by = fields_grad(7)
        grad_bz = fields_grad(8)
        Call This%calc3D_mhd(rho, nrg, u, v, w, bx, by, bz, &
                             grad_rho, grad_nrg, grad_u, grad_v, grad_w, grad_bx, grad_by, grad_bz, &
                             flux(1), flux(2), flux(3), flux(4), flux(5), flux(6), flux(7), flux(8))
    Else
        !> Navier-Stokes case.
        If ( dim == 1 ) Then
            !> 1D Case.
            Call This%calc1D(rho, nrg, u, &
                             grad_rho, grad_nrg, grad_u, &
                             flux(1), flux(2), flux(3))
        Else If ( dim == 2 ) Then
            !> 2D Case.
            Call This%calc2D(rho, nrg, u, v, &
                             grad_rho, grad_nrg, grad_u, grad_v, &
                             flux(1), flux(2), flux(3), flux(4))
        Else If ( dim == 3 ) Then
            !> 3D Case.
            Call This%calc3D(rho, nrg, u, v, w, &
                             grad_rho, grad_nrg, grad_u, grad_v, grad_w, &
                             flux(1), flux(2), flux(3), flux(4), flux(5))
        End If
    End If
End Subroutine mhd_hydro_calc_viscous_flux
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_viscous


Module orchid_solver_hydro_flux_viscous_gas
Use orchid_solver_params
Use orchid_solver_hydro_flux_viscous
Implicit None
Type, Extends(MhdHydroViscousFlux) :: MhdHydroViscousFluxGas
    Contains
    Procedure, Public, Non_Overridable :: calc1D => mhd_hydro_calc_viscous_flux_gas1D
    Procedure, Public, Non_Overridable :: calc2D => mhd_hydro_calc_viscous_flux_gas2D
    Procedure, Public, Non_Overridable :: calc3D => mhd_hydro_calc_viscous_flux_gas3D
    Procedure, Public, Non_Overridable :: calc3D_mhd => mhd_hydro_calc_viscous_flux_gas3D_mhd
End Type MhdHydroViscousFluxGas
Private :: mhd_hydro_calc_viscous_flux_gas1D, &
           mhd_hydro_calc_viscous_flux_gas2D, &
           mhd_hydro_calc_viscous_flux_gas3D, &
           mhd_hydro_calc_viscous_flux_gas3D_mhd
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_viscous_flux_gas1D(This, &
                                           rho, nrg, u, &
                                           grad_rho, grad_nrg, grad_u, &
                                           flux_rho, flux_nrg, flux_u)
    !> Calculate the Viscous Fluxes in 1D.
    !> {{{
    Class(MhdHydroViscousFluxGas), Intent(In) :: This
    Real(8), Intent(In) :: rho, nrg, u
    Real(8), Intent(In) :: grad_rho, grad_nrg, grad_u
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    flux_rho = 0.0D0
    flux_nrg = 0.0D0
    flux_u   = Mu_hydro*grad_u
End Subroutine mhd_hydro_calc_viscous_flux_gas1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_viscous_flux_gas2D(This, &
                                           rho, nrg, u, v, &
                                           grad_rho, grad_nrg, grad_u, grad_v, &
                                           flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the Viscous Fluxes in 2D.
    !> {{{
    Class(MhdHydroViscousFluxGas), Intent(In) :: This
    Real(8), Intent(In) :: rho, nrg, u, v
    Real(8), Intent(In) :: grad_rho, grad_nrg, grad_u, grad_v
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v
    !> }}}
End Subroutine mhd_hydro_calc_viscous_flux_gas2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_viscous_flux_gas3D(This, &
                                             rho, nrg, u, v, w, &
                                             grad_rho, grad_nrg, grad_u, grad_v, grad_w, &
                                             flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the Viscous Fluxes in 3D.
    !> {{{
    Class(MhdHydroViscousFluxGas), Intent(In) :: This
    Real(8), Intent(In) :: rho, nrg, u, v, w
    Real(8), Intent(In) :: grad_rho, grad_nrg, grad_u, grad_v, grad_w
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
End Subroutine mhd_hydro_calc_viscous_flux_gas3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_viscous_flux_gas3D_mhd(This, &
                                                 rho, nrg, u, v, w, bx, by, bz, &
                                                 grad_rho, grad_nrg, grad_u, grad_v, grad_w, grad_bx, grad_by, grad_bz, &
                                                 flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz)
    !> Calculate the Viscous Fluxes in 3D for the MHD Equations.
    !> {{{
    Class(MhdHydroViscousFluxGas), Intent(In) :: This
    Real(8), Intent(In) :: rho, nrg, u, v, w, bx, by, bz
    Real(8), Intent(In) :: grad_rho, grad_nrg, grad_u, grad_v, grad_w, grad_bx, grad_by, grad_bz
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w, flux_bx, flux_by, flux_bz
    !> }}}
End Subroutine mhd_hydro_calc_viscous_flux_gas3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_viscous_gas
