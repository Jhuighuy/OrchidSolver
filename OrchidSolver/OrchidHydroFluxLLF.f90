!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_var
Use orchid_solver_params
Implicit None
Real(8), Parameter :: c2min = 1D-10
Type :: MhdHydroVars
    Real(8) :: rho
    Real(8) :: nrg, eps, kin
    Real(8) :: ent, p
    Real(8) :: Vn, V2
    Real(8) :: c_snd, c2snd
End Type MhdHydroVars
Type, Extends(MhdHydroVars) :: MhdHydroVars1D
    Real(8) :: Vx
    Real(8), Dimension(1:3) :: U
    Real(8), Dimension(1:3) :: F
End Type MhdHydroVars1D
Type, Extends(MhdHydroVars) :: MhdHydroVars2D
    Real(8) :: Vx, Vy
    Real(8), Dimension(1:4) :: U
    Real(8), Dimension(1:4) :: F
End Type MhdHydroVars2D
Type, Extends(MhdHydroVars) :: MhdHydroVars3D
    Real(8) :: Vx, Vy, Vz
    Real(8), Dimension(1:5) :: U
    Real(8), Dimension(1:5) :: F
End Type MhdHydroVars3D
Type :: MhdHydroVars3DMHD
    Real(8) :: rho
    Real(8) :: nrg, eps, kin
    Real(8) :: ent, p, p_tot
    Real(8) :: Vx, Vy, Vz, Vn, V2
    Real(8) :: Bx, By, Bz, Bn, B2
    Real(8) :: BV
    Real(8) :: c_snd, c2snd
    Real(8) :: c_alf, c2alf
    Real(8) :: c_aln, c2aln
    Real(8) :: c_sms, c2sms
    Real(8) :: c_fms, c2fms
    Real(8), Dimension(1:8) :: U
    Real(8), Dimension(1:8) :: F
End Type MhdHydroVars3DMHD
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Function mhd_hydro_vars_load1D(nx, q_cons, q_prim) Result(q)
    !> Initialize the Euler/Navier-Stokes variables in 1D.
    !> Conservative variables are:
    !> * Density, Energy;
    !> * Momentum field.
    !> Primitive variables are:
    !> * Density, Pressure;
    !> * Velocity field.
    !> {{{
    Type(MhdHydroVars1D) :: q
    Real(8), Intent(In) :: nx
    Real(8), Dimension(1:3), Intent(In), Optional :: q_cons, q_prim
    !> }}}
    !> Primitive and Aux variables.
    If ( Present(q_cons) ) Then
        q%rho = q_cons(1)
        q%nrg = q_cons(2)/q%rho
        q%Vx  = q_cons(3)/q%rho
        q%Vn  = q%Vx*nx
        q%V2  = q%Vx**2
        q%kin = 0.5D0*q%V2
        q%eps = q%nrg - q%kin
        q%p   = Gamma1*q%rho*q%eps
        q%ent = q%nrg + q%p/q%rho
    End If
    If ( Present(q_prim) ) Then
        q%rho = q_prim(1)
        q%p   = q_prim(2)
        q%Vx  = q_prim(3)
        q%Vn  = q%Vx*nx
        q%V2  = q%Vx**2
        q%kin = 0.5D0*q%V2
        q%eps = q%p/q%rho/Gamma1
        q%nrg = q%eps + q%kin
        q%ent = q%nrg + q%p/q%rho
    End If
    !> Wave speeds.
    q%c2snd = Gamma*q%p/q%rho
    q%c_snd = Sqrt(Max(q%c2snd, c2min))
    !> Conservative variables and Fluxes.
    q%U(:) = [ q%rho, &
               q%rho*q%nrg, &
               q%rho*q%Vx ]
    q%F(:) = [ q%rho*q%Vn, &
               q%rho*q%Vn*q%ent, &
               q%rho*q%Vn*q%Vx + q%p*nx ]
End Function mhd_hydro_vars_load1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Function mhd_hydro_vars_load2D(nx, ny, q_cons, q_prim) Result(q)
    !> Initialize the Euler/Navier-Stokes variables in 2D.
    !> Conservative variables are:
    !> * Density, Energy;
    !> * Momentum field.
    !> Primitive variables are:
    !> * Density, Pressure;
    !> * Velocity field.
    !> {{{
    Type(MhdHydroVars2D) :: q
    Real(8), Intent(In) :: nx, ny
    Real(8), Dimension(1:4), Intent(In), Optional :: q_cons, q_prim
    !> }}}
    !> Primitive and Aux variables.
    If ( Present(q_cons) ) Then
        q%rho = q_cons(1)
        q%nrg = q_cons(2)/q%rho
        q%Vx  = q_cons(3)/q%rho
        q%Vy  = q_cons(4)/q%rho
        q%Vn  = q%Vx*nx + q%Vy*ny
        q%V2  = q%Vx**2 + q%Vy**2
        q%kin = 0.5D0*q%V2
        q%eps = q%nrg - q%kin
        q%p   = Gamma1*q%rho*q%eps
        q%ent = q%nrg + q%p/q%rho
    End If
    If ( Present(q_prim) ) Then
        q%rho = q_prim(1)
        q%p   = q_prim(2)
        q%Vx  = q_prim(3)
        q%Vy  = q_prim(4)
        q%Vn  = q%Vx*nx + q%Vy*ny
        q%V2  = q%Vx**2 + q%Vy**2
        q%kin = 0.5D0*q%V2
        q%eps = q%p/q%rho/Gamma1
        q%nrg = q%eps + q%kin
        q%ent = q%nrg + q%p/q%rho
    End If
    !> Wave speeds.
    q%c2snd = Gamma*q%p/q%rho
    q%c_snd = Sqrt(Max(q%c2snd, c2min))
    !> Conservative variables and Fluxes.
    q%U(:) = [ q%rho, &
               q%rho*q%nrg, &
               q%rho*q%Vx, q%rho*q%Vy ]
    q%F(:) = [ q%rho*q%Vn, &
               q%rho*q%Vn*q%ent, &
               q%rho*q%Vn*q%Vx + q%p*nx, &
               q%rho*q%Vn*q%Vy + q%p*ny ]
End Function mhd_hydro_vars_load2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Function mhd_hydro_vars_load3D(nx, ny, nz, q_cons, q_prim) Result(q)
    !> Initialize the Euler/Navier-Stokes variables in 3D.
    !> Conservative variables are:
    !> * Density, Energy;
    !> * Momentum field.
    !> Primitive variables are:
    !> * Density, Pressure;
    !> * Velocity field.
    !> {{{
    Type(MhdHydroVars3D) :: q
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Dimension(1:5), Intent(In), Optional :: q_cons, q_prim
    !> }}}
    !> Primitive and Aux variables.
    If ( Present(q_cons) ) Then
        q%rho = q_cons(1)
        q%nrg = q_cons(2)/q%rho
        q%Vx  = q_cons(3)/q%rho
        q%Vy  = q_cons(4)/q%rho
        q%Vz  = q_cons(5)/q%rho
        q%Vn  = q%Vx*nx + q%Vy*ny + q%Vz*nz
        q%V2  = q%Vx**2 + q%Vy**2 + q%Vz**2
        q%kin = 0.5D0*q%V2
        q%eps = q%nrg - q%kin
        q%p   = Gamma1*q%rho*q%eps
        q%ent = q%nrg + q%p/q%rho
    End If
    If ( Present(q_prim) ) Then
        q%rho = q_prim(1)
        q%p   = q_prim(2)
        q%Vx  = q_prim(3)
        q%Vy  = q_prim(4)
        q%Vz  = q_prim(5)
        q%Vn  = q%Vx*nx + q%Vy*ny + q%Vz*nz
        q%V2  = q%Vx**2 + q%Vy**2 + q%Vz**2
        q%kin = 0.5D0*q%V2
        q%eps = q%p/q%rho/Gamma1
        q%nrg = q%eps + q%kin
        q%ent = q%nrg + q%p/q%rho
    End If
    !> Wave speeds.
    q%c2snd = Gamma*q%p/q%rho
    q%c_snd = Sqrt(Max(q%c2snd, c2min))
    !> Conservative variables and Fluxes.
    q%U(:) = [ q%rho, &
               q%rho*q%nrg, &
               q%rho*q%Vx, q%rho*q%Vy, q%rho*q%Vz ]
    q%F(:) = [ q%rho*q%Vn, &
               q%rho*q%Vn*q%ent, &
               q%rho*q%Vn*q%Vx + q%p*nx, &
               q%rho*q%Vn*q%Vy + q%p*ny, &
               q%rho*q%Vn*q%Vz + q%p*nz ]
End Function mhd_hydro_vars_load3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Function mhd_hydro_vars_load3D_mhd(nx, ny, nz, q_cons, q_prim) Result(q)
    !> Initialize the MHD variables in 3D.
    !> Conservative variables are:
    !> * Density, Total Energy (Gas + Magnetic);
    !> * Momentum field.
    !> * Magnetic field.
    !> Primitive variables are:
    !> * Density, Gas Pressure;
    !> * Velocity field.
    !> * Magnetic field * Sqrt(4*Pi).
    !> {{{
    Type(MhdHydroVars3DMHD) :: q
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Dimension(1:8), Intent(In), Optional :: q_cons, q_prim
    !> }}}
    !> Primitive and Aux variables.
    If ( Present(q_cons) ) Then
        q%rho = q_cons(1)
        q%nrg = q_cons(2)/q%rho
        q%Vx  = q_cons(3)/q%rho
        q%Vy  = q_cons(4)/q%rho
        q%Vz  = q_cons(5)/q%rho
        q%Vn  = q%Vx*nx + q%Vy*ny + q%Vz*nz
        q%V2  = q%Vx**2 + q%Vy**2 + q%Vz**2
        q%Bx  = q_cons(6)*0.5D0/Sqrt(Pi)
        q%By  = q_cons(7)*0.5D0/Sqrt(Pi)
        q%Bz  = q_cons(8)*0.5D0/Sqrt(Pi)
        q%Bn  = q%Bx*nx + q%By*ny + q%Bz*nz
        q%B2  = q%Bx**2 + q%By**2 + q%Bz**2
        q%BV  = q%Bx*q%Vx + q%By*q%Vy + q%Bz*q%Vz
        q%kin = 0.5D0*( q%V2 + q%B2/q%rho )
        q%eps = q%nrg - q%kin
        q%p   = Gamma1*q%rho*q%eps
        q%p_tot = q%p + 0.5D0*q%B2
        q%ent = q%nrg + q%p_tot/q%rho
    End If
    If ( Present(q_prim) ) Then
        q%rho = q_prim(1)
        q%p   = q_prim(2)
        q%Vx  = q_prim(3)
        q%Vy  = q_prim(4)
        q%Vz  = q_prim(5)
        q%Vn  = q%Vx*nx + q%Vy*ny + q%Vz*nz
        q%V2  = q%Vx**2 + q%Vy**2 + q%Vz**2
        q%Bx  = q_prim(6)*0.5D0/Sqrt(Pi)
        q%By  = q_prim(7)*0.5D0/Sqrt(Pi)
        q%Bz  = q_prim(8)*0.5D0/Sqrt(Pi)
        q%Bn  = q%Bx*nx + q%By*ny + q%Bz*nz
        q%B2  = q%Bx**2 + q%By**2 + q%Bz**2
        q%BV  = q%Bx*q%Vx + q%By*q%Vy + q%Bz*q%Vz
        q%kin = 0.5D0*( q%V2 + q%B2/q%rho )
        q%eps = q%p/q%rho/Gamma1
        q%nrg = q%eps + q%kin
        q%p_tot = q%p + 0.5D0*q%B2
        q%ent = q%nrg + q%p_tot/q%rho
    End If
    !> Wave speeds.
    q%c2snd = Gamma*q%p/q%rho
    q%c2alf = q%B2/q%rho
    q%c2aln = q%Bn**2/q%rho
    q%c_snd = Sqrt(Max(q%c2snd, c2min))
    q%c_alf = Sqrt(Max(q%c2alf, c2min))
    q%c_aln = Sqrt(Max(q%c2aln, c2min))
    q%c2sms = ( q%c2snd + q%c2alf )**2 - 4.0D0*q%c2snd*q%c2aln
    q%c2sms = Sqrt(Max(q%c2sms, c2min**2))
    q%c2fms = 0.5D0*( q%c2snd + q%c2alf + q%c2sms )
    q%c2sms = 0.5D0*( q%c2snd + q%c2alf - q%c2sms )
    q%c_fms = Sqrt(Max(q%c2fms, c2min))
    q%c_sms = Sqrt(Max(q%c2sms, c2min))
    !> Conservative variables and Fluxes.
    q%U(:) = [ q%rho, &
               q%rho*q%nrg, &
               q%rho*q%Vx, q%rho*q%Vy, q%rho*q%Vz, & 
               [ q%Bx, q%By, q%Bz ]*2.0D0*Sqrt(Pi) ]
    q%F(:) = [ q%rho*q%Vn, &
               q%rho*q%Vn*q%ent - q%Bn*q%BV, &
               q%rho*q%Vn*q%Vx - q%Bx*q%Bn + q%p_tot*nx, &
               q%rho*q%Vn*q%Vy - q%By*q%Bn + q%p_tot*ny, &
               q%rho*q%Vn*q%Vz - q%Bz*q%Bn + q%p_tot*nz, &
               [ q%Bx*q%Vn - q%Bn*q%Vx, &
                 q%By*q%Vn - q%Bn*q%Vy, &
                 q%Bz*q%Vn - q%Bn*q%Vz ]*2.0D0*Sqrt(Pi) ]
End Function mhd_hydro_vars_load3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_var
    

    
Module orchid_solver_hydro_flux_godunov
Use orchid_solver_params
Use orchid_solver_hydro_var
Implicit None
Type :: MhdHydroFlux
    Contains
    Procedure, Public, Non_Overridable :: &
        calc => mhd_hydro_calc_flux_t
    Procedure, Public, NoPass :: &
        calc1D => mhd_hydro_calc_flux1D_t, &
        calc2D => mhd_hydro_calc_flux2D_t, &
        calc3D => mhd_hydro_calc_flux3D_t, &
        calc3D_mhd => mhd_hydro_calc_flux3D_mhd_t
End Type MhdHydroFlux
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_t(This, &
                                 qp_cons, qm_cons, flux, &
                                 nx, ny, nz)
    !> Calculate the Godunov Fluxes in 1D/2D/3D.
    !> {{{
    Class(MhdHydroFlux), Intent(In) :: This
    Real(8), Dimension(1:), Intent(In) :: qp_cons, qm_cons
    Real(8), Dimension(1:), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Real(8) :: nl
    If ( mhd ) Then
        !> MHD case.
        nl = Sqrt(nx**2 + ny**2 + nz**2)
        Call This%calc3d_mhd(nx/nl, ny/nl, nz/nl, &
                             mhd_hydro_vars_load3D_mhd(nx/nl, ny/nl, nz/nl, qp_cons(1:8)), &
                             mhd_hydro_vars_load3D_mhd(nx/nl, ny/nl, nz/nl, qm_cons(1:8)), &
                             flux(1:8))
    Else
        !> Euler/Navier-Stokes case.
        Select Case ( dim )
        Case ( 1 )
            nl = Abs(nx)
            Call This%calc1D(nx/nl, &
                             mhd_hydro_vars_load1D(nx/nl, qp_cons(1:3)), &
                             mhd_hydro_vars_load1D(nx/nl, qm_cons(1:3)), &
                             flux(1:3))
        Case ( 2 )
            nl = Sqrt(nx**2 + ny**2)
            Call This%calc2D(nx/nl, ny/nl, &
                             mhd_hydro_vars_load2D(nx/nl, ny/nl, qp_cons(1:4)), &
                             mhd_hydro_vars_load2D(nx/nl, ny/nl, qm_cons(1:4)), &
                             flux(1:4))
        Case ( 3 )
            nl = Sqrt(nx**2 + ny**2 + nz**2)
            Call This%calc3D(nx/nl, ny/nl, nz/nl, &
                             mhd_hydro_vars_load3D(nx/nl, ny/nl, nz/nl, qp_cons(1:5)), &
                             mhd_hydro_vars_load3D(nx/nl, ny/nl, nz/nl, qm_cons(1:5)), &
                             flux(1:5))
        End Select
    End If
End Subroutine mhd_hydro_calc_flux_t
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux1D_t(nx, &
                                   qp, qm, flux)
    !> Calculate the Godunov Fluxes in 1D for the Euler Equations.
    !> {{{
    Type(MhdHydroVars1D), Intent(In) :: qp, qm
    Real(8), Dimension(1:3), Intent(Out) :: flux
    Real(8), Intent(In) :: nx
    !> }}}
    flux(:) = NaN
End Subroutine mhd_hydro_calc_flux1D_t
Pure &
Subroutine mhd_hydro_calc_flux2D_t(nx, ny, &
                                   qp, qm, flux)
    !> Calculate the Godunov Fluxes in 2D for the Euler Equations.
    !> {{{
    Type(MhdHydroVars2D), Intent(In) :: qp, qm
    Real(8), Dimension(1:4), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny
    !> }}}
    flux(:) = NaN
End Subroutine mhd_hydro_calc_flux2D_t
Pure &
Subroutine mhd_hydro_calc_flux3D_t(nx, ny, nz, &
                                   qp, qm, flux)
    !> Calculate the Godunov Fluxes in 3D for the Euler Equations.
    !> {{{
    Type(MhdHydroVars3D), Intent(In) :: qp, qm
    Real(8), Dimension(1:5), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    flux(:) = NaN
End Subroutine mhd_hydro_calc_flux3D_t
Pure &
Subroutine mhd_hydro_calc_flux3D_mhd_t(nx, ny, nz, &
                                       qp, qm, flux)
    !> Calculate the Godunov Fluxes in 3D for the MHD Equations.
    !> {{{
    Type(MhdHydroVars3DMHD), Intent(In) :: qp, qm
    Real(8), Dimension(1:8), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    flux(:) = NaN
End Subroutine mhd_hydro_calc_flux3D_mhd_t
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
    Procedure, Public, NoPass, Non_Overridable :: &
        calc1D => mhd_hydro_calc_flux_llf1D, &
        calc2D => mhd_hydro_calc_flux_llf2D, &
        calc3D => mhd_hydro_calc_flux_llf3D, &
        calc3D_mhd => mhd_hydro_calc_flux_llf3D_mhd
End Type MhdHydroFluxLLF
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_llf1D(nx, &
                                     qp, qm, flux)
    !> Calculate the Local Lax-Friedrichs (Rusanov) Fluxes in 1D.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars1D), Intent(In) :: qp, qm
    Real(8), Dimension(1:3), Intent(Out) :: flux
    Real(8), Intent(In) :: nx
    !> }}}
    Real(8) :: Lambda
    !> [1], Eq. (10.55-10.56).
    Lambda = Max(Abs(qp%Vn) + qp%c_snd, Abs(qm%Vn) + qm%c_snd)
    flux = 0.5D0*( qp%F + qm%F ) - &
           0.5D0*( qp%U - qm%U )*Lambda
End Subroutine mhd_hydro_calc_flux_llf1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_llf2D(nx, ny, &
                                     qp, qm, flux)
    !> Calculate the Local Lax-Friedrichs (Rusanov) Fluxes in 2D.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars2D), Intent(In) :: qp, qm
    Real(8), Dimension(1:4), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny
    !> }}}
    Real(8) :: Lambda
    !> [1], Eq. (10.55-10.56).
    Lambda = Max(Abs(qp%Vn) + qp%c_snd, Abs(qm%Vn) + qm%c_snd)
    flux = 0.5D0*( qp%F + qm%F ) - &
           0.5D0*( qp%U - qm%U )*Lambda
End Subroutine mhd_hydro_calc_flux_llf2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_llf3D(nx, ny, nz, &
                                     qp, qm, flux)
    !> Calculate the Local Lax-Friedrichs (Rusanov) Fluxes in 3D.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars3D), Intent(In) :: qp, qm
    Real(8), Dimension(1:5), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Real(8) :: Lambda
    !> [1], Eq. (10.55-10.56).
    Lambda = Max(Abs(qp%Vn) + qp%c_snd, Abs(qm%Vn) + qm%c_snd)
    flux = 0.5D0*( qp%F + qm%F ) - &
           0.5D0*( qp%U - qm%U )*Lambda
End Subroutine mhd_hydro_calc_flux_llf3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_llf3D_mhd(nx, ny, nz, &
                                         qp, qm, flux)
    !> Calculate the Local Lax-Friedrichs (Rusanov) Fluxes in 3D for the MHD Equations.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars3DMHD), Intent(In) :: qp, qm
    Real(8), Dimension(1:8), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Real(8) :: Lambda
    !> [1], Eq. (10.55-10.56).
    Lambda = Max(Abs(qp%Vn) + qp%c_fms, Abs(qm%Vn) + qm%c_fms)
    flux = 0.5D0*( qp%F + qm%F ) - &
           0.5D0*( qp%U - qm%U )*Lambda
End Subroutine mhd_hydro_calc_flux_llf3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_llf


