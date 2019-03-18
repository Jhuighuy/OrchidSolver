!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_flux_roe
Use orchid_solver_params
Use orchid_solver_hydro_flux_godunov
Implicit None
Type, Extends(MhdHydroFlux) :: MhdHydroFluxRoe
    Contains
    Procedure, Public, Non_Overridable :: calc1D => mhd_hydro_calc_flux_roe1D
    Procedure, Public, Non_Overridable :: calc2D => mhd_hydro_calc_flux_roe2D
    Procedure, Public, Non_Overridable :: calc3D => mhd_hydro_calc_flux_roe3D
    Procedure, Public, Non_Overridable :: calc3D_mhd => mhd_hydro_calc_flux_roe3D_mhd
End Type MhdHydroFluxRoe
Private :: mhd_hydro_calc_flux_roe1D, &
           mhd_hydro_calc_flux_roe2D, &
           mhd_hydro_calc_flux_roe3D, &
           mhd_hydro_calc_flux_roe3D_mhd
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_roe1D(This, &
                                     nx, &
                                     qp, qm, flux)
    !> Calculate the Roe Fluxes in 1D.
    !> {{{
    Class(MhdHydroFluxRoe), Intent(In) :: This
    Type(MhdHydroVars1D), Intent(In) :: qp, qm
    Real(8), Dimension(1:3), Intent(Out) :: flux
    Real(8), Intent(In) :: nx
    !> }}}
    Type(MhdHydroVars1D) :: qs
    Real(8), Dimension(1:3, 1:3) :: OmegaL, OmegaR
    Real(8), Dimension(1:3) :: Lambda
    Real(8) :: rp, rm
    !> Calculate Average Values.
    rp = Sqrt(qp%Rho)
    rm = sqrt(qm%Rho)
    qs%Rho = 0.5D0*( qp%Rho + qm%Rho )    
    qs%Vx  = ( rp*qp%Vx + rm*qp%Vx )/( rp + rm )
    qs%Vn  = qs%Vx*nx
    qs%V2  = qs%Vx**2
    qs%kin = 0.5D0*qs%V2
    qs%ent = ( rp*qp%ent + rm*qm%ent )/( rp + rm )
    qs%c2snd = Gamma_7*qs%Rho/( qp%Rho + 2.0D0*qs%Rho + qm%Rho )
    qs%c2snd = ( ( qp%Vx - qm%Vx )**2 )*qs%c2snd
    qs%c2snd = ( rp*qp%c2snd + rm*qm%c2snd )/( rp + rm ) + qs%c2snd
    qs%c_snd = Sqrt(Max(qs%c2snd, c2min))
    !> Calculate Eigenvalues.
    Lambda = [ Min(Abs(qs%Vn - qs%c_snd), Abs(qm%Vn - qm%c_snd)), Abs(qs%Vn), &
               Max(Abs(qs%Vn + qs%c_snd), Abs(qp%Vn + qp%c_snd)) ]
    !> Calculate the Right Eigenvectors.
    OmegaR(:,1) = [ 1.0D0, &
                    qs%Vx - qs%c_snd*nx, &
                    qs%ent - qs%c_snd*qs%Vn ]
    OmegaR(:,2) = [ 1.0D0, &
                    qs%Vx, &
                    qs%kin ]
    OmegaR(:,3) = [ 1.0D0, &
                    qs%Vx + qs%c_snd*nx, &
                    qs%ent + qs%c_snd*qs%Vn ]
    !> Calculate the Left Eigenvectors.
    OmegaL(1,:) = [ +Gamma1*qs%kin + qs%c_snd*qs%Vn, &
                    -Gamma1*qs%Vx  - qs%c_snd*nx, &
                    +Gamma1 ]/( 2.0D0*qs%c2snd )
    OmegaL(2,:) = [ -Gamma1*qs%kin + qs%c2snd, &
                    +Gamma1*qs%Vx, &
                    -Gamma1 ]/qs%c2snd
    OmegaL(3,:) = [ +Gamma1*qs%kin - qs%c_snd*qs%Vn, &
                    -Gamma1*qs%Vx  + qs%c_snd*nx, &
                    +Gamma1 ]/( 2.0D0*qs%c2snd )
    !> Calculate Fluxes.
    qs%U = 0.5D0*( qp%U - qm%U )
    qs%U = [ qs%U(1), qs%U(3), qs%U(2) ]
    qs%U = MatMul(OmegaL, qs%U)
    qs%U = MatMul(OmegaR, Lambda*qs%U)
    qs%U = [ qs%U(1), qs%U(3), qs%U(2) ]
    flux = 0.5D0*( qp%F + qm%F ) - qs%U
End Subroutine mhd_hydro_calc_flux_roe1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_roe2D(This, &
                                     nx, ny, &
                                     qp, qm, flux)
    !> Calculate the Roe Fluxes in 2D.
    !> {{{
    Class(MhdHydroFluxRoe), Intent(In) :: This
    Type(MhdHydroVars2D), Intent(In) :: qp, qm
    Real(8), Dimension(1:4), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny
    !> }}}
    Type(MhdHydroVars2D) :: qs
    Real(8), Dimension(1:4, 1:4) :: OmegaL, OmegaR
    Real(8), Dimension(1:4) :: Lambda
    Real(8) :: rp, rm
    !> Calculate Average Values.
    rp = Sqrt(qp%Rho)
    rm = sqrt(qm%Rho)
    qs%Rho = 0.5D0*( qp%Rho + qm%Rho )    
    qs%Vx  = ( rp*qp%Vx + rm*qp%Vx )/( rp + rm )
    qs%Vy  = ( rp*qp%Vy + rm*qp%Vy )/( rp + rm )
    qs%Vn  = qs%Vx*nx + qs%Vy*ny
    qs%V2  = qs%Vx**2 + qs%Vy**2
    qs%kin = 0.5D0*qs%V2
    qs%ent = ( rp*qp%ent + rm*qm%ent )/( rp + rm )
    qs%c2snd = Gamma_7*qs%Rho/( qp%Rho + 2.0D0*qs%Rho + qm%Rho )
    qs%c2snd = ( ( qp%Vx - qm%Vx )**2 + ( qp%Vy - qm%Vy )**2 )*qs%c2snd
    qs%c2snd = ( rp*qp%c2snd + rm*qm%c2snd )/( rp + rm ) + qs%c2snd
    qs%c_snd = Sqrt(Max(qs%c2snd, c2min))
    !> Calculate Eigenvalues.
    Lambda = [ Min(Abs(qs%Vn - qs%c_snd), Abs(qm%Vn - qm%c_snd)), Abs(qs%Vn), &
               Max(Abs(qs%Vn + qs%c_snd), Abs(qp%Vn + qp%c_snd)), Abs(qs%Vn) ]
    !> Calculate the Right Eigenvectors.
    OmegaR(:,1) = [ 1.0D0, &
                    qs%Vx - qs%c_snd*nx, &
                    qs%Vy - qs%c_snd*ny, &
                    qs%ent - qs%c_snd*qs%Vn ]
    OmegaR(:,2) = [ 1.0D0, &
                    qs%Vx, &
                    qs%Vy, &
                    qs%kin ]
    OmegaR(:,3) = [ 1.0D0, &
                    qs%Vx + qs%c_snd*nx, &
                    qs%Vy + qs%c_snd*ny, &
                    qs%ent + qs%c_snd*qs%Vn ]
    OmegaR(:,4) = [ 0.0D0, +ny, -nx, &
                    qs%Vx*ny - qs%Vy*nx ]
    !> Calculate the Left Eigenvectors.
    OmegaL(1,:) = [ +Gamma1*qs%kin + qs%c_snd*qs%Vn, &
                    -Gamma1*qs%Vx  - qs%c_snd*nx, &
                    -Gamma1*qs%Vy  - qs%c_snd*ny, &
                    +Gamma1 ]/( 2.0D0*qs%c2snd )
    OmegaL(2,:) = [ -Gamma1*qs%kin + qs%c2snd, &
                    +Gamma1*qs%Vx, &
                    +Gamma1*qs%Vy, &
                    -Gamma1 ]/qs%c2snd
    OmegaL(3,:) = [ +Gamma1*qs%kin - qs%c_snd*qs%Vn, &
                    -Gamma1*qs%Vx  + qs%c_snd*nx, &
                    -Gamma1*qs%Vy  + qs%c_snd*ny, &
                    +Gamma1 ]/( 2.0D0*qs%c2snd )
    OmegaL(4,:) = [ qs%Vy*nx - qs%Vx*ny, &
                    +ny, -nx, 0.0D0 ]
    !> Calculate Fluxes.
    qs%U = 0.5D0*( qp%U - qm%U )
    qs%U = [ qs%U(1), qs%U(3:4), qs%U(2) ]
    qs%U = MatMul(OmegaL, qs%U)
    qs%U = MatMul(OmegaR, Lambda*qs%U)
    qs%U = [ qs%U(1), qs%U(4), qs%U(2:3) ]
    flux = 0.5D0*( qp%F + qm%F ) - qs%U
End Subroutine mhd_hydro_calc_flux_roe2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_roe3D(This, &
                                     nx, ny, nz, &
                                     qp, qm, flux)
    !> Calculate the Roe Fluxes in 3D.
    !> {{{
    Class(MhdHydroFluxRoe), Intent(In) :: This
    Type(MhdHydroVars3D), Intent(In) :: qp, qm
    Real(8), Dimension(1:5), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Type(MhdHydroVars3D) :: qs
    Real(8), Dimension(1:5, 1:5) :: OmegaL, OmegaR
    Real(8), Dimension(1:5) :: Lambda
    Real(8) :: rp, rm
    !> Calculate Average Values.
    rp = Sqrt(qp%Rho)
    rm = sqrt(qm%Rho)
    qs%Rho = 0.5D0*( qp%Rho + qm%Rho )    
    qs%Vx  = ( rp*qp%Vx + rm*qp%Vx )/( rp + rm )
    qs%Vy  = ( rp*qp%Vy + rm*qp%Vy )/( rp + rm )
    qs%Vz  = ( rp*qp%Vz + rm*qp%Vz )/( rp + rm )
    qs%Vn  = qs%Vx*nx + qs%Vy*ny + qs%Vz*nz
    qs%V2  = qs%Vx**2 + qs%Vy**2 + qs%Vz**2
    qs%kin = 0.5D0*qs%V2
    qs%ent = ( rp*qp%ent + rm*qm%ent )/( rp + rm )
    qs%c2snd = Gamma_7*qs%Rho/( qp%Rho + 2.0D0*qs%Rho + qm%Rho )
    qs%c2snd = ( ( qp%Vx - qm%Vx )**2 + ( qp%Vy - qm%Vy )**2 + ( qp%Vz - qm%Vz )**2 )*qs%c2snd
    qs%c2snd = ( rp*qp%c2snd + rm*qm%c2snd )/( rp + rm ) + qs%c2snd
    qs%c_snd = Sqrt(Max(qs%c2snd, c2min))
    !> Calculate Eigenvalues.
    Lambda = [ Min(Abs(qs%Vn - qs%c_snd), Abs(qm%Vn - qm%c_snd)), Abs(qs%Vn), &
               Max(Abs(qs%Vn + qs%c_snd), Abs(qp%Vn + qp%c_snd)), Abs(qs%Vn), Abs(qs%Vn) ]
    !> Calculate the Right Eigenvectors.
    OmegaR(:,1) = [ 1.0D0, &
                    qs%Vx - qs%c_snd*nx, &
                    qs%Vy - qs%c_snd*ny, &
                    qs%Vz - qs%c_snd*nz, &
                    qs%ent - qs%c_snd*qs%Vn ]
    OmegaR(:,2) = [ 1.0D0, &
                    qs%Vx, &
                    qs%Vy, &
                    qs%Vz, &
                    qs%kin ]
    OmegaR(:,3) = [ 1.0D0, &
                    qs%Vx + qs%c_snd*nx, &
                    qs%Vy + qs%c_snd*ny, &
                    qs%Vz + qs%c_snd*nz, &
                    qs%ent + qs%c_snd*qs%Vn ]
    !> Calculate the Left Eigenvectors.
    OmegaL(1,:) = [ +Gamma1*qs%kin + qs%c_snd*qs%Vn, &
                    -Gamma1*qs%Vx  - qs%c_snd*nx, &
                    -Gamma1*qs%Vy  - qs%c_snd*ny, &
                    -Gamma1*qs%Vz  - qs%c_snd*nz, & 
                    +Gamma1 ]/( 2.0D0*qs%c2snd )
    OmegaL(2,:) = [ -Gamma1*qs%kin + qs%c2snd, &
                    +Gamma1*qs%Vx, &
                    +Gamma1*qs%Vy, &
                    +Gamma1*qs%Vz, &
                    -Gamma1 ]/qs%c2snd
    OmegaL(3,:) = [ +Gamma1*qs%kin - qs%c_snd*qs%Vn, &
                    -Gamma1*qs%Vx  + qs%c_snd*nx, &
                    -Gamma1*qs%Vy  + qs%c_snd*ny, &
                    -Gamma1*qs%Vz  + qs%c_snd*nz, & 
                    +Gamma1 ]/( 2.0D0*qs%c2snd )
    If ( Abs(nx) >= Max(Abs(ny), Abs(nz)) ) Then
        !> Select case with Nx!=0 to prevent singularities.
        !> Calculate the Right Eigenvectors.
        OmegaR(:,4) = [ 0.0D0, +ny, -nx, 0.0D0, &
                        qs%Vx*ny - qs%Vy*nx ]
        OmegaR(:,5) = [ 0.0D0, -nz, 0.0D0, +nx, &
                        qs%Vz*nx - qs%Vx*nz]
        !> Calculate the Left Eigenvectors.
        OmegaL(4,:) = [ ( qs%Vy - qs%Vn*ny )/nx, +ny, &
                        ( ny**2 - 1.0D0 )/nx, (+ny*nz)/nx, 0.0D0 ]
        OmegaL(5,:) = [ ( qs%Vn*nz - qs%Vz )/nx, -nz, &
                        ( -ny*nz )/nx, ( 1.0D0 - nz**2 )/nx, 0.0D0 ]
    Else If ( Abs(ny) >= Max(Abs(nx), Abs(nz)) ) Then
        !> Select case with Ny!=0 to prevent singularities.
        !> Calculate the Right Eigenvectors.
        !> @todo Fix me
        !> Calculate the Left Eigenvectors.
        !> @todo Fix me        
    Else If ( Abs(nz) >= Max(Abs(nx), Abs(ny)) ) Then
        !> Select case with Nz!=0 to prevent singularities.
        !> Calculate the Right Eigenvectors.
        !> @todo Fix me
        !> Calculate the Left Eigenvectors.
        !> @todo Fix me
    End If
    !> Calculate Fluxes.
    qs%U = 0.5D0*( qp%U - qm%U )
    qs%U = [ qs%U(1), qs%U(3:5), qs%U(2) ]
    qs%U = MatMul(OmegaL, qs%U)
    qs%U = MatMul(OmegaR, Lambda*qs%U)
    qs%U = [ qs%U(1), qs%U(5), qs%U(2:4) ]
    flux = 0.5D0*( qp%F + qm%F ) - qs%U
End Subroutine mhd_hydro_calc_flux_roe3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_roe3D_mhd(This, &
                                         nx, ny, nz, &
                                         qp, qm, flux)
    !> Calculate the Roe Fluxes in 3D for the MHD Equations.
    !> {{{
    Class(MhdHydroFluxRoe), Intent(In) :: This
    Type(MhdHydroVars3DMHD), Intent(In) :: qp, qm
    Real(8), Dimension(1:8), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
End Subroutine mhd_hydro_calc_flux_roe3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_roe


