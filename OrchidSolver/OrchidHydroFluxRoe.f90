!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_jacobian
Use orchid_solver_params
Use orchid_solver_hydro_var
Implicit None
Type :: MhdHydroJacobian1D
    Real(8), Dimension(1:3) :: Lambda
    Real(8), Dimension(1:3, 1:3) :: OmegaL, OmegaR
End Type MhdHydroJacobian1D
Type :: MhdHydroJacobian2D
    Real(8), Dimension(1:4) :: Lambda
    Real(8), Dimension(1:4, 1:4) :: OmegaL, OmegaR
End Type MhdHydroJacobian2D
Type :: MhdHydroJacobian3D
    Real(8), Dimension(1:5) :: Lambda
    Real(8), Dimension(1:5, 1:5) :: OmegaL, OmegaR
End Type MhdHydroJacobian3D
Type :: MhdHydroJacobian1DMHD
    Real(8), Dimension(1:7) :: Lambda
    Real(8), Dimension(1:7, 1:7) :: OmegaL, OmegaR
End Type MhdHydroJacobian1DMHD
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_jacobian_load1D(q, J, nx)
    !> Calculate the Euler equations Jacobian decomposision in 1D.
    !> {{{
    Type(MhdHydroVars1D), Intent(In) :: q
    Type(MhdHydroJacobian1D), Intent(Out) :: J
    Real(8), Intent(In) :: nx
    !> }}}
    !> Calculate Eigenvalues.
    J%Lambda(:) = [ q%Vn - q%c_snd, q%Vn, &
                    q%Vn + q%c_snd ]
    !> Calculate the Right Eigenvectors.
    J%OmegaR(:,1) = [ 1.0D0, &
                      q%Vx - q%c_snd*nx, &
                      q%ent - q%c_snd*q%Vn ]
    J%OmegaR(:,2) = [ 1.0D0, &
                      q%Vx, &
                      q%kin ]
    J%OmegaR(:,3) = [ 1.0D0, &
                      q%Vx + q%c_snd*nx, &
                      q%ent + q%c_snd*q%Vn ]
    !> Calculate the Left Eigenvectors.
    J%OmegaL(1,:) = [ +Gamma1*q%kin + q%c_snd*q%Vn, &
                      -Gamma1*q%Vx  - q%c_snd*nx, &
                      +Gamma1 ]/( 2.0D0*q%c2snd )
    J%OmegaL(2,:) = [ -Gamma1*q%kin + q%c2snd, &
                      +Gamma1*q%Vx, &
                      -Gamma1 ]/q%c2snd
    J%OmegaL(3,:) = [ +Gamma1*q%kin - q%c_snd*q%Vn, &
                      -Gamma1*q%Vx  + q%c_snd*nx, &
                      +Gamma1 ]/( 2.0D0*q%c2snd )
End Subroutine mhd_hydro_jacobian_load1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_jacobian_load2D(q, J, nx, ny)
    !> Calculate the Euler equations Jacobian decomposision in 2D.
    !> {{{
    Type(MhdHydroVars2D), Intent(In) :: q
    Type(MhdHydroJacobian2D), Intent(Out) :: J
    Real(8), Intent(In) :: nx, ny
    !> }}}
    !> Calculate Eigenvalues.
    J%Lambda(:) = [ q%Vn - q%c_snd, q%Vn, &
                    q%Vn + q%c_snd, q%Vn ]
    !> Calculate the Right Eigenvectors.
    J%OmegaR(:,1) = [ 1.0D0, &
                      q%Vx - q%c_snd*nx, &
                      q%Vy - q%c_snd*ny, &
                      q%ent - q%c_snd*q%Vn ]
    J%OmegaR(:,2) = [ 1.0D0, &
                      q%Vx, &
                      q%Vy, &
                      q%kin ]
    J%OmegaR(:,3) = [ 1.0D0, &
                      q%Vx + q%c_snd*nx, &
                      q%Vy + q%c_snd*ny, &
                      q%ent + q%c_snd*q%Vn ]
    J%OmegaR(:,4) = [ 0.0D0, +ny, -nx, &
                      q%Vx*ny - q%Vy*nx ]
    !> Calculate the Left Eigenvectors.
    J%OmegaL(1,:) = [ +Gamma1*q%kin + q%c_snd*q%Vn, &
                      -Gamma1*q%Vx  - q%c_snd*nx, &
                      -Gamma1*q%Vy  - q%c_snd*ny, &
                      +Gamma1 ]/( 2.0D0*q%c2snd )
    J%OmegaL(2,:) = [ -Gamma1*q%kin + q%c2snd, &
                      +Gamma1*q%Vx, &
                      +Gamma1*q%Vy, &
                      -Gamma1 ]/q%c2snd
    J%OmegaL(3,:) = [ +Gamma1*q%kin - q%c_snd*q%Vn, &
                      -Gamma1*q%Vx  + q%c_snd*nx, &
                      -Gamma1*q%Vy  + q%c_snd*ny, &
                      +Gamma1 ]/( 2.0D0*q%c2snd )
    J%OmegaL(4,:) = [ q%Vy*nx - q%Vx*ny, &
                      +ny, -nx, 0.0D0 ]
End Subroutine mhd_hydro_jacobian_load2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_jacobian_load3D(q, J, nx, ny, nz)
    !> Calculate the Euler equations Jacobian decomposision in 2D.
    !> {{{
    Type(MhdHydroVars3D), Intent(In) :: q
    Type(MhdHydroJacobian3D), Intent(Out) :: J
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Integer :: dir
    !> Select direction to prevent 0/0 singularities.
    If ( Abs(nx) >= Max(Abs(ny), Abs(nz)) ) Then
        dir = 1
    Else If ( Abs(ny) >= Max(Abs(nx), Abs(nz)) ) Then
        dir = 2
    Else If ( Abs(nz) >= Max(Abs(nx), Abs(ny)) ) Then
        dir = 3
    End If
    !> Calculate Eigenvalues.
    J%Lambda(:) = [ q%Vn - q%c_snd, q%Vn, &
                    q%Vn + q%c_snd, q%Vn, q%Vn ]
    !> Calculate the Right Eigenvectors.
    J%OmegaR(:,1) = [ 1.0D0, &
                      q%Vx - q%c_snd*nx, &
                      q%Vy - q%c_snd*ny, &
                      q%Vz - q%c_snd*nz, &
                      q%ent - q%c_snd*q%Vn ]
    J%OmegaR(:,2) = [ 1.0D0, &
                      q%Vx, &
                      q%Vy, &
                      q%Vz, &
                      q%kin ]
    J%OmegaR(:,3) = [ 1.0D0, &
                      q%Vx + q%c_snd*nx, &
                      q%Vy + q%c_snd*ny, &
                      q%Vz + q%c_snd*nz, &
                      q%ent + q%c_snd*q%Vn ]
    Select Case ( dir )
    Case ( 1 )
        J%OmegaR(:,4) = [ 0.0D0, +ny, -nx, 0.0D0, &
                          q%Vx*ny - q%Vy*nx ]
        J%OmegaR(:,5) = [ 0.0D0, -nz, 0.0D0, +nx, &
                          q%Vz*nx - q%Vx*nz ]
    Case ( 2 )
        J%OmegaR(:,4) = [ 0.0D0, +ny, -nx, 0.0D0, &
                          q%Vx*ny - q%Vy*nx ]
        J%OmegaR(:,5) = [ 0.0D0, 0.0D0, +nz, -ny, &
                          q%Vy*nz - q%Vz*ny ]
    Case ( 3 )
        J%OmegaR(:,4) = [ 0.0D0, -nz, 0.0D0, nx, &
                          q%Vz*nx - q%Vx*nz ]
        J%OmegaR(:,5) = [ 0.0D0, 0.0D0, +nz, -ny, &
                          q%Vy*nz - q%Vz*ny ]
    End Select
    !> Calculate the Left Eigenvectors.
    J%OmegaL(1,:) = [ +Gamma1*q%kin + q%c_snd*q%Vn, &
                      -Gamma1*q%Vx  - q%c_snd*nx, &
                      -Gamma1*q%Vy  - q%c_snd*ny, &
                      -Gamma1*q%Vz  - q%c_snd*nz, & 
                      +Gamma1 ]/( 2.0D0*q%c2snd )
    J%OmegaL(2,:) = [ -Gamma1*q%kin + q%c2snd, &
                      +Gamma1*q%Vx, &
                      +Gamma1*q%Vy, &
                      +Gamma1*q%Vz, &
                      -Gamma1 ]/q%c2snd
    J%OmegaL(3,:) = [ +Gamma1*q%kin - q%c_snd*q%Vn, &
                      -Gamma1*q%Vx  + q%c_snd*nx, &
                      -Gamma1*q%Vy  + q%c_snd*ny, &
                      -Gamma1*q%Vz  + q%c_snd*nz, & 
                      +Gamma1 ]/( 2.0D0*q%c2snd )
    Select Case ( dir )
    Case ( 1 )
        J%OmegaL(4,:) = [ ( q%Vy - q%Vn*ny )/nx, +ny, &
                          ( ny**2 - 1.0D0 )/nx, (+ny*nz)/nx, 0.0D0 ]
        J%OmegaL(5,:) = [ ( q%Vn*nz - q%Vz )/nx, -nz, &
                          ( -ny*nz )/nx, ( 1.0D0 - nz**2 )/nx, 0.0D0 ]
    Case ( 2 )
        !> @todo Fix me!
    Case ( 3 )
        !> @todo Fix me!
    End Select
End Subroutine mhd_hydro_jacobian_load3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_jacobian_load1D_mhd(q, J)
    !> Calculate the MHD equations Jacobian decomposision in 1D 
    !> (in X direction, assuming Bx is const).
    !> {{{
    Type(MhdHydroVars3DMHD), Intent(In) :: q
    Type(MhdHydroJacobian1DMHD), Intent(Out) :: J
    !> }}}
    Real(8) :: s, r, t, t2
    Real(8) :: Alpha2fms, Alpha2snd, &
               Alpha_fms, Alpha_snd
    Real(8) :: R_fms, R_sms
    Real(8) :: Beta_y, Beta_z
    Real(8), Dimension(1:7, 1:7) :: UW, WU
    !> Calculate aux variables.
    s = Sqrt(q%By**2 + q%Bz**2)
    Beta_y = q%By/s
    Beta_z = q%Bz/s
    s = q%c2fms - q%c2sms
    Alpha2fms = ( q%c2fms - q%c2aln )/s
    Alpha2snd = ( q%c2fms - q%c2snd )/s
    Alpha_fms = Sqrt(Alpha2fms)
    Alpha_snd = Sqrt(Alpha2snd)
    R_fms = Alpha2fms*( q%c2fms + q%c2snd ) + Alpha2snd*( q%c2fms + q%c2aln )
    R_sms = Alpha2fms*q%c2snd*( q%c2fms + q%c2snd ) + Alpha2snd*q%c2fms*( q%c2sms + q%c2snd )
    R_fms = q%c_fms/Sqrt(R_fms)
    R_sms = q%c2fms/Sqrt(R_sms)
    s = Sign(1.0D0, q%Bx)
    r = 0.5D0*Sqrt(Pi*q%Rho)
    !> Calculate Eigenvalues.
    J%Lambda(:) = [ q%Vx - q%c_fms, q%Vx - q%c_alf, q%Vx - q%c_sms, &
                    q%Vx + q%c_fms, q%Vx + q%c_alf, q%Vx + q%c_sms, q%Vx ]
    !> Calculate the Right Eigenvectors in W.
    J%OmegaR(:,1) = [ -Alpha_fms*t, +Alpha_fms*q%c_fms, &
                      -Alpha_snd*Beta_y*q%c_aln*s, &
                      -Alpha_snd*Beta_z*q%c_aln*s, &
                      +Alpha_snd*Beta_y*q%c_fms*r, &
                      +Alpha_snd*Beta_z*q%c_fms*r, &
                      +Alpha_fms*Gamma*q%p ]*R_fms
    J%OmegaR(:,2) = [ 0.0D0, 0.0D0, +Beta_y, -Beta_z, &
                      -s*r*Beta_z, +s*r*Beta_y, &
                      0.0D0 ]*q%c_fms/Sqrt(2.0D0)
    J%OmegaR(:,3) = [ -Alpha_snd*t, +Alpha_snd*q%c_sms, &
                      +Alpha_fms*Beta_y*q%c_snd*s, &
                      +Alpha_fms*Beta_z*q%c_snd*s, &
                      -Alpha_fms*Beta_y*q%c2snd/q%c_fms*r, &
                      -Alpha_fms*Beta_z*q%c2snd/q%c_fms*r, &
                      +Alpha_snd*Gamma*q%p ]*R_sms
    J%OmegaR(:,4) = [ 1.0D0/q%Rho, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 ]
    J%OmegaR(:,5) = [ +Alpha_snd*t, +Alpha_snd*q%c_sms, &
                      +Alpha_fms*Beta_y*q%c_snd*s, &
                      +Alpha_fms*Beta_z*q%c_snd*s, &
                      +Alpha_fms*Beta_y*q%c2snd/q%c_fms*r, &
                      +Alpha_fms*Beta_z*q%c2snd/q%c_fms*r, &
                      -Alpha_snd*Gamma*q%p ]*R_sms
    J%OmegaR(:,6) = [ 0.0D0, 0.0D0, -Beta_y, +Beta_z, &
                      -s*r*Beta_z, +s*r*Beta_y, &
                      0.0D0 ]*q%c_fms/Sqrt(2.0D0)
    J%OmegaR(:,7) = [ +Alpha_fms*t, +Alpha_fms*q%c_fms, &
                      -Alpha_snd*Beta_y*q%c_aln*s, &
                      -Alpha_snd*Beta_z*q%c_aln*s, &
                      -Alpha_snd*Beta_y*q%c_fms*r, &
                      -Alpha_snd*Beta_z*q%c_fms*r, &
                      +Alpha_fms*Gamma*q%p ]*R_fms
    !> Calculate the Left Eigenvectors in W.
    J%OmegaL(3,:) = [ 0.0D0, &
                      +Alpha_snd*q%c_sms, &
                      +Alpha_fms*Beta_y*q%c_snd*s, &
                      +Alpha_fms*Beta_z*q%c_snd*s, &
                      +Alpha_fms*Beta_y*q%c2snd/q%c_fms/r, &
                      +Alpha_fms*Beta_z*q%c2snd/q%c_fms/r, &
                      -Alpha_snd*Gamma*q%p ]*R_sms/q%c2fms
    J%OmegaL(4,:) = [ q%Rho, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0/( Gamma*q%p ) ]
    J%OmegaL(5,:) = [ 0.0D0, &
                      +Alpha_snd*q%c_sms, &
                      +Alpha_fms*Beta_y*q%c_snd*s, &
                      +Alpha_fms*Beta_z*q%c_snd*s, &
                      -Alpha_fms*Beta_y*q%c2snd/q%c_fms/r, &
                      -Alpha_fms*Beta_z*q%c2snd/q%c_fms/r, &
                      +Alpha_snd*Gamma*q%p ]*R_sms/q%c2fms
    !> Calculate the dW/dU.
    t = 1.0D0/q%Rho
    t2 = t**2
    WU(1,:) = [ -t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 ]
    WU(2,:) = [ -t2*q%Vx, t, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 ]
    WU(3,:) = [ -t2*q%Vy, 0.0D0, t, 0.0D0, 0.0D0, 0.0D0, 0.0D0 ]
    WU(4,:) = [ -t2*q%Vz, 0.0D0, 0.0D0, t, 0.0D0, 0.0D0, 0.0D0 ]    
    WU(5,:) = [ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0 ]
    WU(6,:) = [ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0 ]
    UW(7,:) = [ +Gamma1*q%V2*0.5D0, &
                -Gamma1*q%Vx, -Gamma1*q%Vz, -Gamma1*q%Vz, &
                -Gamma1*q%By*0.5D0/Sqrt(Pi), -Gamma1*q%Bz*0.5D0/Sqrt(Pi), +Gamma1 ]
    !> Calculate the dU/dW.
    t = 1.0D0*q%Rho
    t2 = t**2
    UW(1,:) = [ -t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 ]
    UW(2,:) = [ -t2*q%Vx, t, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 ]
    UW(3,:) = [ -t2*q%Vy, 0.0D0, t, 0.0D0, 0.0D0, 0.0D0, 0.0D0 ]
    UW(4,:) = [ -t2*q%Vz, 0.0D0, 0.0D0, t, 0.0D0, 0.0D0, 0.0D0 ]
    UW(5,:) = [ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0, 0.0D0 ]
    UW(6,:) = [ 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 1.0D0, 0.0D0 ]
    UW(7,:) = [ -t2*q%V2*0.5D0, &
                +t*q%Vx, +t*q%Vz, +t*q%Vz, &
                +q%By*0.5D0/Sqrt(Pi), +q%Bz*0.5D0/Sqrt(Pi), +1.0D0/Gamma1 ]
    !> Translate Eigenvectors from W to U.
    J%OmegaR = MatMul(J%OmegaR, WU)
    J%OmegaL = MatMul(UW, J%OmegaL)
End Subroutine mhd_hydro_jacobian_load1D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_jacobian



Module orchid_solver_hydro_flux_roe
Use orchid_solver_params
Use orchid_solver_hydro_jacobian
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
    Type(MhdHydroJacobian1D) :: Js
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
    !> Calculate Jacobian.
    Call mhd_hydro_jacobian_load1D(qs, Js, nx)
    Js%Lambda = Abs(Js%Lambda)
    Js%Lambda(1) = Min(Js%Lambda(1), Abs(qm%Vn - qm%c_snd)) 
    Js%Lambda(3) = Max(Js%Lambda(3), Abs(qp%Vn + qp%c_snd)) 
    !> Calculate Fluxes.
    qs%U = 0.5D0*( qp%U - qm%U )
    qs%U = [ qs%U(1), qs%U(3), qs%U(2) ]
    qs%U = MatMul(Js%OmegaL, qs%U)
    qs%U = MatMul(Js%OmegaR, Js%Lambda*qs%U)
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
    Type(MhdHydroJacobian2D) :: Js
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
    !> Calculate Jacobian.
    Call mhd_hydro_jacobian_load2D(qs, Js, nx, ny)
    Js%Lambda = Abs(Js%Lambda)
    Js%Lambda(1) = Min(Js%Lambda(1), Abs(qm%Vn - qm%c_snd)) 
    Js%Lambda(3) = Max(Js%Lambda(3), Abs(qp%Vn + qp%c_snd)) 
    !> Calculate Fluxes.
    qs%U = 0.5D0*( qp%U - qm%U )
    qs%U = [ qs%U(1), qs%U(3:4), qs%U(2) ]
    qs%U = MatMul(Js%OmegaL, qs%U)
    qs%U = MatMul(Js%OmegaR, Js%Lambda*qs%U)
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
    Type(MhdHydroJacobian3D) :: Js
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
    !> Calculate Jacobian.
    Call mhd_hydro_jacobian_load3D(qs, Js, nx, ny, nz)
    Js%Lambda = Abs(Js%Lambda)
    Js%Lambda(1) = Min(Js%Lambda(1), Abs(qm%Vn - qm%c_snd)) 
    Js%Lambda(3) = Max(Js%Lambda(3), Abs(qp%Vn + qp%c_snd)) 
    !> Calculate Fluxes.
    qs%U = 0.5D0*( qp%U - qm%U )
    qs%U = [ qs%U(1), qs%U(3:5), qs%U(2) ]
    qs%U = MatMul(Js%OmegaL, qs%U)
    qs%U = MatMul(Js%OmegaR, Js%Lambda*qs%U)
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


