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
                                     qp, qm, flux)
    !> Calculate the HLL Fluxes in 1D.
    !> {{{
    Class(MhdHydroFluxHLL), Intent(In) :: This
    Type(MhdHydroVars1D), Intent(In) :: qp, qm
    Real(8), Dimension(1:3), Intent(Out) :: flux
    Real(8), Intent(In) :: nx
    !> }}}
    Real(8) :: sp, sm
    sp = Max(qp%Vn + qp%c_snd, qm%Vn + qm%c_snd)
    sm = Max(qp%Vn - qp%c_snd, qm%Vn - qm%c_snd)
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        flux = ( sp*qm%F - sm*qp%F + sp*sm*( qp%U - qm%U ) )/( sp - sm )
    End If
End Subroutine mhd_hydro_calc_flux_hll1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hll2D(This, &
                                     nx, ny, &
                                     qp, qm, flux)
    !> Calculate the HLL Fluxes in 2D.
    !> {{{ 
    Class(MhdHydroFluxHLL), Intent(In) :: This
    Type(MhdHydroVars2D), Intent(In) :: qp, qm
    Real(8), Dimension(1:4), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny
    !> }}}
    Real(8) :: sp, sm
    sp = Max(qp%Vn + qp%c_snd, qm%Vn + qm%c_snd)
    sm = Max(qp%Vn - qp%c_snd, qm%Vn - qm%c_snd)
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        flux = ( sp*qm%F - sm*qp%F + sp*sm*( qp%U - qm%U ) )/( sp - sm )
    End If
End Subroutine mhd_hydro_calc_flux_hll2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hll3D(This, &
                                     nx, ny, nz, &
                                     qp, qm, flux)
    !> Calculate the HLL Fluxes in 3D.
    !> {{{
    Class(MhdHydroFluxHLL), Intent(In) :: This
    Type(MhdHydroVars3D), Intent(In) :: qp, qm
    Real(8), Dimension(1:5), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Real(8) :: sp, sm
    sp = Max(qp%Vn + qp%c_snd, qm%Vn + qm%c_snd)
    sm = Max(qp%Vn - qp%c_snd, qm%Vn - qm%c_snd)
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        flux = ( sp*qm%F - sm*qp%F + sp*sm*( qp%U - qm%U ) )/( sp - sm )
    End If
End Subroutine mhd_hydro_calc_flux_hll3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hll3D_mhd(This, &
                                         nx, ny, nz, &
                                         qp, qm, flux)
    !> Calculate the HLL Fluxes in 3D for the MHD Equations.
    !> {{{
    Class(MhdHydroFluxHLL), Intent(In) :: This
    Type(MhdHydroVars3DMHD), Intent(In) :: qp, qm
    Real(8), Dimension(1:8), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Real(8) :: sp, sm
    sp = Max(qp%Vn + qp%c_fms, qm%Vn + qm%c_fms)
    sm = Max(qp%Vn - qp%c_fms, qm%Vn - qm%c_fms)
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        flux = ( sp*qm%F - sm*qp%F + sp*sm*( qp%U - qm%U ) )/( sp - sm )
    End If
End Subroutine mhd_hydro_calc_flux_hll3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_hll



Module orchid_solver_hydro_flux_hllc
Use orchid_solver_params
Use orchid_solver_hydro_flux_godunov
Implicit None
Real(8), Parameter :: g2min = 1D-10
Integer, Parameter :: hllc_variation = 0
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
                                      qp, qm, flux)
    !> Calculate the HLLC Fluxes in 1D.
    !> {{{
    Class(MhdHydroFluxHLLC), Intent(In) :: This
    Type(MhdHydroVars1D), Intent(In) :: qp, qm
    Real(8), Dimension(1:3), Intent(Out) :: flux
    Real(8), Intent(In) :: nx
    !> }}}
End Subroutine mhd_hydro_calc_flux_hllc1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc2D(This, &
                                      nx, ny, &
                                      qp, qm, flux)
    !> Calculate the HLLC Fluxes in 2D.
    !> {{{ 
    Class(MhdHydroFluxHLLC), Intent(In) :: This
    Type(MhdHydroVars2D), Intent(In) :: qp, qm
    Real(8), Dimension(1:4), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny
    !> }}}
End Subroutine mhd_hydro_calc_flux_hllc2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc3D(This, &
                                     nx, ny, nz, &
                                     qp, qm, flux)
    !> Calculate the HLLC Fluxes in 3D.
    !> {{{
    Class(MhdHydroFluxHLLC), Intent(In) :: This
    Type(MhdHydroVars3D), Intent(In) :: qp, qm
    Real(8), Dimension(1:5), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Type(MhdHydroVars3D) :: qs
    Real(8), Dimension(1:5) :: ds
    Real(8) :: gp, sp, &
               gm, sm, ps, ss
    !> Calculate Pressure-based Wave speeds.
    qs%Rho = 0.5D0*( qp%Rho + qm%Rho )
    qs%c_snd = 0.5D0*( qp%c_snd + qm%c_snd )
    qs%p = Max(0.5D0*( ( qp%p + qm%p ) - qs%Rho*qs%c_snd*( qp%Vn - qm%Vn ) ), 0.0D0)
    If ( qs%p > qp%p ) Then
        gp = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( qs%p/qp%p - 1.0D0 )
        gp = Sqrt(Max(gp, g2min))
    Else
        gp = 0.0D0
    End If
    If ( qs%p > qm%p ) Then
        gm = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( qs%p/qm%p - 1.0D0 )
        gm = Sqrt(Max(gm, g2min))
    Else
        gm = 0.0D0
    End If
    sp = qp%Vn + qp%c_snd*gp
    sm = qm%Vn - qm%c_snd*gm
    !> Calculate Fluxes.
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        ss = ( ( qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p ) - &
                 qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p )/ &
             ( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        Select Case ( hllc_variation )
        Case ( 0 )
            !> Original HLLC.
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                qs%U = qp%Rho*( sp - qp%Vn )/( sp - ss )*[ 1.0D0, &
                    qp%nrg + ( ss - qp%Vn )*( ss + qp%p/qp%Rho/( sp - ss ) ), &
                    qp%Vx - nx*( qp%Vn - ss ), &
                    qp%Vy - ny*( qp%Vn - ss ), &
                    qp%Vz - nz*( qp%Vn - ss ) ]
                flux = qp%F + sp*( qs%U - qp%U )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                qs%U = qm%Rho*( sm - qm%Vn )/( sm - ss )*[ 1.0D0, &
                    qm%nrg + ( ss - qm%Vn )*( ss + qm%p/qm%Rho/( sm - ss ) ), &
                    qm%Vx - nx*( qm%Vn - ss ), &
                    qm%Vy - ny*( qm%Vn - ss ), &
                    qm%Vz - nz*( qm%Vn - ss ) ]
                flux = qm%F + sm*( qs%U - qm%U )
            End If
        Case ( 1 )
            !> Variation 1 of HLLC.
            ds = [ 0.0D0, ss, nx, ny, nz ]
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                flux = ( ss*( sp*qp%U - qp%F ) + &
                         sp*( qp%p + qp%Rho*( sp - qp%Vn )*( ss - qp%Vn ) )*ds )/ &
                       ( sp - ss )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                flux = ( ss*( sm*qm%U - qm%F ) + &
                         sm*( qm%p + qm%Rho*( sm - qm%Vn )*( ss - qm%Vn ) )*ds )/ &
                       ( sm - ss )
            End If
        Case ( 2 )
            !> Variation 2 of HLLC.
            ds = [ 0.0D0, ss, nx, ny, nz ]
            ps = ( ( qp%p + qp%Rho*( sp - qp%Vn )*( ss - qp%Vn ) ) + &
                   ( qm%p + qm%Rho*( sm - qm%Vn )*( ss - qm%Vn ) ) )*0.5D0
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                flux = ( ss*( sp*qp%U - qp%F ) + sp*ps*ds )/( sp - ss )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                flux = ( ss*( sm*qm%U - qm%F ) + sm*ps*ds )/( sm - ss )
            End If
        End Select
        flux = ( sp*qm%F - sm*qp%F + sp*sm*( qp%U - qm%U ) )/( sp - sm )
    End If
End Subroutine mhd_hydro_calc_flux_hllc3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc3D_mhd(This, &
                                          nx, ny, nz, &
                                          qp, qm, flux)
    !> Calculate the HLLC Fluxes in 3D for the MHD Equations.
    !> {{{
    Class(MhdHydroFluxHLLC), Intent(In) :: This
    Type(MhdHydroVars3DMHD), Intent(In) :: qp, qm
    Real(8), Dimension(1:8), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Type(MhdHydroVars3DMHD) :: qs
    Real(8) :: gp, sp, &
               gm, sm, ps, ss
    !> Calculate Pressure-based Wave speeds.
    qs%Rho = 0.5D0*( qp%Rho + qm%Rho )
    qs%c_fms = 0.5D0*( qp%c_fms + qm%c_fms )
    qs%p = Max(0.5D0*( qp%p + qm%p ) - 0.5D0*qs%Rho*qs%c_fms*( qp%Vn - qm%Vn ), 0.0D0)
    If ( qs%p > qp%p ) Then
        gp = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( qs%p/qp%p - 1.0D0 )
        gp = Sqrt(Max(gp, c2min))
    Else
        gp = 0.0D0
    End If
    If ( qs%p > qm%p ) Then
        gm = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( qs%p/qm%p - 1.0D0 )
        gm = Sqrt(Max(gm, c2min))
    Else
        gm = 0.0D0
    End If
    sp = qp%Vn + qp%c_fms*gp
    sm = qm%Vn + qm%c_fms*gm
    !> Calculate Fluxes.
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        ss = ( ( qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p + 0.25D0/Pi*qp%Bn**2 ) - &
                 qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p + 0.25D0/Pi*qm%Bn**2 )/ &
             ( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
    End If
End Subroutine mhd_hydro_calc_flux_hllc3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_hllc


#if 0
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
#endif

