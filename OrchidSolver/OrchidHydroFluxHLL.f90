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
Integer, Parameter :: hllc_variation_mhd = 0
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
    Type(MhdHydroVars1D) :: qs
    Real(8), Dimension(1:3) :: ds
    Real(8) :: gp, sp, &
               gm, sm, ps, ss
    !> Calculate Pressure-based Wave speeds.
    qs%Rho = 0.5D0*( qp%Rho + qm%Rho )
    qs%c_snd = 0.5D0*( qp%c_snd + qm%c_snd )
    qs%p = Max(0.5D0*( ( qp%p + qm%p ) - qs%Rho*qs%c_snd*( qp%Vn - qm%Vn ) ), 0.0D0)
    If ( qs%p > qp%p ) Then
        gp = 1.0D0 + Gamma_2*( qs%p/qp%p - 1.0D0 )
        gp = Sqrt(Max(gp, g2min))
    Else
        gp = 1.0D0
    End If
    If ( qs%p > qm%p ) Then
        gm = 1.0D0 + Gamma_2*( qs%p/qm%p - 1.0D0 )
        gm = Sqrt(Max(gm, g2min))
    Else
        gm = 1.0D0
    End If
    sp = qp%Vn + qp%c_snd*gp
    sm = qm%Vn - qm%c_snd*gm
    !> Calculate Fluxes.
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        Select Case ( hllc_variation )
        Case ( 0 )
            !> Original HLLC.
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                qs%Rho = qp%Rho*( sp - qp%Vn )/( sp - ss )
                qs%nrg = qp%nrg + ( ss - qp%Vn )*( ss + qp%p/qp%Rho/( sp - ss ) )
                qs%Vx  = qp%Vx - nx*( qp%Vn - ss )
                qs%U(:) = [ qs%rho, &
                            qs%rho*qs%nrg, &
                            qs%rho*qs%Vx ]
                flux = qp%F + sp*( qs%U - qp%U )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                qs%Rho = qm%Rho*( sm - qm%Vn )/( sm - ss )
                qs%nrg = qm%nrg + ( ss - qm%Vn )*( ss + qm%p/qm%Rho/( sm - ss ) )
                qs%Vx  = qm%Vx - nx*( qm%Vn - ss )
                qs%U(:) = [ qs%rho, &
                            qs%rho*qs%nrg, &
                            qs%rho*qs%Vx ]
                flux = qm%F + sm*( qs%U - qm%U )
            End If
        Case ( 1 )
            !> Variation 1 of HLLC.
            ds = [ 0.0D0, ss, nx ]
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                flux = ss*( sp*qp%U - qp%F )
                flux = sp*( qp%p + qp%Rho*( sp - qp%Vn )*( ss - qp%Vn ) )*ds + flux
                flux = flux/( sp - ss )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                flux = ss*( sm*qm%U - qm%F )
                flux = sm*( qm%p + qm%Rho*( sm - qm%Vn )*( ss - qm%Vn ) )*ds + flux
                flux = flux/( sm - ss )
            End If
        Case ( 2 )
            !> Variation 2 of HLLC.
            ds = [ 0.0D0, ss, nx ]
            ps = qp%p + qp%Rho*( sp - qp%Vn )*( ss - qp%Vn )
            ps = qm%p + qm%Rho*( sm - qm%Vn )*( ss - qm%Vn ) + ps
            ps = 0.5D0*ps
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                flux = ( ss*( sp*qp%U - qp%F ) + sp*ps*ds )/( sp - ss )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                flux = ( ss*( sm*qm%U - qm%F ) + sm*ps*ds )/( sm - ss )
            End If
        End Select
    End If
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
    Type(MhdHydroVars2D) :: qs
    Real(8), Dimension(1:4) :: ds
    Real(8) :: gp, sp, &
               gm, sm, ps, ss
    !> Calculate Pressure-based Wave speeds.
    qs%Rho = 0.5D0*( qp%Rho + qm%Rho )
    qs%c_snd = 0.5D0*( qp%c_snd + qm%c_snd )
    qs%p = 0.5D0*( ( qp%p + qm%p ) - qs%Rho*qs%c_snd*( qp%Vn - qm%Vn ) )
    qs%p = Max(qs%p, 0.0D0)
    If ( qs%p > qp%p ) Then
        gp = 1.0D0 + Gamma_2*( qs%p/qp%p - 1.0D0 )
        gp = Sqrt(Max(gp, g2min))
    Else
        gp = 1.0D0
    End If
    If ( qs%p > qm%p ) Then
        gm = 1.0D0 + Gamma_2*( qs%p/qm%p - 1.0D0 )
        gm = Sqrt(Max(gm, g2min))
    Else
        gm = 1.0D0
    End If
    sp = qp%Vn + qp%c_snd*gp
    sm = qm%Vn - qm%c_snd*gm
    !> Calculate Fluxes.
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        Select Case ( hllc_variation )
        Case ( 0 )
            !> Original HLLC.
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                qs%Rho = qp%Rho*( sp - qp%Vn )/( sp - ss )
                qs%nrg = qp%nrg + ( ss - qp%Vn )*( ss + qp%p/qp%Rho/( sp - ss ) )
                qs%Vx  = qp%Vx - nx*( qp%Vn - ss )
                qs%Vy  = qp%Vy - ny*( qp%Vn - ss )
                qs%U(:) = [ qs%rho, &
                            qs%rho*qs%nrg, &
                            qs%rho*qs%Vx, qs%rho*qs%Vy ]
                flux = qp%F + sp*( qs%U - qp%U )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                qs%Rho = qm%Rho*( sm - qm%Vn )/( sm - ss )
                qs%nrg = qm%nrg + ( ss - qm%Vn )*( ss + qm%p/qm%Rho/( sm - ss ) )
                qs%Vx  = qm%Vx - nx*( qm%Vn - ss )
                qs%Vy  = qm%Vy - ny*( qm%Vn - ss )
                qs%U(:) = [ qs%rho, &
                            qs%rho*qs%nrg, &
                            qs%rho*qs%Vx, qs%rho*qs%Vy ]
                flux = qm%F + sm*( qs%U - qm%U )
            End If
        Case ( 1 )
            !> Variation 1 of HLLC.
            ds = [ 0.0D0, ss, nx, ny ]
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                flux = ss*( sp*qp%U - qp%F )
                flux = sp*( qp%p + qp%Rho*( sp - qp%Vn )*( ss - qp%Vn ) )*ds + flux
                flux = flux/( sp - ss )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                flux = ss*( sm*qm%U - qm%F )
                flux = sm*( qm%p + qm%Rho*( sm - qm%Vn )*( ss - qm%Vn ) )*ds + flux
                flux = flux/( sm - ss )
            End If
        Case ( 2 )
            !> Variation 2 of HLLC.
            ds = [ 0.0D0, ss, nx, ny ]
            ps = qp%p + qp%Rho*( sp - qp%Vn )*( ss - qp%Vn )
            ps = qm%p + qm%Rho*( sm - qm%Vn )*( ss - qm%Vn ) + ps
            ps = 0.5D0*ps
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                flux = ( ss*( sp*qp%U - qp%F ) + sp*ps*ds )/( sp - ss )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                flux = ( ss*( sm*qm%U - qm%F ) + sm*ps*ds )/( sm - ss )
            End If
        End Select
    End If
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
    qs%p = 0.5D0*( ( qp%p + qm%p ) - qs%Rho*qs%c_snd*( qp%Vn - qm%Vn ) )
    qs%p = Max(qs%p, 0.0D0)
    If ( qs%p > qp%p ) Then
        gp = 1.0D0 + Gamma_2*( qs%p/qp%p - 1.0D0 )
        gp = Sqrt(Max(gp, g2min))
    Else
        gp = 1.0D0
    End If
    If ( qs%p > qm%p ) Then
        gm = 1.0D0 + Gamma_2*( qs%p/qm%p - 1.0D0 )
        gm = Sqrt(Max(gm, g2min))
    Else
        gm = 1.0D0
    End If
    sp = qp%Vn + qp%c_snd*gp
    sm = qm%Vn - qm%c_snd*gm
    !> Calculate Fluxes.
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        Select Case ( hllc_variation )
        Case ( 0 )
            !> Original HLLC.
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                qs%Rho = qp%Rho*( sp - qp%Vn )/( sp - ss )
                qs%nrg = qp%nrg + ( ss - qp%Vn )*( ss + qp%p/qp%Rho/( sp - ss ) )
                qs%Vx  = qp%Vx - nx*( qp%Vn - ss )
                qs%Vy  = qp%Vy - ny*( qp%Vn - ss )
                qs%Vz  = qp%Vz - nz*( qp%Vn - ss )
                qs%U(:) = [ qs%rho, &
                            qs%rho*qs%nrg, &
                            qs%rho*qs%Vx, qs%rho*qs%Vy, qs%rho*qs%Vz ]
                flux = qp%F + sp*( qs%U - qp%U )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                qs%Rho = qm%Rho*( sm - qm%Vn )/( sm - ss )
                qs%nrg = qm%nrg + ( ss - qm%Vn )*( ss + qm%p/qm%Rho/( sm - ss ) )
                qs%Vx  = qm%Vx - nx*( qm%Vn - ss )
                qs%Vy  = qm%Vy - ny*( qm%Vn - ss )
                qs%Vz  = qm%Vz - nz*( qm%Vn - ss )
                qs%U(:) = [ qs%rho, &
                            qs%rho*qs%nrg, &
                            qs%rho*qs%Vx, qs%rho*qs%Vy, qs%rho*qs%Vz ]
                flux = qm%F + sm*( qs%U - qm%U )
            End If
        Case ( 1 )
            !> Variation 1 of HLLC.
            ds = [ 0.0D0, ss, nx, ny, nz ]
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                flux = ss*( sp*qp%U - qp%F )
                flux = sp*( qp%p + qp%Rho*( sp - qp%Vn )*( ss - qp%Vn ) )*ds + flux
                flux = flux/( sp - ss )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                flux = ss*( sm*qm%U - qm%F )
                flux = sm*( qm%p + qm%Rho*( sm - qm%Vn )*( ss - qm%Vn ) )*ds + flux
                flux = flux/( sm - ss )
            End If
        Case ( 2 )
            !> Variation 2 of HLLC.
            ds = [ 0.0D0, ss, nx, ny, nz ]
            ps = qp%p + qp%Rho*( sp - qp%Vn )*( ss - qp%Vn )
            ps = qm%p + qm%Rho*( sm - qm%Vn )*( ss - qm%Vn ) + ps
            ps = 0.5D0*ps
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                flux = ( ss*( sp*qp%U - qp%F ) + sp*ps*ds )/( sp - ss )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                flux = ( ss*( sm*qm%U - qm%F ) + sm*ps*ds )/( sm - ss )
            End If
        End Select
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
    qs%p = 0.5D0*( ( qp%p + qm%p ) - qs%Rho*qs%c_fms*( qp%Vn - qm%Vn ) )
    qs%p = Max(qs%p, 0.0D0)
    If ( qs%p > qp%p ) Then
        gp = 1.0D0 + Gamma_2*( qs%p/qp%p - 1.0D0 )
        gp = Sqrt(Max(gp, c2min))
    Else
        gp = 1.0D0
    End If
    If ( qs%p > qm%p ) Then
        gm = 1.0D0 + Gamma_2*( qs%p/qm%p - 1.0D0 )
        gm = Sqrt(Max(gm, c2min))
    Else
        gm = 1.0D0
    End If
    sp = qp%Vn + qp%c_fms*gp
    sm = qm%Vn - qm%c_fms*gm
    !> Calculate Fluxes.
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p_tot
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p_tot - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        Select Case ( hllc_variation_mhd )
        Case ( 0 )
            !> MHD HLLC by Shengtai Li (2003).
            qs%U = ( ( sp*qp%U - qp%F ) - ( sm*qm%U - qm%F ) )/( sp - sm )
            qs%Rho = qs%U(1)
            qs%Vx  = qs%U(3)/qs%Rho
            qs%Vy  = qs%U(4)/qs%Rho
            qs%Vz  = qs%U(5)/qs%Rho
            qs%Bx  = qs%U(6)*0.5D0/Sqrt(Pi)
            qs%By  = qs%U(7)*0.5D0/Sqrt(Pi)
            qs%Bz  = qs%U(8)*0.5D0/Sqrt(Pi)
            qs%Bn  = qs%Bx*nx + qs%By*ny + qs%Bz*nz
            qs%BV  = qs%Bx*qs%Vx + qs%By*qs%Vy + qs%Bz*qs%Vz
            If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                !> Select Fp*.
                ps = qp%p_tot + qp%Rho*( sp - qp%Vn )*( ss - qp%Vn )
                ps = qs%Bn**2 - qp%Bn**2 + ps
                gp = qp%Rho*( sp - qp%Vn )
                qs%Rho = gp/( sp - ss )
                qs%nrg = qp%nrg + ( ( ps*ss - qp%p_tot*qp%Vn ) - ( qs%Bn*qs%BV - qp%Bn*qp%BV ) )/gp
                qs%Vx  = qp%Vx - nx*( qp%Vn - ss ) - ( qs%Bn*qs%Bx - qp%Bn*qp%Bx )/gp
                qs%Vy  = qp%Vy - ny*( qp%Vn - ss ) - ( qs%Bn*qs%By - qp%Bn*qp%By )/gp
                qs%Vz  = qp%Vz - nz*( qp%Vn - ss ) - ( qs%Bn*qs%Bz - qp%Bn*qp%Bz )/gp
                qs%U(:) = [ qs%Rho, &
                            qs%Rho*qs%nrg, &
                            qs%Rho*qs%Vx, qs%Rho*qs%Vy, qs%Rho*qs%Vz, & 
                            [ qs%Bx, qs%By, qs%Bz ]*2.0D0*Sqrt(Pi) ]
                flux = qp%F + sp*( qs%U - qp%U )
            Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                !> Select Fm*.
                ps = qm%p_tot + qm%Rho*( sp - qm%Vn )*( ss - qm%Vn )
                ps = qs%Bn**2 - qm%Bn**2 + ps
                gm = qm%Rho*( sm - qm%Vn )
                qs%Rho = gm/( sm - ss )
                qs%nrg = qm%nrg + ( ( ps*ss - qm%p_tot*qm%Vn ) - ( qs%Bn*qs%BV - qm%Bn*qm%BV ) )/gm
                qs%Vx  = qm%Vx - nx*( qm%Vn - ss ) - ( qs%Bn*qs%Bx - qm%Bn*qm%Bx )/gm
                qs%Vy  = qm%Vy - ny*( qm%Vn - ss ) - ( qs%Bn*qs%By - qm%Bn*qm%By )/gm
                qs%Vz  = qm%Vz - nz*( qm%Vn - ss ) - ( qs%Bn*qs%Bz - qm%Bn*qm%Bz )/gm
                qs%U(:) = [ qs%Rho, &
                            qs%Rho*qs%nrg, &
                            qs%Rho*qs%Vx, qs%Rho*qs%Vy, qs%Rho*qs%Vz, & 
                            [ qs%Bx, qs%By, qs%Bz ]*2.0D0*Sqrt(Pi) ]
                flux = qm%F + sm*( qs%U - qm%U )
            End If
        End Select
    End If
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
                                          qp, qm, flux)
    !> Calculate the HLLD Fluxes in 3D for the MHD Equations.
    !> {{{
    Class(MhdHydroFluxHLLD), Intent(In) :: This
    Type(MhdHydroVars3DMHD), Intent(In) :: qp, qm
    Real(8), Dimension(1:8), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Type(MhdHydroVars3DMHD) :: qs, qps, qms
    Real(8) :: gp, sp, ssp, rp, &
               gm, sm, ssm, rm, ps, ss
    !> Calculate Pressure-based Wave speeds.
    qs%Rho = 0.5D0*( qp%Rho + qm%Rho )
    qs%c_fms = 0.5D0*( qp%c_fms + qm%c_fms )
    qs%p = Max(0.5D0*( qp%p + qm%p ) - 0.5D0*qs%Rho*qs%c_fms*( qp%Vn - qm%Vn ), 0.0D0)
    If ( qs%p > qp%p ) Then
        gp = 1.0D0 + Gamma_2*( qs%p/qp%p - 1.0D0 )
        gp = Sqrt(Max(gp, c2min))
    Else
        gp = 1.0D0
    End If
    If ( qs%p > qm%p ) Then
        gm = 1.0D0 + Gamma_2*( qs%p/qm%p - 1.0D0 )
        gm = Sqrt(Max(gm, c2min))
    Else
        gm = 1.0D0
    End If
    sp = qp%Vn + qp%c_fms*gp
    sm = qm%Vn - qm%c_fms*gm
    !> Calculate Fluxes.
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        !> @todo 
        !> This Solver works only in 1D currently.
        !> We may use a dirty hack: go to the TBN space, like in Roe.      
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p_tot
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p_tot - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        ps = qm%Rho*qp%p_tot*( sm - qm%Vn )
        ps = qp%Rho*qm%p_tot*( sp - qp%Vn ) - ps
        ps = qp%Rho*qm%Rho*( sp - qp%Vn )*( sm - qm%Vn )*( qp%Vn - qm%Vn ) + ps
        ps = ps/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        If ( .TRUE. ) Then
            !> Calculate Fp*.
            qps%Rho = qp%Rho*( sp - qp%Vn )/( sp - ss )
            gp = ( qp%Rho*( sp - qp%Vn )*( sp - ss ) - qp%Bn**2 )
            gp = ( ss - qp%Vn )/gp
            qps%Vx  = ss    !> @todo Fix me!
            qps%Vy  = qp%Vy - qp%By*qp%Bn*gp
            qps%Vz  = qp%Vz - qp%Bz*qp%Bn*gp
            gp = ( qp%Rho*( sp - qp%Vn )*( sp - ss ) - qp%Bn**2 )
            gp = ( qp%Rho*( sp - qp%Vn )**2 - qp%Bn**2 )/gp
            qps%Bx  = qp%Bx     !> @todo Fix me!
            qps%By  = qp%By*gp
            qps%Bz  = qp%Bz*gp
            qps%BV  = qps%Bx*qps%Vx + qps%By*qps%Vy + qps%Bz*qps%Vz
            qps%nrg = qp%Bn*( qps%BV - qp%BV )
            qps%nrg = qp%Rho*qp%nrg*( sp - qp%Vn ) + ( ps*ss - qp%p_tot*qp%Vn ) - qps%nrg
            qps%nrg = qps%nrg/( sp - ss )
            qps%U(:) = [ qps%Rho, qps%nrg, &
                        qps%Rho*qps%Vx, qps%Rho*qps%Vy, qps%Rho*qps%Vz, & 
                        [ qps%Bx, qps%By, qps%Bz ]*2.0D0*Sqrt(Pi) ]
        End If
        If ( .TRUE. ) Then
            !> Calculate Fm*.
            qms%Rho = qm%Rho*( sm - qm%Vn )/( sm - ss )
            gm = ( qm%Rho*( sm - qm%Vn )*( sm - ss ) - qm%Bn**2 )
            gm = ( ss - qm%Vn )/gm
            qms%Vx  = ss    !> @todo Fix me!
            qms%Vy  = qm%Vy - qm%By*qm%Bn*gm
            qms%Vz  = qm%Vz - qm%Bz*qm%Bn*gm
            gm = ( qm%Rho*( sm - qm%Vn )*( sm - ss ) - qm%Bn**2 )
            gm = ( qm%Rho*( sm - qm%Vn )**2 - qm%Bn**2 )/gm
            qms%Bx  = qm%Bx     !> @todo Fix me!
            qms%By  = qm%By*gm
            qms%Bz  = qm%Bz*gm
            qms%BV  = qms%Bx*qms%Vx + qms%By*qms%Vy + qms%Bz*qms%Vz
            qms%nrg = qm%Bn*( qms%BV - qm%BV )
            qms%nrg = qm%Rho*qm%nrg*( sm - qm%Vn ) + ( ps*ss - qm%p_tot*qm%Vn ) - qms%nrg
            qms%nrg = qms%nrg/( sm - ss )
            qms%U(:) = [ qms%Rho, qms%nrg, &
                        qms%Rho*qms%Vx, qms%Rho*qms%Vy, qms%Rho*qms%Vz, & 
                        [ qms%Bx, qms%By, qms%Bz ]*2.0D0*Sqrt(Pi) ]
        End If
        If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
            ssp = ss + Abs(qp%Bn)/Sqrt(qps%Rho)
            If ( ssp <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                !> Select Fp*.
                flux = qp%F + sp*( qps%U - qp%U )
            Else If ( ss <= 0.0D0 .AND. 0.0D0 <= ssp ) Then
                !> Select Fp**.
                rp = Sqrt(qps%Rho)
                rm = Sqrt(qms%Rho)
                qs%Rho = qps%Rho
                qs%Vx  = ss    !> @todo Fix me!
                qs%Vy  = ( rp*qps%Vy + rm*qms%Vy + Sign(1.0D0, qp%Bn)*( qps%By - qms%By ) )/( rp + rm )
                qs%Vz  = ( rp*qps%Vz + rm*qms%Vz + Sign(1.0D0, qp%Bn)*( qps%Bz - qms%Bz ) )/( rp + rm )
                qs%Bx  = qp%Bx
                qs%By  = ( rp*qps%By + rm*qms%By + Sign(1.0D0, qp%Bn)*( qps%Vy - qms%Vy )*rp*rm )/( rp + rm )
                qs%Bz  = ( rp*qps%Bz + rm*qms%Bz + Sign(1.0D0, qp%Bn)*( qps%Vz - qms%Vz )*rp*rm )/( rp + rm )
                qs%BV  = qs%Bx*qs%Vx + qs%By*qs%Vy + qs%Bz*qs%Vz
                qs%nrg = qps%nrg + rp*Sign(1.0D0, qp%Bn)*( qps%BV - qs%BV )
                qs%U(:) = [ qs%Rho, qs%nrg, &
                            qs%Rho*qs%Vx, qs%Rho*qs%Vy, qs%Rho*qs%Vz, & 
                            [ qs%Bx, qs%By, qs%Bz ]*2.0D0*Sqrt(Pi) ]
                flux = qp%F + ssp*qs%U - ( ssp - sp )*qps%U - sp*qp%U
            End If
        Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
            ssm = ss - Abs(qm%Bn)/Sqrt(qms%Rho)
            If ( sm <= 0.0D0 .AND. 0.0D0 <= ssm ) Then
                !> Select Fm*.
                flux = qm%F + sm*( qms%U - qm%U )
            Else If ( ssm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                !> Select Fm**.
                rp = Sqrt(qps%Rho)
                rm = Sqrt(qms%Rho)
                qs%Rho = qms%Rho
                qs%Vx  = ss    !> @todo Fix me!
                qs%Vy  = ( rp*qps%Vy + rm*qms%Vy + Sign(1.0D0, qp%Bn)*( qps%By - qms%By ) )/( rp + rm )
                qs%Vz  = ( rp*qps%Vz + rm*qms%Vz + Sign(1.0D0, qp%Bn)*( qps%Bz - qms%Bz ) )/( rp + rm )
                qs%Bx  = qm%Bx
                qs%By  = ( rp*qps%By + rm*qms%By + Sign(1.0D0, qp%Bn)*( qps%Vy - qms%Vy )*rp*rm )/( rp + rm )
                qs%Bz  = ( rp*qps%Bz + rm*qms%Bz + Sign(1.0D0, qp%Bn)*( qps%Vz - qms%Vz )*rp*rm )/( rp + rm )
                qs%BV  = qs%Bx*qs%Vx + qs%By*qs%Vy + qs%Bz*qs%Vz
                qs%nrg = qms%nrg - rm*Sign(1.0D0, qp%Bn)*( qms%BV - qs%BV )
                qs%U(:) = [ qs%Rho, qs%nrg, &
                            qs%Rho*qs%Vx, qs%Rho*qs%Vy, qs%Rho*qs%Vz, & 
                            [ qs%Bx, qs%By, qs%Bz ]*2.0D0*Sqrt(Pi) ]
                flux = qm%F + ssm*qs%U - ( ssm - sm )*qms%U - sm*qm%U
            End If
        End If
    End If
End Subroutine mhd_hydro_calc_flux_hlld3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_hlld

