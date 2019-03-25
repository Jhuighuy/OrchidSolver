!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_flux_hlle
Use orchid_solver_params
Use orchid_solver_hydro_flux_godunov
Implicit None
Type, Extends(MhdHydroFlux) :: MhdHydroFluxHLLE
    Contains
    Procedure, Public, NoPass, Non_Overridable :: &
        calc1D => mhd_hydro_calc_flux_hlle1D, &
        calc2D => mhd_hydro_calc_flux_hlle2D, &
        calc3D => mhd_hydro_calc_flux_hlle3D, &
        calc3D_mhd => mhd_hydro_calc_flux_hlle3D_mhd
End Type MhdHydroFluxHLLE
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_wave_speed_hlle(qp, qm, sp, sm)
    !> Calculate the Wave Speeds.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars), Intent(In) :: qp, qm
    Real(8), Intent(Out) :: sp, sm
    !> }}}
    Real(8) :: rp, rm, d, n
    !> [1], Eq. (10.53-10.54).
    rp = Sqrt(qp%Rho)
    rm = Sqrt(qm%Rho)
    n = 0.5D0*rp*rm/( rp + rm )**2
    d = ( rp*qp%c2snd + rm*qm%c2snd )/( rp + rm ) + n*( qp%Vn - qm%Vn )**2
    d = Sqrt(d)
    !> [1], Eq. (10.52).
    sp = 0.5D0*( qp%Vn + qm%Vn ) + d
    sm = 0.5D0*( qp%Vn + qm%Vn ) - d
End Subroutine mhd_hydro_calc_wave_speed_hlle
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hlle1D(nx, &
                                      qp, qm, flux)
    !> Calculate the HLLE Fluxes in 1D.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars1D), Intent(In) :: qp, qm
    Real(8), Dimension(1:3), Intent(Out) :: flux
    Real(8), Intent(In) :: nx
    !> }}}
    Real(8) :: sp, sm
    !> Calculate Wave speeds.
    Call mhd_hydro_calc_wave_speed_hlle(qp%MhdHydroVars, &
                                        qm%MhdHydroVars, sp, sm)
    !> Calculate Fluxes: [1], Eq. (10.20-10.21).
    If ( sp < 0.0D0 ) Then
        flux = qp%F
    Else If ( sm > 0.0D0 ) Then
        flux = qm%F
    Else
        flux = ( sp*qm%F - sm*qp%F + sp*sm*( qp%U - qm%U ) )/( sp - sm )
    End If
End Subroutine mhd_hydro_calc_flux_hlle1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hlle2D(nx, ny, &
                                      qp, qm, flux)
    !> Calculate the HLLE Fluxes in 2D.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{ 
    Type(MhdHydroVars2D), Intent(In) :: qp, qm
    Real(8), Dimension(1:4), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny
    !> }}}
    Real(8) :: sp, sm
    !> Calculate Wave speeds.
    Call mhd_hydro_calc_wave_speed_hlle(qp%MhdHydroVars, &
                                        qm%MhdHydroVars, sp, sm)
    !> Calculate Fluxes: [1], Eq. (10.20-10.21).
    If ( sp < 0.0D0 ) Then
        flux = qp%F
    Else If ( sm > 0.0D0 ) Then
        flux = qm%F
    Else
        flux = ( sp*qm%F - sm*qp%F + sp*sm*( qp%U - qm%U ) )/( sp - sm )
    End If
End Subroutine mhd_hydro_calc_flux_hlle2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hlle3D(nx, ny, nz, &
                                      qp, qm, flux)
    !> Calculate the HLLE Fluxes in 3D.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars3D), Intent(In) :: qp, qm
    Real(8), Dimension(1:5), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Real(8) :: sp, sm
    !> Calculate Wave speeds.
    Call mhd_hydro_calc_wave_speed_hlle(qp%MhdHydroVars, &
                                        qm%MhdHydroVars, sp, sm)
    !> Calculate Fluxes: [1], Eq. (10.20-10.21).
    If ( sp <= 0.0D0 ) Then
        flux = qp%F
    Else If ( sm >= 0.0D0 ) Then
        flux = qm%F
    Else
        flux = ( sp*qm%F - sm*qp%F + sp*sm*( qp%U - qm%U ) )/( sp - sm )
    End If
End Subroutine mhd_hydro_calc_flux_hlle3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hlle3D_mhd(nx, ny, nz, &
                                          qp, qm, flux)
    !> Calculate the HLLE Fluxes in 3D for the MHD Equations.
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars3DMHD), Intent(In) :: qp, qm
    Real(8), Dimension(1:8), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Type(MhdHydroVars2D) :: qs
    Real(8) :: sp, sm
    !> Calculate Wave speeds.
    sp = Max(qp%Vn + qp%c_fms, qm%Vn + qm%c_fms)
    sm = Max(qp%Vn - qp%c_fms, qm%Vn - qm%c_fms)
    !> Calculate Fluxes: [1], Eq. (10.20-10.21).
    If ( sp < 0.0D0 ) Then
        flux = qp%F
    Else If ( sm > 0.0D0 ) Then
        flux = qm%F
    Else
        flux = ( sp*qm%F - sm*qp%F + sp*sm*( qp%U - qm%U ) )/( sp - sm )
    End If
End Subroutine mhd_hydro_calc_flux_hlle3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_hlle



Module orchid_solver_hydro_flux_hllc
Use orchid_solver_params
Use orchid_solver_hydro_flux_godunov
Implicit None
Integer :: hllc_variation = 0
Type, Extends(MhdHydroFlux) :: MhdHydroFluxHLLC
    Contains
    Procedure, Public, NoPass, Non_Overridable :: &
        calc1D => mhd_hydro_calc_flux_hllc1D, &
        calc2D => mhd_hydro_calc_flux_hllc2D, &
        calc3D => mhd_hydro_calc_flux_hllc3D, &
        calc3D_mhd => mhd_hydro_calc_flux_hllc3D_mhd
End Type MhdHydroFluxHLLC
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_wave_speed_hllc(qp, qm, sp, sm)
    !> Calculate Pressure-based Wave speeds.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars), Intent(In) :: qp, qm
    Real(8), Intent(Out) :: sp, sm
    !> }}}
    Real(8) :: Rho, c_snd, p
    Real(8) :: gp, gm
    !> [1], Eq. (10.61-10.62).
    Rho = 0.5D0*( qp%Rho + qm%Rho )
    c_snd = 0.5D0*( qp%c_snd + qm%c_snd )
    p = Max(0.5D0*( ( qp%p + qm%p ) - Rho*c_snd*( qp%Vn - qm%Vn ) ), 0.0D0)
    !> [1], Eq. (10.60).
    If ( p > qp%p ) Then
        gp = 1.0D0 + Gamma_2*( p/qp%p - 1.0D0 )
        gp = Sqrt(gp)
    Else
        gp = 1.0D0
    End If
    If ( p > qm%p ) Then
        gm = 1.0D0 + Gamma_2*( p/qm%p - 1.0D0 )
        gm = Sqrt(gm)
    Else
        gm = 1.0D0
    End If
    !> [1], Eq. (10.59).
    sp = qp%Vn + qp%c_snd*gp
    sm = qm%Vn - qm%c_snd*gm
    End Subroutine mhd_hydro_calc_wave_speed_hllc
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc1D(nx, &
                                      qp, qm, flux)
    !> Calculate the HLLC Fluxes in 1D.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars1D), Intent(In) :: qp, qm
    Real(8), Dimension(1:3), Intent(Out) :: flux
    Real(8), Intent(In) :: nx
    !> }}}
    Type(MhdHydroVars1D) :: qs
    Real(8), Dimension(1:3) :: ds
    Real(8) :: sp, &
               sm, ps, ss
    !> Calculate Wave speeds.
    Call mhd_hydro_calc_wave_speed_hllc(qp%MhdHydroVars, &
                                        qm%MhdHydroVars, sp, sm)
    !> Calculate Fluxes: [1], Eq. (10.26).
    If ( sp < 0.0D0 ) Then
        flux = qp%F
    Else If ( sm > 0.0D0 ) Then
        flux = qm%F
    Else
        !> [1], Eq. (10.37).
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        Select Case ( hllc_variation )
        Case ( 0 )
            !> Original HLLC: [1], Eq. (10.38), (10.39).
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
            !> Variation 1 of HLLC: [1], Eq. (10.40), (10.41).
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
            !> Variation 2 of HLLC: [1], Eq. (10.40), (10.42), (10.44).
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
Subroutine mhd_hydro_calc_flux_hllc2D(nx, ny, &
                                      qp, qm, flux)
    !> Calculate the HLLC Fluxes in 2D.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{ 
    Type(MhdHydroVars2D), Intent(In) :: qp, qm
    Real(8), Dimension(1:4), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny
    !> }}}
    Type(MhdHydroVars2D) :: qs
    Real(8), Dimension(1:4) :: ds
    Real(8) :: sp, &
               sm, ps, ss
    !> Calculate Wave speeds.
    Call mhd_hydro_calc_wave_speed_hllc(qp%MhdHydroVars, &
                                        qm%MhdHydroVars, sp, sm)
    !> Calculate Fluxes: [1], Eq. (10.26).
    If ( sp < 0.0D0 ) Then
        flux = qp%F
    Else If ( sm > 0.0D0 ) Then
        flux = qm%F
    Else
        !> [1], Eq. (10.37).
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        Select Case ( hllc_variation )
        Case ( 0 )
            !> Original HLLC: [1], Eq. (10.38), (10.39).
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
            !> Variation 1 of HLLC: [1], Eq. (10.40), (10.41).
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
            !> Variation 2 of HLLC: [1], Eq. (10.40), (10.42), (10.44).
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
Subroutine mhd_hydro_calc_flux_hllc3D(nx, ny, nz, &
                                      qp, qm, flux)
    !> Calculate the HLLC Fluxes in 3D.
    !> REFERENCES:
    !> [1] Eleuterio F. Toro,
    !>     "Riemann Solvers and Numerical Methods
    !>      for Fluid Dynamics" (Third Edition, 2009).
    !> {{{
    Type(MhdHydroVars3D), Intent(In) :: qp, qm
    Real(8), Dimension(1:5), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Type(MhdHydroVars3D) :: qs
    Real(8), Dimension(1:5) :: ds
    Real(8) :: sp, &
               sm, ps, ss
    !> Calculate Wave speeds.
    Call mhd_hydro_calc_wave_speed_hllc(qp%MhdHydroVars, &
                                        qm%MhdHydroVars, sp, sm)
    !> Calculate Fluxes: [1], Eq. (10.26).
    If ( sp < 0.0D0 ) Then
        flux = qp%F
    Else If ( sm > 0.0D0 ) Then
        flux = qm%F
    Else
        !> [1], Eq. (10.37).
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        Select Case ( hllc_variation )
        Case ( 0 )
            !> Original HLLC: [1], Eq. (10.38), (10.39).
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
            !> Variation 1 of HLLC: [1], Eq. (10.40), (10.41).
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
            !> Variation 2 of HLLC: [1], Eq. (10.40), (10.42), (10.44).
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
Subroutine mhd_hydro_calc_flux_hllc3D_mhd(nx, ny, nz, &
                                          qp, qm, flux)
    !> Calculate the HLLC Fluxes in 3D for the MHD Equations.
    !> REFERENCES:
    !> [1] Shengtai Li, 
    !>     "An HLLC Riemann Solver for Magnetohydrodynamics" (2003).
    !> [2] Takahiro Miyoshi & Kanya Kusano,
    !>     "A multi-state HLL approximate Riemann solver for
    !>      ideal Magnetohydrodynamics" (2005).
    !> {{{
    Type(MhdHydroVars3DMHD), Intent(In) :: qp, qm
    Real(8), Dimension(1:8), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Type(MhdHydroVars3DMHD) :: qs
    Real(8) :: gp, sp, &
               gm, sm, ps, ss
    !> Calculate Wave speeds: [2], Eq. (67).
    sp = Max(qp%Vn, qm%Vn) + Max(qp%c_fms, qm%c_fms)
    sm = Min(qp%Vn, qm%Vn) - Max(qp%c_fms, qm%c_fms)
    !> Calculate Fluxes: [1], Eq (32).
    If ( sp < 0.0D0 ) Then
        flux = qp%F
    Else If ( sm > 0.0D0 ) Then
        flux = qm%F
    Else
        !> [1], Eq. (14).
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p_tot
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p_tot - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        !> Calculate HLL average state: [1], Eq (2), (28), (33).
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
            !> Select Fp*: [1], Eq (17), (19-23), (31-32).
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
            !> Select Fm*: [1], Eq (17), (19-23), (31-32).
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
Type, Extends(MhdHydroFlux) :: MhdHydroFluxHLLD
    Contains
    Procedure, Public, NoPass, Non_Overridable :: &
        calc3D_mhd => mhd_hydro_calc_flux_hlld3D_mhd
End Type MhdHydroFluxHLLD
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hlld3D_mhd(nx, ny, nz, &
                                          qp, qm, flux)
    !> Calculate the HLLD Fluxes in 3D for the MHD Equations.
    !> REFERENCES:
    !> [1] Shengtai Li, 
    !>     "An HLLC Riemann Solver for Magnetohydrodynamics" (2003).
    !> [2] Takahiro Miyoshi & Kanya Kusano,
    !>     "A multi-state HLL approximate Riemann solver for
    !>      ideal Magnetohydrodynamics" (2005).
    !> {{{
    Type(MhdHydroVars3DMHD), Intent(In) :: qp, qm
    Real(8), Dimension(1:8), Intent(Out) :: flux
    Real(8), Intent(In) :: nx, ny, nz
    !> }}}
    Type(MhdHydroVars3DMHD) :: qs, qps, qms
    Real(8) :: gp, sp, dp, ssp, rp, &
               gm, sm, dm, ssm, rm, ir, ps, ss, sgn, sgnr, ds
    !> Calculate Wave speeds: [2], Eq. (67).
    sp = Max(qp%Vn, qm%Vn) + Max(qp%c_fms, qm%c_fms)
    sm = Min(qp%Vn, qm%Vn) - Max(qp%c_fms, qm%c_fms)
    !> Calculate Fluxes: [2], Eq. (66).
    If ( sp < 0.0D0 ) Then
        flux = qp%F
    Else If ( sm > 0.0D0 ) Then
        flux = qm%F
    Else
        !> [2], Eq (38).
        ss = qm%Rho*qm%Vn*( sm - qm%Vn ) - qm%p_tot
        ss = qp%Rho*qp%Vn*( sp - qp%Vn ) - qp%p_tot - ss
        ss = ss/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        !> [2], Eq (41).
        ps = qm%Rho*qp%p_tot*( sm - qm%Vn )
        ps = qp%Rho*qm%p_tot*( sp - qp%Vn ) - ps
        ps = qp%Rho*qm%Rho*( sp - qp%Vn )*( sm - qm%Vn )*( qp%Vn - qm%Vn ) + ps
        ps = ps/( qp%Rho*( sp - qp%Vn ) - qm%Rho*( sm - qm%Vn ) )
        !> Calculate Fp*: [2], Eq (43-47).
        gp = ( qp%Rho*( sp - qp%Vn )*( sp - ss ) - qp%Bn**2 )
        If ( Abs(gp) > 0.0D0  ) Then
            dp = ( qp%Rho*( sp - qp%Vn )**2 - qp%Bn**2 )/gp
            gp = ( ss - qp%Vn )/gp
            qps%Vx = qp%Vx - nx*( qp%Vn - ss ) - qp%Bn*gp*( qp%Bx - qp%Bn*nx  )
            qps%Vy = qp%Vy - ny*( qp%Vn - ss ) - qp%Bn*gp*( qp%By - qp%Bn*ny  )
            qps%Vz = qp%Vz - nz*( qp%Vn - ss ) - qp%Bn*gp*( qp%Bz - qp%Bn*nz  )
            qps%Bx = qp%Bx*dp - qp%Bn*( dp - 1.0D0 )*nx
            qps%By = qp%By*dp - qp%Bn*( dp - 1.0D0 )*ny
            qps%Bz = qp%Bz*dp - qp%Bn*( dp - 1.0D0 )*nz
        Else
            !> Prevent 0/0 operations.
            qps%Vx = qp%Vx - nx*( qp%Vn - ss )
            qps%Vy = qp%Vy - ny*( qp%Vn - ss )
            qps%Vz = qp%Vz - nz*( qp%Vn - ss )
            qps%Bx = qp%Bx
            qps%By = qp%By
            qps%Bz = qp%Bz
        End If
        qps%Rho = qp%Rho*( sp - qp%Vn )/( sp - ss )
        qps%BV  = qps%Bx*qps%Vx + qps%By*qps%Vy + qps%Bz*qps%Vz
        qps%nrg = qp%Rho*qp%nrg*( sp - qp%Vn ) + ( ps*ss - qp%p_tot*qp%Vn )
        qps%nrg = qps%nrg - qp%Bn*( qps%BV - qp%BV )
        qps%nrg = qps%nrg/( sp - ss )
        qps%U(:) = [ qps%Rho, qps%nrg, &
                     qps%Rho*qps%Vx, qps%Rho*qps%Vy, qps%Rho*qps%Vz, & 
                     [ qps%Bx, qps%By, qps%Bz ]*2.0D0*Sqrt(Pi) ]
        !> Calculate Fm*: [2], Eq (43-47).
        gm = ( qm%Rho*( sm - qm%Vn )*( sm - ss ) - qm%Bn**2 )
        If ( Abs(gm) > 0.0D0 ) Then               
            dm = ( qm%Rho*( sm - qm%Vn )**2 - qm%Bn**2 )/gm
            gm = ( ss - qm%Vn )/gm
            qms%Vx = qm%Vx - nx*( qm%Vn - ss ) - qm%Bn*gm*( qm%Bx - qm%Bn*nx  )
            qms%Vy = qm%Vy - ny*( qm%Vn - ss ) - qm%Bn*gm*( qm%By - qm%Bn*ny  )
            qms%Vz = qm%Vz - nz*( qm%Vn - ss ) - qm%Bn*gm*( qm%Bz - qm%Bn*nz  )
            qms%Bx = qm%Bx*dm - qm%Bn*( dm - 1.0D0 )*nx
            qms%By = qm%By*dm - qm%Bn*( dm - 1.0D0 )*ny
            qms%Bz = qm%Bz*dm - qm%Bn*( dm - 1.0D0 )*nz
        Else
            !> Prevent 0/0 operations.
            qms%Vx = qm%Vx
            qms%Vy = qm%Vy
            qms%Vz = qm%Vz
            qms%Bx = qm%Bx
            qms%By = qm%By
            qms%Bz = qm%Bz
        End If
        qms%Rho = qm%Rho*( sm - qm%Vn )/( sm - ss )
        qms%BV  = qms%Bx*qms%Vx + qms%By*qms%Vy + qms%Bz*qms%Vz
        qms%nrg = qm%Rho*qm%nrg*( sm - qm%Vn ) + ( ps*ss - qm%p_tot*qm%Vn )
        qms%nrg = qms%nrg - qm%Bn*( qms%BV - qm%BV )
        qms%nrg = qms%nrg/( sm - ss )
        qms%U(:) = [ qms%Rho, qms%nrg, &
                     qms%Rho*qms%Vx, qms%Rho*qms%Vy, qms%Rho*qms%Vz, & 
                     [ qms%Bx, qms%By, qms%Bz ]*2.0D0*Sqrt(Pi) ]
        rp = Sqrt(qps%Rho)
        rm = Sqrt(qms%Rho)
        ir = 1.0D0/( rp + rm )
        If ( ss <= 0.0D0 .AND. 0.0D0 <= sp ) Then
            !> [2], Eq. (51).
            ssp = ss + Abs(qp%Bn)/rp
            If ( ssp <= 0.0D0 .AND. 0.0D0 <= sp ) Then
                !> Select Fp*: [2], Eq. (64).
                flux = qp%F + sp*( qps%U - qp%U )
            Else If ( ss <= 0.0D0 .AND. 0.0D0 <= ssp ) Then
                !> Select Fp**: [2], Eq (49), (59-63).
                sgn = Merge(Sign(1.0D0, qp%Bn), 0.0D0, qp%Bn /= 0.0D0)
                sgnr = sgn*rp*rm
                qs%Rho = qps%Rho
                qs%Vx  = ( rp*qps%Vx + rm*qms%Vx + sgn*( qps%Bx - qms%Bx ) )*ir
                qs%Vy  = ( rp*qps%Vy + rm*qms%Vy + sgn*( qps%By - qms%By ) )*ir
                qs%Vz  = ( rp*qps%Vz + rm*qms%Vz + sgn*( qps%Bz - qms%Bz ) )*ir
                qs%Bx  = ( rp*qps%Bx + rm*qms%Bx + sgnr*( qps%Vx - qms%Vx ) )*ir
                qs%By  = ( rp*qps%By + rm*qms%By + sgnr*( qps%Vy - qms%Vy ) )*ir
                qs%Bz  = ( rp*qps%Bz + rm*qms%Bz + sgnr*( qps%Vz - qms%Vz ) )*ir
                qs%BV  = qs%Bx*qs%Vx + qs%By*qs%Vy + qs%Bz*qs%Vz
                qs%nrg = qps%nrg + rp*sgn*( qps%BV - qs%BV )
                qs%U(:) = [ qs%Rho, qs%nrg, &
                            qs%Rho*qs%Vx, qs%Rho*qs%Vy, qs%Rho*qs%Vz, & 
                            [ qs%Bx, qs%By, qs%Bz ]*2.0D0*Sqrt(Pi) ]
                !> [2], Eq. (65).
                flux = qp%F + ssp*qs%U - ( ssp - sp )*qps%U - sp*qp%U
            End If
        Else If ( sm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
            !> [2], Eq. (51).
            ssm = ss - Abs(qm%Bn)/rm
            If ( sm <= 0.0D0 .AND. 0.0D0 <= ssm ) Then
                !> Select Fm*: [2], Eq. (64).
                flux = qm%F + sm*( qms%U - qm%U )
            Else If ( ssm <= 0.0D0 .AND. 0.0D0 <= ss ) Then
                !> Select Fm**: [2], Eq (49), (59-63).
                sgn = Merge(Sign(1.0D0, qm%Bn), 0.0D0, qm%Bn /= 0.0D0)
                sgnr = sgn*rp*rm
                qs%Rho = qms%Rho
                qs%Vx  = ( rp*qps%Vx + rm*qms%Vx + sgn*( qps%Bx - qms%Bx ) )*ir
                qs%Vy  = ( rp*qps%Vy + rm*qms%Vy + sgn*( qps%By - qms%By ) )*ir
                qs%Vz  = ( rp*qps%Vz + rm*qms%Vz + sgn*( qps%Bz - qms%Bz ) )*ir
                qs%Bx  = ( rp*qps%Bx + rm*qms%Bx + sgnr*( qps%Vx - qms%Vx ) )*ir
                qs%By  = ( rp*qps%By + rm*qms%By + sgnr*( qps%Vy - qms%Vy ) )*ir
                qs%Bz  = ( rp*qps%Bz + rm*qms%Bz + sgnr*( qps%Vz - qms%Vz ) )*ir
                qs%BV  = qs%Bx*qs%Vx + qs%By*qs%Vy + qs%Bz*qs%Vz
                qs%nrg = qms%nrg - rm*sgn*( qms%BV - qs%BV )
                qs%U(:) = [ qs%Rho, qs%nrg, &
                            qs%Rho*qs%Vx, qs%Rho*qs%Vy, qs%Rho*qs%Vz, & 
                            [ qs%Bx, qs%By, qs%Bz ]*2.0D0*Sqrt(Pi) ]
                !> [2], Eq. (65).
                flux = qm%F + ssm*qs%U - ( ssm - sm )*qms%U - sm*qm%U
            End If
        End If
    End If
End Subroutine mhd_hydro_calc_flux_hlld3D_mhd
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_hlld


