!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.
#define MHD_DIM 1

Module orchid_solver_hydro
Use orchid_solver_grid
Use orchid_solver_params
Use orchid_solver_hydro_flux
Implicit None
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_update(Tau, g, gp, fl)
    !> Calculate the new Field values.
    !> {{{
    Real(8), Intent(In) :: Tau
    Type(mhd_grid_dg), Intent(InOut) :: g, gp
    Type(mhd_grid_flux), Intent(InOut) :: fl
    !> }}}
    Integer :: i, i_min, i_max, &
               j, j_min, j_max, &
               k, k_min, k_max, &
               m, m_min, m_max
    Integer :: ii, ii_min, ii_max, &
               jj, jj_min, jj_max, &
               kk, kk_min, kk_max
    Integer :: di, ddi, di_min, di_max, &
               dj, ddj, dj_min, dj_max, &
               dk, ddk, dk_min, dk_max
    Real(8), Dimension(-1:1) :: coef_r, coef_p, coef_t
    Real(8), Dimension(-1:1) :: Psi_m_r, Psi_m_p, Psi_m_t
    Real(8) :: r_i, r_iph, r_imh
    Real(8) :: Theta_ja, Theta_jz, Theta_jph, Theta_jmh
    Real(8) :: Phi_k, Phi_kph, Phi_kmh
    j_min = 1; j_max = N_phi
    k_min = 1; k_max = 1;
    i_min = 1; i_max = N_r
    m_min = 0; m_max = 0
    di_min = -1; di_max = 1
    dj_min = -1; dj_max = 1
    dk_min = -1; dk_max = -10

    !> Calculate the fluxes.
    gp = g
    Call mhd_hydro_calc_dg_fluxes(g, fl)

    !> Iterate through all Cells and Basis functions.
    !$OMP Parallel Do
    Do i = i_min, i_max
    Do j = j_min, j_max
    Do k = k_min, k_max
    Do m = m_min, m_max
        
        !>-------------------------------------------------------------------------------
        r_i = r_0 + (i - 0.5D0)*h_r
        r_iph = r_i + 0.5D0*h_r
        r_imh = r_i - 0.5D0*h_r
        If ( dim == 1 ) Then
            coef_r(:)=1.0D0
            coef_p(:)=1.0D0
            coef_t(:)=1.0D0
        Else If (dim == 2 ) Then
            coef_r(:) = [r_imh,0.0D0,r_iph]
            coef_p(:) = 1.0D0
        End If
        Psi_m_r(:)=1.0D0/h_r/h_p
        Psi_m_p(:)=1.0D0/h_r/h_p
        Psi_m_t(:)=1.0D0/h_r/h_p
        !>-------------------------------------------------------------------------------

        !>-------------------------------------------------------------------------------
        !> Clear the values.
        gp%rho(i, j, k, m) = 0.0D0
        gp%nrg(i, j, k, m) = 0.0D0
        gp%v_r(i, j, k, m) = 0.0D0
        gp%v_p(i, j, k, m) = 0.0D0
        gp%v_t(i, j, k, m) = 0.0D0
        !>-------------------------------------------------------------------------------

        gp%rho(i, j, k, m) = gp%rho(i, j, k, m)&
                + ( fl%r%rho(i, j, k, 0, 0) - fl%r%rho(i-1, j, k, 0, 0) )/h_r &
                + ( fl%p%rho(i, j, k, 0, 0) - fl%p%rho(i, j-1, k, 0, 0) )/h_p/r_i &
                + ( fl%r%rho(i, j, k, 0, 0) + fl%r%rho(i-1, j, k, 0, 0) )/(2*r_i)
        gp%nrg(i, j, k, m) = gp%nrg(i, j, k, m)&
                + ( fl%r%nrg(i, j, k, 0, 0) - fl%r%nrg(i-1, j, k, 0, 0) )/h_r &
                + ( fl%p%nrg(i, j, k, 0, 0) - fl%p%nrg(i, j-1, k, 0, 0) )/h_p/r_i &
                + ( fl%r%nrg(i, j, k, 0, 0) + fl%r%nrg(i-1, j, k, 0, 0) )/(2*r_i)
        
        gp%v_r(i, j, k, m) = gp%v_r(i, j, k, m)&
                + ( fl%r%v_r(i, j, k, 0, 0) - fl%r%v_r(i-1, j, k, 0, 0) )/h_r &
                + ( fl%p%v_r(i, j, k, 0, 0) - fl%p%v_r(i, j-1, k, 0, 0) )/h_p/r_i &
                + ( fl%r%v_r(i, j, k, 0, 0) + fl%r%v_r(i-1, j, k, 0, 0) )/(2*r_i) &
                - ( fl%p%v_p(i, j, k, 0, 0) + fl%p%v_p(i, j-1, k, 0, 0) )/(2*r_i)
        gp%v_p(i, j, k, m) = gp%v_p(i, j, k, m)&
                + ( fl%r%v_p(i, j, k, 0, 0) - fl%r%v_p(i-1, j, k, 0, 0) )/h_r &
                + ( fl%p%v_p(i, j, k, 0, 0) - fl%p%v_p(i, j-1, k, 0, 0) )/h_p/r_i &
                + ( fl%r%v_p(i, j, k, 0, 0) + fl%r%v_p(i-1, j, k, 0, 0) )/(2*r_i) &
                + ( fl%p%v_r(i, j, k, 0, 0) + fl%p%v_r(i, j-1, k, 0, 0) )/(2*r_i)
                
        !>-------------------------------------------------------------------------------
        !> Calculate the increment:
        !Do di = di_min, di_max, 2; ddi = ( di - 1 )/2
        !    gp%rho(i, j, k, m) = gp%rho(i, j, k, m) &
        !        + di*h_p*h_t*( coef_r(di)*Psi_m_r(di)*( fl%r%rho(i+ddi, j, k) ) )
        !    gp%nrg(i, j, k, m) = gp%nrg(i, j, k, m) &
        !        + di*h_p*h_t*( coef_r(di)*Psi_m_r(di)*( fl%r%nrg(i+ddi, j, k) ) )
        !    gp%v_r(i, j, k, m) = gp%v_r(i, j, k, m) &
        !        + di*h_p*h_t*( coef_r(di)*Psi_m_r(di)*( fl%r%v_r(i+ddi, j, k) ) )
        !    If ( dim >= 2 ) Then
        !        !> Polar Or Spherical Case.
        !        gp%v_p(i, j, k, m) = gp%v_p(i, j, k, m) &
        !            + di*h_p*h_t*( coef_r(di)*Psi_m_r(di)*( fl%r%v_p(i+ddi, j, k) ) )
        !        If ( dim >= 3 ) Then
        !            !> Spherical Case.
        !            gp%v_t(i, j, k, m) = gp%v_t(i, j, k, m) &
        !                + di*h_p*h_t*( coef_r(di)*Psi_m_r(di)*( fl%r%v_t(i+ddi, j, k) ) )
        !        End If
        !    End If
        !End Do
        !Do dj = dj_min, dj_max, 2; ddj = ( dj - 1 )/2
        !    !> Polar Or Spherical Case.
        !    gp%rho(i, j, k, m) = gp%rho(i, j, k, m) &
        !        + dj*h_r*h_t*( coef_p(dj)*Psi_m_p(dj)*( fl%p%rho(i, j+ddj, k) ) )
        !    gp%nrg(i, j, k, m) = gp%nrg(i, j, k, m) &
        !        + dj*h_r*h_t*( coef_p(dj)*Psi_m_p(dj)*( fl%p%nrg(i, j+ddj, k) ) )
        !    If ( dim == 2 ) Then
        !        !> Polar Case.
        !        gp%v_r(i, j, k, m) = gp%v_r(i, j, k, m) &
        !            + dj*h_r*h_t*( coef_p(dj)*Psi_m_p(dj)*( fl%p%v_r(i, j+ddj, k) &
        !                - dj*0.5D0*h_p*( fl%p%v_p(i, j+ddj, k) ) ) )
        !        gp%v_p(i, j, k, m) = gp%v_p(i, j, k, m) &
        !            + dj*h_r*h_t*( coef_p(dj)*Psi_m_p(dj)*( fl%p%v_p(i, j+ddj, k) &
        !                + dj*0.5D0*h_p*( fl%p%v_r(i, j+ddj, k) ) ) )
        !    Else
        !        !> Spherical Case.
        !        gp%v_r(i, j, k, m) = gp%v_r(i, j, k, m) &
        !            + dj*h_r*h_t*( coef_p(dj)*Psi_m_p(dj)*( fl%p%v_r(i, j+ddj, k) &
        !                - dj*0.5D0*h_p*( fl%p%v_p(i, j+ddj, k)*Sin(Theta_ja) ) ) )
        !        gp%v_p(i, j, k, m) = gp%v_p(i, j, k, m) &
        !            + dj*h_r*h_t*( coef_p(dj)*Psi_m_p(dj)*( fl%p%v_p(i, j+ddj, k) &
        !                + dj*0.5D0*h_p*( fl%p%v_r(i, j+ddj, k)*Sin(Theta_ja) &
        !                                - fl%p%v_t(i, j+ddj, k)*Cos(Theta_jz) ) ) )
        !        gp%v_t(i, j, k, m) = gp%v_t(i, j, k, m) &
        !            + dj*h_r*h_t*( coef_p(dj)*Psi_m_p(dj)*( fl%p%v_t(i, j+ddj, k) &
        !                - dj*0.5D0*h_p*( fl%p%v_p(i, j+ddj, k)*Cos(Theta_jz) ) ) )
        !    End If
        !End Do
        !Do dk = dk_min, dk_max, 2; ddk = ( dk - 1 )/2
        !    !> Spherical Case.
        !    gp%rho(i, j, k, m) = gp%rho(i, j, k, m) &
        !        + dk*h_r*h_p*( coef_t(dk)*Psi_m_t(dk)*( fl%t%rho(i, j, k+ddk) ) )
        !    gp%nrg(i, j, k, m) = gp%nrg(i, j, k, m) &
        !        + dk*h_r*h_p*( coef_t(dk)*Psi_m_t(dk)*( fl%t%nrg(i, j, k+ddk) ) )
        !    gp%v_r(i, j, k, m) = gp%v_r(i, j, k, m) &
        !        + dk*h_r*h_p*( coef_t(dk)*Psi_m_t(dk)*( fl%t%v_r(i, j, k+ddk) &
        !            - dk*0.5D0*h_t*( fl%t%v_p(i, j, k+ddk)*Sin(Theta_ja) ) ) )
        !    gp%v_p(i, j, k, m) = gp%v_p(i, j, k, m) &
        !        + dk*h_r*h_p*( coef_t(dk)*Psi_m_t(dk)*( fl%t%v_p(i, j, k+ddk) ) )
        !    gp%v_t(i, j, k, m) = gp%v_t(i, j, k, m) &
        !        + dk*h_r*h_p*( coef_t(dk)*Psi_m_t(dk)*( fl%t%v_t(i, j, k+ddk) &
        !            - dk*0.5D0*h_t*( fl%t%v_p(i, j, k+ddk)*Cos(Theta_jz) ) ) )
        !End Do
        !!>-------------------------------------------------------------------------------

        !>-------------------------------------------------------------------------------
        !> Update the values.
        gp%rho(i, j, k, m) = g%rho(i, j, k, m) &
            - Tau*gp%rho(i, j, k, m)
        gp%nrg(i, j, k, m) = g%nrg(i, j, k, m) &
            - Tau*gp%nrg(i, j, k, m)
        gp%v_r(i, j, k, m) = g%v_r(i, j, k, m) &
            - Tau*gp%v_r(i, j, k, m)
        If ( dim >= 2 ) Then
            !> Polar Or Spherical Case.
            gp%v_p(i, j, k, m) = g%v_p(i, j, k, m) &
                - Tau*gp%v_p(i, j, k, m)
            If ( dim >= 3 ) Then
                !> Spherical Case.
                gp%v_t(i, j, k, m) = g%v_t(i, j, k, m) &
                    - Tau*gp%v_t(i, j, k, m)
            End If
        End If
        !>-------------------------------------------------------------------------------

        !>-------------------------------------------------------------------------------
        !> Valify the values.
        If ( debug ) Then
            If ( (gp%rho(i, j, k, m) < 0.0D0) .OR. IsNan(gp%rho(i, j, k, m)) .OR. &
                 (gp%nrg(i, j, k, m) < 0.0D0) .OR. IsNan(gp%nrg(i, j, k, m)) .OR. &
                 ( dim >= 1 .AND. IsNan(gp%v_r(i, j, k, m)) ) .OR. &
                 ( dim >= 2 .AND. IsNan(gp%v_p(i, j, k, m)) ) .OR. &
                 ( dim >= 3 .AND. IsNan(gp%v_t(i, j, k, m)) ) ) Then
                If ( verbose ) Then
                    Write (0,*) 'Invalid flow paramaters were detected at: ', i, j, k
                End If
                Stop 1
            End If
        End If
        !>-------------------------------------------------------------------------------
    End Do
    End Do
    End Do
    End Do
    !$OMP End Parallel Do
End Subroutine mhd_hydro_update
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro
