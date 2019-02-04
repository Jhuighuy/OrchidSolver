!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.
#define MHD_DIM 1

Module orchid_solver_hydro
Use orchid_solver_grid
Use orchid_solver_params
Use orchid_solver_hydro_flux_roe
Use orchid_solver_hydro_flux_hllc
Implicit None
    
Contains
    
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_flux_single1D(nx, &
                                        rho_p, nrg_p, u_p, &
                                        rho_m, nrg_m, u_m, &
                                        flux_rho, flux_nrg, flux_u)
    !> Calculate the Fluxes in 1D.
    !> {{{
    Real(8), Intent(In) :: nx
    Real(8), Intent(In) :: rho_p, nrg_p, u_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    If ( hydro_flux == 'roe' ) Then
         Call mhd_hydro_calc_flux_roe1D(nx, &
                rho_p, nrg_p, u_p, &
                rho_m, nrg_m, u_m, &
                flux_rho, flux_nrg, flux_u)
    Else If ( hydro_flux == 'hllc' ) Then
         Call mhd_hydro_calc_flux_hllc1D(nx, &
                rho_p, nrg_p, u_p, &
                rho_m, nrg_m, u_m, &
                flux_rho, flux_nrg, flux_u)
    Else
        If ( debug ) Then 
            Write (0,*) 'Hydro flux ', Trim(hydro_flux), &
                        ' is not implemented for 1D.'
        End If
        Stop
    End If
End Subroutine mhd_hydro_calc_flux_single1D
Subroutine mhd_hydro_calc_flux_single2D(nx, ny, &
                                        rho_p, nrg_p, u_p, v_p, &
                                        rho_m, nrg_m, u_m, v_m, &
                                        flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the Fluxes in 2D.
    !> {{{
    Real(8), Intent(In) :: nx, ny
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v
    !> }}}
    If ( hydro_flux == 'roe' ) Then
        Call mhd_hydro_calc_flux_roe2D(nx, ny, &
                rho_p, nrg_p, u_p, v_p, &
                rho_m, nrg_m, u_m, v_m, &
                flux_rho, flux_nrg, flux_u, flux_v)
    Else If ( hydro_flux == 'hllc' ) Then
        Call mhd_hydro_calc_flux_hllc2D(nx, ny, &
                rho_p, nrg_p, u_p, v_p, &
                rho_m, nrg_m, u_m, v_m, &
                flux_rho, flux_nrg, flux_u, flux_v)
    Else
        If ( debug ) Then 
            Write (0,*) 'Hydro flux ', Trim(hydro_flux), &
                        ' is not implemented for 2D.'
        End If
        Stop
    End If
End Subroutine mhd_hydro_calc_flux_single2D
Subroutine mhd_hydro_calc_flux_single3D(nx, ny, nz, &
                                        rho_p, nrg_p, u_p, v_p, w_p, &
                                        rho_m, nrg_m, u_m, v_m, w_m, &
                                        flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the Fluxes in 3D.
    !> {{{
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    If ( hydro_flux == 'roe' ) Then
        Call mhd_hydro_calc_flux_roe3D(nx, ny, nz, &
            rho_p, nrg_p, u_p, v_p, w_p, &
            rho_m, nrg_m, u_m, v_m, w_m, &
            flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    Else If ( hydro_flux == 'roe' ) Then
        Call mhd_hydro_calc_flux_hllc3D(nx, ny, nz, &
            rho_p, nrg_p, u_p, v_p, w_p, &
            rho_m, nrg_m, u_m, v_m, w_m, &
            flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    Else
        If ( debug ) Then 
            Write (0,*) 'Hydro flux ', Trim(hydro_flux), &
                        ' is not implemented for 2D.'
        End If
        Stop
    End If
End Subroutine mhd_hydro_calc_flux_single3D
Subroutine mhd_hydro_calc_flux_single(g, fl, ip, jp, kp, &
                                      nr, np, nt, im, jm, km, ii, jj, kk, &
                                      aap, aam)
    !> {{{
    Type(mhd_grid_dg), Intent(In) :: g
    Type(mhd_grid), Intent(Out) :: fl
    Integer, Intent(In) :: nr, nt, np
    Integer, Intent(In) :: ip, jp, kp, im, jm, km
    Integer, Intent(In), Optional :: ii, jj, kk
    Real(8), Intent(In), Optional :: aap, aam
    !> }}}
    Integer :: m
    Integer :: i, j, k
    Real(8) :: rho_p, nrg_p, u_p, v_p, w_p, ap, bp, cp, &
               rho_m, nrg_m, u_m, v_m, w_m, am, bm, cm
    If ( Present(ii) ) Then
        i = ii; j = jj; k = kk
    Else
        i = im; j = jm; k = km
    End If
    If ( Present(aap) ) Then
        ap=aap;bp=aap;cp=aap
        am=aam;bm=aam;cm=aam
    Else
        ap=1;bp=1;cp=1
        am=1;bm=1;cm=1
    End If
    !> @todo
    !> For DG, we need to calculate the values and
    !> estimate the integration, using e.g. Gauss
    !> quadratures.
    m = 0
    rho_p = g%rho(ip, jp, kp, m)
    nrg_p = g%nrg(ip, jp, kp, m)/rho_p
    u_p   = g%v_r(ip, jp, kp, m)/rho_p*ap
    rho_m = g%rho(im, jm, km, m)
    nrg_m = g%nrg(im, jm, km, m)/rho_m
    u_m   = g%v_r(im, jm, km, m)/rho_m*am
    If ( dim == 1 ) Then
        !> 1D Case.
        Call mhd_hydro_calc_flux_single1D(Dble(nr), &
                rho_p, nrg_p, u_p, &
                rho_m, nrg_m, u_m, &
                fl%rho(i, j, k), fl%nrg(i, j, k), &
                fl%v_r(i, j, k))
    Else
        !> Polar Or Spherical Case.
        v_p = g%v_p(ip, jp, kp, m)/rho_p*bp
        v_m = g%v_p(im, jm, km, m)/rho_m*bm
        If ( dim == 2 ) Then
            !> Polar Case.
            Call mhd_hydro_calc_flux_single2D(Dble(nr), Dble(np), &
                    rho_p, nrg_p, u_p, v_p, &
                    rho_m, nrg_m, u_m, v_m, &
                    fl%rho(i, j, k), fl%nrg(i, j, k), &
                    fl%v_r(i, j, k), fl%v_p(i, j, k))
        Else
            !> Spherical Case.
            w_p = g%v_t(ip, jp, kp, m)/rho_p*cp
            w_m = g%v_t(im, jm, km, m)/rho_m*cm
            Call mhd_hydro_calc_flux_single3D(Dble(nr), Dble(np), Dble(nt), &
                    rho_p, nrg_p, u_p, v_p, w_p, &
                    rho_m, nrg_m, u_m, v_m, w_m, &
                    fl%rho(i, j, k), fl%nrg(i, j, k), &
                    fl%v_r(i, j, k), fl%v_p(i, j, k), fl%v_t(i, j, k))
        End If
    End If
End Subroutine mhd_hydro_calc_flux_single
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_fluxes(g, fl)
    !> {{{
    Type(mhd_grid_dg), Intent(In) :: g
    Type(mhd_grid_flux), Intent(Out) :: fl
    !> }}}
    Integer :: i, i_min, i_max, &
               j, j_min, j_max, &
               k, k_min, k_max, &
               m, m_min, m_max
    Real(8) :: rho_p, nrg_p, u_p, v_p, w_p, &
               rho_m, nrg_m, u_m, v_m, w_m

    j_min = 1; j_max = N_phi
    k_min = 1; k_max = 1;
    i_min = 1; i_max = N_r
    m_min = 0; m_max = 0

    !$OMP Parallel Do
    Do i = i_min, i_max
    Do j = j_min, j_max
    Do k = k_min, k_max; m = 0

        !>-------------------------------------------------------------------------------
        !> Calculate the R faces interface values.
        !> Calculate the Fluxes through R faces. 
        If ( i /= i_max ) Then
            !> Domain Interior.
            Call mhd_hydro_calc_flux_single(g, fl%r, i+1, j, k, &
                                            1, 0, 0, i+0, j, k)
        Else
            !> Domain boundary:
            If ( R0_bc == 'free' ) Then
                !> Free flow BC at the inner radius.
                Call mhd_hydro_calc_flux_single(g, fl%r, 1+0, j, k, &
                                                1, 0, 0, 1+0, j, k, 0, j, k)
            Else If ( R0_bc == 'wall' ) Then
                !> Solid wall BC at the inner radius.
                Call mhd_hydro_calc_flux_single(g, fl%r, 1+0, j, k, &
                                                1, 0, 0, 1+0, j, k, 0, j, k, 1.0D0, 0.0D0)
            End If
            If ( R1_bc == 'free' ) Then
                !> Free flow BC at the outer radius.
                Call mhd_hydro_calc_flux_single(g, fl%r, i+0, j, k, &
                                                1, 0, 0, i+0, j, k)
            Else If ( R1_bc == 'wall' ) Then
                !> Solid wall BC at the outer radius.
                Call mhd_hydro_calc_flux_single(g, fl%r, i+0, j, k, &
                                                1, 0, 0, i+0, j, k, i, j, k, 0.0D0, 1.0D0)
            Else If ( R1_bc == 'periodic' ) Then
                !> Periodic BC. Makes sense only in 1D case.
                Call mhd_hydro_calc_flux_single(g, fl%r, 0+1, j, k, &
                                                1, 0, 0, i+0, j, k)
                fl%r%rho(0, j, k) = fl%r%rho(i, j, k)
                fl%r%nrg(0, j, k) = fl%r%nrg(i, j, k)
                fl%r%v_r(0, j, k) = fl%r%v_r(i, j, k)
                If ( dim >= 2 ) Then
                    !> Polar Or Spherical Case.
                    fl%r%v_p(0, j, k) = fl%r%v_p(i, j, k)
                    If ( dim >= 3 ) Then
                        !> Spherical Case.
                        fl%r%v_t(0, j, k) = fl%r%v_t(i, j, k)
                    End If
                End If
            End If
        End If
        !>-------------------------------------------------------------------------------

        !>-------------------------------------------------------------------------------
        !> Calculate the Fluxes through Phi faces. 
        If ( dim >= 2 ) Then
            !> Polar Or Spherical Case.
            If ( j /= j_max ) Then
                !!> Domain Interior.
                Call mhd_hydro_calc_flux_single(g, fl%p, i, j+1, k, &
                                                0, 1, 0, i, j+0, k)
            Else
                ! Call mhd_hydro_calc_flux_single(g, fl%p, i, 1+0, k, &
                !                                 0, 1, 0, i, 1+0, k, i, 0, k)
                ! Call mhd_hydro_calc_flux_single(g, fl%p, i, j+0, k, &
                !                                 0, 1, 0, i, j+0, k)
                !> Domain boundary:
                !> Force periodic BC for Phi.
                Call mhd_hydro_calc_flux_single(g, fl%p, i, 0+1, k, &
                                               0, 1, 0, i, j+0, k)
                fl%p%rho(i, 0, k) = fl%p%rho(i, j, k)
                fl%p%nrg(i, 0, k) = fl%p%nrg(i, j, k)
                fl%p%v_r(i, 0, k) = fl%p%v_r(i, j, k)
                fl%p%v_p(i, 0, k) = fl%p%v_p(i, j, k)
                If ( dim >= 3 ) Then
                   !> Spherical Case.
                   fl%p%v_t(i, 0, k) = fl%p%v_t(i, j, k)
                End If
            End If
        End If
        !>-------------------------------------------------------------------------------

        !>-------------------------------------------------------------------------------
        !> Calculate the Fluxes through Theta faces. 
        If ( dim >= 3 ) Then
            !> Spherical Case.
            If ( j /= j_max ) Then
                !> Domain Interior.
                Call mhd_hydro_calc_flux_single(g, fl%t, i, j, k+1, &
                                                0, 1, 0, i, j, k+0)
            Else
                !> Domain boundary:
                !> No BC for Theta.
            End If
        End If
        !>-------------------------------------------------------------------------------
    End Do
    End Do
    End Do
    !$OMP End Parallel Do
End Subroutine mhd_hydro_calc_fluxes
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
    Call mhd_hydro_calc_fluxes(g, fl)

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
                + ( fl%r%rho(i, j, k) - fl%r%rho(i-1, j, k) )/h_r &
                + ( fl%p%rho(i, j, k) - fl%p%rho(i, j-1, k) )/h_p/r_i &
                + ( fl%r%rho(i, j, k) + fl%r%rho(i-1, j, k) )/(2*r_i)
        gp%nrg(i, j, k, m) = gp%nrg(i, j, k, m)&
                + ( fl%r%nrg(i, j, k) - fl%r%nrg(i-1, j, k) )/h_r &
                + ( fl%p%nrg(i, j, k) - fl%p%nrg(i, j-1, k) )/h_p/r_i &
                + ( fl%r%nrg(i, j, k) + fl%r%nrg(i-1, j, k) )/(2*r_i)
        
        gp%v_r(i, j, k, m) = gp%v_r(i, j, k, m)&
                + ( fl%r%v_r(i, j, k) - fl%r%v_r(i-1, j, k) )/h_r &
                + ( fl%p%v_r(i, j, k) - fl%p%v_r(i, j-1, k) )/h_p/r_i &
                + ( fl%r%v_r(i, j, k) + fl%r%v_r(i-1, j, k) )/(2*r_i) &
                - ( fl%p%v_p(i, j, k) + fl%p%v_p(i, j-1, k) )/(2*r_i)
        gp%v_p(i, j, k, m) = gp%v_p(i, j, k, m)&
                + ( fl%r%v_p(i, j, k) - fl%r%v_p(i-1, j, k) )/h_r &
                + ( fl%p%v_p(i, j, k) - fl%p%v_p(i, j-1, k) )/h_p/r_i &
                + ( fl%r%v_p(i, j, k) + fl%r%v_p(i-1, j, k) )/(2*r_i) &
                + ( fl%p%v_r(i, j, k) + fl%p%v_r(i, j-1, k) )/(2*r_i)
                
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
                Stop
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
