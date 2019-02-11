!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.
    
Module orchid_solver_hydro_flux
Use orchid_solver_grid
Use orchid_solver_hydro_flux_hllc
Use orchid_solver_hydro_flux_roe
Implicit None
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_dg_flux1D(nx, &
                                    rho_vp, nrg_vp, u_vp, &
                                    rho_vm, nrg_vm, u_vm, &
                                    flux_rho, flux_nrg, flux_u)
    !> Calculate the Fluxes in 1D.
    !> {{{
    Real(8), Intent(In) :: nx
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    Real(8), Dimension(m_min:m_max) :: basis
    Real(8) :: rho_p, nrg_p, u_p, &
               rho_m, nrg_m, u_m
    !> Calculate +Values.
    basis = GaussLegendreEdge1D(:)
    rho_p = Dot_Product(rho_vp, basis)
    nrg_p = Dot_Product(nrg_vp, basis)/rho_p
    u_p = Dot_Product(u_vp, basis)/rho_p
    !> Calculate -Values.
    basis = GaussLegendreEdge1D(:)
    rho_m = Dot_Product(rho_vm, basis)
    nrg_m = Dot_Product(nrg_vm, basis)/rho_m
    u_m = Dot_Product(u_vm, basis)/rho_m
    !> Calculate the Fluxes.
    If ( hydro_flux == 'hllc' ) Then
            Call mhd_hydro_calc_flux_hllc1D(nx, &
                    rho_p, nrg_p, u_p, &
                    rho_m, nrg_m, u_m, &
                    flux_rho, flux_nrg, flux_u)
    Else If ( hydro_flux == 'roe' ) Then
            Call mhd_hydro_calc_flux_roe1D(nx, &
                    rho_p, nrg_p, u_p, &
                    rho_m, nrg_m, u_m, &
                    flux_rho, flux_nrg, flux_u)
    Else
        Write (0,*) 'Hydro flux ', Trim(hydro_flux), &
                    ' is not implemented for 1D.'
        Stop 1
    End If
End Subroutine mhd_hydro_calc_dg_flux1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_dg_flux2D(nx, ny, &
                                    rho_vp, nrg_vp, u_vp, v_vp, &
                                    rho_vm, nrg_vm, u_vm, v_vm, &
                                    flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the Fluxes in 2D at Gauss points on the cell edge.
    !> {{{
    Real(8), Intent(In) :: nx, ny
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp, v_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm, v_vm
    Real(8), Dimension(m_min:m_max), Intent(Out) :: flux_rho, flux_nrg, flux_v, flux_u
    !> }}}
    Integer :: m
    Integer :: np, nm
    Real(8), Dimension(m_min:m_max) :: basis
    Real(8) :: rho_p, nrg_p, u_p, v_p, &
               rho_m, nrg_m, u_m, v_m
    np = 1
    nm = 3
    Do m = m_min, m_max
        !> Calculate +Values.
        basis = GaussLegendreEdge2D(np, m, :)
        rho_p = Dot_Product(rho_vp, basis)
        nrg_p = Dot_Product(nrg_vp, basis)/rho_p
        u_p = Dot_Product(u_vp, basis)/rho_p
        v_p = Dot_Product(v_vp, basis)/rho_p
        !> Calculate -Values.
        basis = GaussLegendreEdge2D(nm, m, :)
        rho_m = Dot_Product(rho_vm, basis)
        nrg_m = Dot_Product(nrg_vm, basis)/rho_m
        u_m = Dot_Product(u_vm, basis)/rho_m
        v_m = Dot_Product(v_vm, basis)/rho_m
        !> Calculate the Fluxes.
        If ( hydro_flux == 'hllc' ) Then
            Call mhd_hydro_calc_flux_hllc2D(nx, ny, &
                    rho_p, nrg_p, u_p, v_p, &
                    rho_m, nrg_m, u_m, v_m, &
                    flux_rho(m), flux_nrg(m), flux_u(m), flux_v(m))
        Else If ( hydro_flux == 'roe' ) Then
            Call mhd_hydro_calc_flux_roe2D(nx, ny, &
                    rho_p, nrg_p, u_p, v_p, &
                    rho_m, nrg_m, u_m, v_m, &
                    flux_rho(m), flux_nrg(m), flux_u(m), flux_v(m))
        Else
            Write (0,*) 'Hydro flux ', Trim(hydro_flux), &
                        ' is not implemented for 2D.'
            Stop 1
        End If
        !> Premultiply Fluxes by the Gauss weights.
        flux_rho(m) = Gauss_w(m_max, m)*flux_rho(m)
        flux_nrg(m) = Gauss_w(m_max, m)*flux_nrg(m)
        flux_u(m) = Gauss_w(m_max, m)*flux_u(m)
        flux_v(m) = Gauss_w(m_max, m)*flux_v(m)
    End Do
End Subroutine mhd_hydro_calc_dg_flux2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_dg_flux3D(nx, ny, nz, &
                                    rho_vp, nrg_vp, u_vp, v_vp, w_vp, &
                                    rho_vm, nrg_vm, u_vm, v_vm, w_vm, &
                                    flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the Fluxes in 3D at Gauss points on the cell edge.
    !> {{{
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp, v_vp, w_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm, v_vm, w_vm
    Real(8), Dimension(m_min:m_max, m_min:m_max), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    Integer :: m1, m2
    Integer :: np, nm
    Real(8), Dimension(m_min:m_max) :: basis
    Real(8) :: rho_p, nrg_p, u_p, v_p, w_p, &
               rho_m, nrg_m, u_m, v_m, w_m
    np = 1
    nm = 4
    Do m1 = m_min, m_max
    Do m2 = m_min, m_max
        !> Calculate +Values.
        basis = GaussLegendreEdge3D(np, m1, m2, :)
        rho_p = Dot_Product(rho_vp, basis)
        nrg_p = Dot_Product(nrg_vp, basis)/rho_p
        u_p = Dot_Product(u_vp, basis)/rho_p
        v_p = Dot_Product(v_vp, basis)/rho_p
        w_p = Dot_Product(w_vp, basis)/rho_p
        !> Calculate -Values.
        basis = GaussLegendreEdge3D(nm, m1, m2, :)
        rho_m = Dot_Product(rho_vm, basis)
        nrg_m = Dot_Product(nrg_vm, basis)/rho_m
        u_m = Dot_Product(u_vm, basis)/rho_m
        v_m = Dot_Product(v_vm, basis)/rho_m
        w_m = Dot_Product(w_vm, basis)/rho_m
        !> Calculate Fluxes.
        If ( hydro_flux == 'hllc' ) Then
            Call mhd_hydro_calc_flux_hllc3D(nx, ny, nz, &
                    rho_p, nrg_p, u_p, v_p, w_p, &
                    rho_m, nrg_m, u_m, v_m, w_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2), flux_v(m1, m2), flux_w(m1, m2))
        Else If ( hydro_flux == 'roe' ) Then
            Call mhd_hydro_calc_flux_roe3D(nx, ny, nz, &
                    rho_p, nrg_p, u_p, v_p, w_p, &
                    rho_m, nrg_m, u_m, v_m, w_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2), flux_v(m1, m2), flux_w(m1, m2))
        Else
            Write (0,*) 'Hydro flux ', Trim(hydro_flux), &
                        ' is not implemented for 3D.'
            Stop 1
        End If
    End Do
    End Do
End Subroutine mhd_hydro_calc_dg_flux3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_dg_fluxes(g, fl)
    !> Calculate the Fluxes.
    !> {{{
    Type(mhd_grid_dg), Intent(In) :: g
    Type(mhd_grid_flux), Intent(Out) :: fl
    !> }}}
    Integer :: i, i_min, i_max, &
               j, j_min, j_max, &
               k, k_min, k_max
    
    Real(8) :: nx_r, nx_p, nx_t
    Real(8) :: ny_r, ny_p, ny_t
    Real(8) :: nz_r, nz_p, nz_t
    
    Real(8), Dimension(0:0) :: rho_p, nrg_p, u_p, v_p, w_p, &
               rho_m, nrg_m, u_m, v_m, w_m
    
    j_min = 1; j_max = N_phi
    k_min = 1; k_max = 1;
    i_min = 1; i_max = N_r
    
    !$OMP Do Private(rho_p, nrg_p, u_p, v_p, w_p, &
                     rho_m, nrg_m, u_m, v_m, w_m)
    Do i = i_min, i_max
    Do j = j_min, j_max
    Do k = k_min, k_max
        
        !> @todo Real normals from the unstructured grid should be here.
        nx_r = 1; ny_r = 0; nz_r = 0
        nx_p = 0; ny_p = 1; nz_p = 0
        nx_t = 0; ny_t = 0; nz_t = 1
        
        !>-------------------------------------------------------------------------------
        !> Calculate the Fluxes through R faces. 
        If ( i /= i_max ) Then
            !> Domain Interior.
            rho_p = g%rho(i+1, j, k, :)
            nrg_p = g%nrg(i+1, j, k, :)
            u_p   = g%v_r(i+1, j, k, :)
            rho_m = g%rho(i+0, j, k, :)
            nrg_m = g%nrg(i+0, j, k, :)
            u_m   = g%v_r(i+0, j, k, :)
            If ( dim == 1 ) Then
                !> 1D Case.
                Call mhd_hydro_calc_dg_flux1D(nx_r, &
                        rho_p, nrg_p, u_p, &
                        rho_m, nrg_m, u_m, &
                        fl%r%rho(i, j, k, m_min, m_min), fl%r%nrg(i, j, k, m_min, m_min), &
                        fl%r%v_r(i, j, k, m_min, m_min))
            Else
                !> Polar Or Spherical Case.
                v_p = g%v_p(i+1, j, k, :)
                v_m = g%v_p(i+0, j, k, :)
                If ( dim == 2 ) Then
                    !> Polar Case.
                    Call mhd_hydro_calc_dg_flux2D(nx_r, ny_r, &
                            rho_p, nrg_p, u_p, v_p, &
                            rho_m, nrg_m, u_m, v_m, &
                            fl%r%rho(i, j, k, :, m_min), fl%r%nrg(i, j, k, :, m_min), &
                            fl%r%v_r(i, j, k, :, m_min), fl%r%v_p(i, j, k, :, m_min))
                Else If ( dim == 3 ) Then
                    !> Spherical Case.
                    w_p = g%v_t(i+1, j, k, :)
                    w_m = g%v_t(i+0, j, k, :)
                    Call mhd_hydro_calc_dg_flux3D(nx_r, ny_r, nz_r, &
                            rho_p, nrg_p, u_p, v_p, w_p, &
                            rho_m, nrg_m, u_m, v_m, w_m, &
                            fl%r%rho(i, j, k, :, :), fl%r%nrg(i, j, k, :, :), &
                            fl%r%v_r(i, j, k, :, :), fl%r%v_p(i, j, k, :, :), fl%r%v_t(i, j, k, :, :))
                End If
            End If
        Else If ( r0 > 0.0D0 ) Then
            !> Domain boundary at the inner Radius.
            If ( MHD_R0_BOUNDARY_COND == 'free' ) Then
                !> Free flow boundary conditions.
                rho_p = g%rho(1, j, k, :)
                nrg_p = g%nrg(1, j, k, :)
                u_p   = g%v_r(1, j, k, :)
                If ( dim == 1 ) Then
                    !> 1D Case.
                    Call mhd_hydro_calc_dg_flux1D(nx_r, &
                            rho_p, nrg_p, u_p, &
                            rho_p, nrg_p, u_p, &
                            fl%r%rho(0, j, k, m_min, m_min), fl%r%nrg(0, j, k, m_min, m_min), &
                            fl%r%v_r(0, j, k, m_min, m_min))
                Else
                    !> Polar Or Spherical Case.
                    v_p = g%v_p(1, j, k, :)
                    If ( dim == 2 ) Then
                        !> Polar Case.
                        Call mhd_hydro_calc_dg_flux2D(nx_r, ny_r, &
                                rho_p, nrg_p, u_p, v_p, &
                                rho_p, nrg_p, u_p, v_p, &
                                fl%r%rho(0, j, k, :, m_min), fl%r%nrg(0, j, k, :, m_min), &
                                fl%r%v_r(0, j, k, :, m_min), fl%r%v_p(0, j, k, :, m_min))
                    Else If ( dim == 3 ) Then
                        !> Spherical Case.
                        w_p = g%v_t(1, j, k, :)
                        Call mhd_hydro_calc_dg_flux3D(nx_r, ny_r, nz_r, &
                                rho_p, nrg_p, u_p, v_p, w_p, &
                                rho_p, nrg_p, u_p, v_p, w_p, &
                                fl%r%rho(0, j, k, :, :), fl%r%nrg(0, j, k, :, :), &
                                fl%r%v_r(0, j, k, :, :), fl%r%v_p(0, j, k, :, :), fl%r%v_t(0, j, k, :, :))
                    End If
                End If
            Else If ( MHD_R0_BOUNDARY_COND == 'wall' ) Then
                !> Solid wall boundary conditions.
                rho_p = g%rho(1, j, k, :)
                nrg_p = g%nrg(1, j, k, :)
                u_p   = g%v_r(1, j, k, :)
                u_m   = 0.0D0
                If ( dim == 1 ) Then
                    !> 1D Case.
                    Call mhd_hydro_calc_dg_flux1D(nx_r, &
                            rho_p, nrg_p, u_p, &
                            rho_p, nrg_p, u_m, &
                            fl%r%rho(0, j, k, m_min, m_min), fl%r%nrg(0, j, k, m_min, m_min), &
                            fl%r%v_r(0, j, k, m_min, m_min))
                Else
                    !> Polar Or Spherical Case.
                    v_p = g%v_p(1, j, k, :)
                    v_m = 0.0D0
                    If ( dim == 2 ) Then
                        !> Polar Case.
                        Call mhd_hydro_calc_dg_flux2D(nx_r, ny_r, &
                                rho_p, nrg_p, u_p, v_p, &
                                rho_p, nrg_p, u_m, v_m, &
                                fl%r%rho(0, j, k, :, m_min), fl%r%nrg(0, j, k, :, m_min), &
                                fl%r%v_r(0, j, k, :, m_min), fl%r%v_p(0, j, k, :, m_min))
                    Else If ( dim == 3 ) Then
                        !> Spherical Case.
                        w_p = g%v_t(1, j, k, :)
                        w_m = 0.0D0
                        Call mhd_hydro_calc_dg_flux3D(nx_r, ny_r, nz_r, &
                                rho_p, nrg_p, u_p, v_p, w_p, &
                                rho_p, nrg_p, u_m, v_m, w_m, &
                                fl%r%rho(0, j, k, :, :), fl%r%nrg(0, j, k, :, :), &
                                fl%r%v_r(0, j, k, :, :), fl%r%v_p(0, j, k, :, :), fl%r%v_t(0, j, k, :, :))
                    End If
                End If
            End If
            !> Domain boundary at the outer Radius.
            If ( MHD_R1_BOUNDARY_COND == 'free' ) Then
                !> Free flow boundary conditions.
                rho_m = g%rho(i, j, k, :)
                nrg_m = g%nrg(i, j, k, :)
                u_m   = g%v_r(i, j, k, :)
                If ( dim == 1 ) Then
                    !> 1D Case.
                    Call mhd_hydro_calc_dg_flux1D(nx_r, &
                            rho_m, nrg_m, u_m, &
                            rho_m, nrg_m, u_m, &
                            fl%r%rho(i, j, k, m_min, m_min), fl%r%nrg(i, j, k, m_min, m_min), &
                            fl%r%v_r(i, j, k, m_min, m_min))
                Else
                    !> Polar Or Spherical Case.
                    v_m = g%v_p(i, j, k, :)
                    If ( dim == 2 ) Then
                        !> Polar Case.
                        Call mhd_hydro_calc_dg_flux2D(nx_r, ny_r, &
                                rho_m, nrg_m, u_m, v_m, &
                                rho_m, nrg_m, u_m, v_m, &
                                fl%r%rho(i, j, k, :, m_min), fl%r%nrg(i, j, k, :, m_min), &
                                fl%r%v_r(i, j, k, :, m_min), fl%r%v_p(i, j, k, :, m_min))
                    Else If ( dim == 3 ) Then
                        !> Spherical Case.
                        w_m = g%v_t(i, j, k, :)
                        Call mhd_hydro_calc_dg_flux3D(nx_r, ny_r, nz_r, &
                                rho_m, nrg_m, u_m, v_m, w_m, &
                                rho_m, nrg_m, u_m, v_m, w_m, &
                                fl%r%rho(i, j, k, :, :), fl%r%nrg(i, j, k, :, :), &
                                fl%r%v_r(i, j, k, :, :), fl%r%v_p(i, j, k, :, :), fl%r%v_t(i, j, k, :, :))
                    End If
                End If
            Else If ( MHD_R1_BOUNDARY_COND == 'wall' ) Then
                !> Solid wall boundary conditions.
                rho_m = g%rho(i, j, k, :)
                nrg_m = g%nrg(i, j, k, :)
                u_m   = g%v_r(i, j, k, :)
                u_p   = 0.0D0
                If ( dim == 1 ) Then
                    !> 1D Case.
                    Call mhd_hydro_calc_dg_flux1D(nx_r, &
                            rho_p, nrg_p, u_p, &
                            rho_p, nrg_p, u_m, &
                            fl%r%rho(i, j, k, m_min, m_min), fl%r%nrg(i, j, k, m_min, m_min), &
                            fl%r%v_r(i, j, k, m_min, m_min))
                Else
                    !> Polar Or Spherical Case.
                    v_m = g%v_p(i, j, k, :)
                    v_p = 0.0D0
                    If ( dim == 2 ) Then
                        !> Polar Case.
                        Call mhd_hydro_calc_dg_flux2D(nx_r, ny_r, &
                                rho_p, nrg_p, u_p, v_p, &
                                rho_p, nrg_p, u_m, v_m, &
                                fl%r%rho(i, j, k, :, m_min), fl%r%nrg(i, j, k, :, m_min), &
                                fl%r%v_r(i, j, k, :, m_min), fl%r%v_p(i, j, k, :, m_min))
                    Else If ( dim == 3 ) Then
                        !> Spherical Case.
                        w_m = g%v_t(i, j, k, :)
                        w_p = 0.0D0
                        Call mhd_hydro_calc_dg_flux3D(nx_r, ny_r, nz_r, &
                                rho_p, nrg_p, u_p, v_p, w_p, &
                                rho_p, nrg_p, u_m, v_m, w_m, &
                                fl%r%rho(i, j, k, :, :), fl%r%nrg(i, j, k, :, :), &
                                fl%r%v_r(i, j, k, :, :), fl%r%v_p(i, j, k, :, :), fl%r%v_t(i, j, k, :, :))
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
                !> Domain Interior.
                rho_p = g%rho(i, j+1, k, :)
                nrg_p = g%nrg(i, j+1, k, :)
                u_p   = g%v_r(i, j+1, k, :)
                v_p   = g%v_p(i, j+1, k, :)
                rho_m = g%rho(i, j+0, k, :)
                nrg_m = g%nrg(i, j+0, k, :)
                u_m   = g%v_r(i, j+0, k, :)
                v_m   = g%v_p(i, j+0, k, :)
                If ( dim == 2 ) Then
                    !> Polar Case.
                    Call mhd_hydro_calc_dg_flux2D(nx_p, ny_p, &
                            rho_p, nrg_p, u_p, v_p, &
                            rho_m, nrg_m, u_m, v_m, &
                            fl%p%rho(i, j, k, :, m_min), fl%p%nrg(i, j, k, :, m_min), &
                            fl%p%v_r(i, j, k, :, m_min), fl%p%v_p(i, j, k, :, m_min))
                Else If ( dim == 3 ) Then
                    !> Spherical Case.
                    w_p = g%v_t(i, j+1, k, :)
                    w_m = g%v_t(i, j+0, k, :)
                    Call mhd_hydro_calc_dg_flux3D(nx_p, ny_p, nz_p, &
                            rho_p, nrg_p, u_p, v_p, w_p, &
                            rho_m, nrg_m, u_m, v_m, w_m, &
                            fl%p%rho(i, j, k, :, :), fl%p%nrg(i, j, k, :, :), &
                            fl%p%v_r(i, j, k, :, :), fl%p%v_p(i, j, k, :, :), fl%p%v_t(i, j, k, :, :))
                End If
            Else
                !> Periodic BC for the Polar angle.
                rho_p = g%rho(i, 1, k, :)
                nrg_p = g%nrg(i, 1, k, :)
                u_p   = g%v_r(i, 1, k, :)
                v_p   = g%v_p(i, 1, k, :)
                rho_m = g%rho(i, j, k, :)
                nrg_m = g%nrg(i, j, k, :)
                u_m   = g%v_r(i, j, k, :)
                v_m   = g%v_p(i, j, k, :)
                If ( dim == 2 ) Then
                    !> Polar Case.
                    Call mhd_hydro_calc_dg_flux2D(nx_p, ny_p, &
                            rho_p, nrg_p, u_p, v_p, &
                            rho_m, nrg_m, u_m, v_m, &
                            fl%p%rho(i, j, k, :, m_min), fl%p%nrg(i, j, k, :, m_min), &
                            fl%p%v_r(i, j, k, :, m_min), fl%p%v_p(i, j, k, :, m_min))
                    fl%p%rho(i, 0, k, :, m_min) = fl%p%rho(i, j, k, :, m_min)
                    fl%p%nrg(i, 0, k, :, m_min) = fl%p%nrg(i, j, k, :, m_min)
                    fl%p%v_r(i, 0, k, :, m_min) = fl%p%v_r(i, j, k, :, m_min)
                    fl%p%v_p(i, 0, k, :, m_min) = fl%p%v_p(i, j, k, :, m_min)
                Else If ( dim == 3 ) Then
                    !> Spherical Case.
                    w_p = g%v_t(i, 1, k, :)
                    w_m = g%v_t(i, j, k, :)
                    Call mhd_hydro_calc_dg_flux3D(nx_p, ny_p, nz_p, &
                            rho_p, nrg_p, u_p, v_p, w_p, &
                            rho_m, nrg_m, u_m, v_m, w_m, &
                            fl%p%rho(i, j, k, :, :), fl%p%nrg(i, j, k, :, :), &
                            fl%p%v_r(i, j, k, :, :), fl%p%v_p(i, j, k, :, :), fl%p%v_t(i, j, k, :, :))
                    fl%p%rho(i, 0, k, :, :) = fl%p%rho(i, j, k, :, :)
                    fl%p%nrg(i, 0, k, :, :) = fl%p%nrg(i, j, k, :, :)
                    fl%p%v_r(i, 0, k, :, :) = fl%p%v_r(i, j, k, :, :)
                    fl%p%v_p(i, 0, k, :, :) = fl%p%v_p(i, j, k, :, :)
                    fl%p%v_t(i, 0, k, :, :) = fl%p%v_t(i, j, k, :, :)
                End If
            End If
        End If
        !>-------------------------------------------------------------------------------
        
        !>-------------------------------------------------------------------------------
        !> Calculate the Fluxes through Theta faces. 
        If ( dim >= 3 ) Then
            !> Spherical Case.
            If ( k /= k_max ) Then
                !> Domain Interior.
                rho_p = g%rho(i, j, k+1, :)
                nrg_p = g%nrg(i, j, k+1, :)
                u_p   = g%v_r(i, j, k+1, :)
                v_p   = g%v_p(i, j, k+1, :)
                w_p   = g%v_t(i, j, k+1, :)
                rho_m = g%rho(i, j, k+0, :)
                nrg_m = g%nrg(i, j, k+0, :)
                u_m   = g%v_r(i, j, k+0, :)
                v_m   = g%v_p(i, j, k+0, :)
                w_m   = g%v_t(i, j, k+0, :)
                Call mhd_hydro_calc_dg_flux3D(nx_t, ny_t, nz_t, &
                        rho_p, nrg_p, u_p, v_p, w_p, &
                        rho_m, nrg_m, u_m, v_m, w_m, &
                        fl%t%rho(i, j, k, :, :), fl%t%nrg(i, j, k, :, :), &
                        fl%t%v_r(i, j, k, :, :), fl%t%v_p(i, j, k, :, :), fl%t%v_t(i, j, k, :, :))
            End If
        End If
        !>-------------------------------------------------------------------------------
    End Do
    End Do
    End Do
    !$OMP End Do
End Subroutine mhd_hydro_calc_dg_fluxes
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux
