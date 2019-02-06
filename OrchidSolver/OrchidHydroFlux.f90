!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.
    
Module orchid_solver_hydro_flux
Use orchid_solver_hydro_flux_roe
Use orchid_solver_hydro_flux_hllc
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
    Integer, Intent(In) :: nx
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm
    Real(8), Dimension(m_min:m_min, m_min:m_min), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    Integer :: m1, m2
    Real(8) :: x
    Real(8), Dimension(m_min:m_max) :: basis
    Real(8) :: rho_p, nrg_p, u_p, &
               rho_m, nrg_m, u_m
    Do m1 = m_min, m_min
    Do m2 = m_min, m_min
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
        If ( hydro_flux == 'roe' ) Then
             Call mhd_hydro_calc_flux_roe1D(Dble(nx), &
                    rho_p, nrg_p, u_p, &
                    rho_m, nrg_m, u_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2))
        Else If ( hydro_flux == 'hllc' ) Then
             Call mhd_hydro_calc_flux_hllc1D(Dble(nx), &
                    rho_p, nrg_p, u_p, &
                    rho_m, nrg_m, u_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2))
        Else
            Write (0,*) 'Hydro flux ', Trim(hydro_flux), &
                        ' is not implemented for 1D.'
            Stop 1
        End If
    End Do
    End Do
End Subroutine mhd_hydro_calc_dg_flux1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_dg_flux2D(nx, ny, &
                                    rho_vp, nrg_vp, u_vp, v_vp, &
                                    rho_vm, nrg_vm, u_vm, v_vm, &
                                    flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the Fluxes in 2D at several points on the cell edge,
    !> premutiplied by the integration weigths.
    !> {{{
    Integer, Intent(In) :: nx, ny
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp, v_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm, v_vm
    Real(8), Dimension(m_min:m_max, m_min:m_min), Intent(Out) :: flux_rho, flux_nrg, flux_v, flux_u
    !> }}}
    Integer :: np, nm
    Integer :: m1, m2
    Real(8), Dimension(m_min:m_max) :: basis
    Real(8) :: rho_p, nrg_p, u_p, v_p, &
               rho_m, nrg_m, u_m, v_m
    np = 1*nx + 2*ny
    nm = 3*nx + 4*ny
    Do m1 = m_min, m_max
    Do m2 = m_min, m_min
        !> Calculate +Values.
        basis = GaussLegendreEdge2D(np, m1, :)
        rho_p = Dot_Product(rho_vp, basis)
        nrg_p = Dot_Product(nrg_vp, basis)/rho_p
        u_p = Dot_Product(u_vp, basis)/rho_p
        v_p = Dot_Product(v_vp, basis)/rho_p
        !> Calculate -Values.
        basis = GaussLegendreEdge2D(nm, m1, :)
        rho_m = Dot_Product(rho_vm, basis)
        nrg_m = Dot_Product(nrg_vm, basis)/rho_m
        u_m = Dot_Product(u_vm, basis)/rho_m
        v_m = Dot_Product(v_vm, basis)/rho_m
        !> Calculate the Fluxes.
        If ( hydro_flux == 'roe' ) Then
            Call mhd_hydro_calc_flux_roe2D(Dble(nx), Dble(ny), &
                    rho_p, nrg_p, u_p, v_p, &
                    rho_m, nrg_m, u_m, v_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2), flux_v(m1, m2))
        Else If ( hydro_flux == 'hllc' ) Then
            Call mhd_hydro_calc_flux_hllc2D(Dble(nx), Dble(ny), &
                    rho_p, nrg_p, u_p, v_p, &
                    rho_m, nrg_m, u_m, v_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2), flux_v(m1, m2))
        Else
            Write (0,*) 'Hydro flux ', Trim(hydro_flux), &
                        ' is not implemented for 2D.'
            Stop 1
        End If
        !> Premultiply Fluxes by the Gauss weights.
        flux_rho(m1, m2) = Gauss_w(m_max, m1)*flux_rho(m1, m2)
        flux_nrg(m1, m2) = Gauss_w(m_max, m1)*flux_nrg(m1, m2)
        flux_u(m1, m2) = Gauss_w(m_max, m1)*flux_u(m1, m2)
        flux_v(m1, m2) = Gauss_w(m_max, m1)*flux_v(m1, m2)
    End Do
    End Do
End Subroutine mhd_hydro_calc_dg_flux2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_dg_flux3D(nx, ny, nz, &
                                    rho_vp, nrg_vp, u_vp, v_vp, w_vp, &
                                    rho_vm, nrg_vm, u_vm, v_vm, w_vm, &
                                    flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the Fluxes in 2D at several points on the cell edge,
    !> premutiplied by the integration weigths.
    !> {{{
    Integer, Intent(In) :: nx, ny, nz
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp, v_vp, w_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm, v_vm, w_vm
    Real(8), Dimension(m_min:m_max, m_min:m_max), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    Integer :: np, nm
    Integer :: m1, m2
    Real(8), Dimension(m_min:m_max) :: basis
    Real(8) :: rho_p, nrg_p, u_p, v_p, w_p, &
               rho_m, nrg_m, u_m, v_m, w_m
    np = 1*nx + 2*ny + 3*nz
    nm = 4*nx + 5*ny + 6*nz
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
        If ( hydro_flux == 'roe' ) Then
            Call mhd_hydro_calc_flux_roe3D(Dble(nx), Dble(ny), Dble(nz), &
                    rho_p, nrg_p, u_p, v_p, w_p, &
                    rho_m, nrg_m, u_m, v_m, w_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2), flux_v(m1, m2), flux_w(m1, m2))
        Else If ( hydro_flux == 'hllc' ) Then
            Call mhd_hydro_calc_flux_hllc3D(Dble(nx), Dble(ny), Dble(nz), &
                    rho_p, nrg_p, u_p, v_p, w_p, &
                    rho_m, nrg_m, u_m, v_m, w_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2), flux_v(m1, m2), flux_w(m1, m2))
        Else
            Write (0,*) 'Hydro flux ', Trim(hydro_flux), &
                        ' is not implemented for 3D.'
            Stop 1
        End If
        !> Premultiply Fluxes by the Gauss weights.
        flux_rho(m1, m2) = Gauss_w(m_max, m1)*Gauss_w(m_max, m2)*flux_rho(m1, m2)
        flux_nrg(m1, m2) = Gauss_w(m_max, m1)*Gauss_w(m_max, m2)*flux_nrg(m1, m2)
        flux_u(m1, m2) = Gauss_w(m_max, m1)*Gauss_w(m_max, m2)*flux_u(m1, m2)
        flux_v(m1, m2) = Gauss_w(m_max, m1)*Gauss_w(m_max, m2)*flux_v(m1, m2)
        flux_w(m1, m2) = Gauss_w(m_max, m1)*Gauss_w(m_max, m2)*flux_w(m1, m2)
    End Do
    End Do
End Subroutine mhd_hydro_calc_dg_flux3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux
