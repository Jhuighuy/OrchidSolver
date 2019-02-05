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
                                    ip, rho_vp, nrg_vp, u_vp, &
                                    im, rho_vm, nrg_vm, u_vm, &
                                    flux_rho, flux_nrg, flux_u)
    !> Calculate the Fluxes in 1D.
    !> {{{
    Real(8), Intent(In) :: nx
    Real(8), Intent(In) :: ip
    Real(8), Intent(In) :: im
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm
    Real(8), Dimension(m_min:m_min, m_min:m_min), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    Integer :: m1, m2
    Real(8) :: x
    Real(8) :: rho_p, nrg_p, u_p, &
               rho_m, nrg_m, u_m
    Do m1 = m_min, m_min
    Do m2 = m_min, m_min
        x = 0.5D0*h_r*( ip + im )
        !> Calculate +Values.
        rho_p = mhd_eval1D(rho_vp, ip, x)
        nrg_p = mhd_eval1D(nrg_vp, ip, x)/rho_p
        u_p = mhd_eval1D(u_vp, ip, x)/rho_p
        !> Calculate -Values.
        rho_m = mhd_eval1D(rho_vm, im, x)
        nrg_m = mhd_eval1D(nrg_vm, im, x)/rho_m
        u_m = mhd_eval1D(u_vm, im, x)/rho_m
        If ( hydro_flux == 'roe' ) Then
             Call mhd_hydro_calc_flux_roe1D(nx, &
                    rho_p, nrg_p, u_p, &
                    rho_m, nrg_m, u_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2))
        Else If ( hydro_flux == 'hllc' ) Then
             Call mhd_hydro_calc_flux_hllc1D(nx, &
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
                                    ip, jp, rho_vp, nrg_vp, u_vp, v_vp, &
                                    im, jm, rho_vm, nrg_vm, u_vm, v_vm, &
                                    flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the Fluxes in 2D at several points on the cell edge,
    !> premutiplied by the integration weigths.
    !> {{{
    Real(8), Intent(In) :: nx, ny
    Real(8), Intent(In) :: ip, jp
    Real(8), Intent(In) :: im, jm
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp, v_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm, v_vm
    Real(8), Dimension(m_min:m_max, m_min:m_min), Intent(Out) :: flux_rho, flux_nrg, flux_v, flux_u
    !> }}}
    Integer :: m1, m2
    Real(8) :: x, y
    Real(8) :: tx, ty
    Real(8) :: rho_p, nrg_p, u_p, v_p, &
               rho_m, nrg_m, u_m, v_m
    Real(8) :: rho_pm, nrg_pm, u_pm, v_pm, &
               rho_pp, nrg_pp, u_pp, v_pp
    !> Calculate Tangent.
    tx = -ny; ty = +nx
    Do m1 = m_min, m_max
    Do m2 = m_min, m_min
        !> Calculate point.
        x = 0.5D0*( ip + im + tx*Gauss_x(m_max, m1) )*h_r
        y = 0.5D0*( jp + jm + ty*Gauss_x(m_max, m1) )*h_p
        !> Calculate +Values.
        rho_p = mhd_eval2D(rho_vp, ip, jp, x, y)
        nrg_p = mhd_eval2D(nrg_vp, ip, jp, x, y)/rho_p
        u_p = mhd_eval2D(u_vp, ip, jp, x, y)/rho_p
        v_p = mhd_eval2D(v_vp, ip, jp, x, y)/rho_p
        !> Calculate -Values.
        rho_m = mhd_eval2D(rho_vm, im, jm, x, y)
        nrg_m = mhd_eval2D(nrg_vm, im, jm, x, y)/rho_m
        u_m = mhd_eval2D(u_vm, im, jm, x, y)/rho_m
        v_m = mhd_eval2D(v_vm, im, jm, x, y)/rho_m
        !> Calculate the Fluxes.
        If ( hydro_flux == 'roe' ) Then
            Call mhd_hydro_calc_flux_roe2D(nx, ny, &
                    rho_p, nrg_p, u_p, v_p, &
                    rho_m, nrg_m, u_m, v_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2), flux_v(m1, m2))
        Else If ( hydro_flux == 'hllc' ) Then
            Call mhd_hydro_calc_flux_hllc2D(nx, ny, &
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
                                    ip, jp, kp, rho_vp, nrg_vp, u_vp, v_vp, w_vp, &
                                    im, jm, km, rho_vm, nrg_vm, u_vm, v_vm, w_vm, &
                                    flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the Fluxes in 2D at several points on the cell edge,
    !> premutiplied by the integration weigths.
    !> {{{
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: ip, jp, kp
    Real(8), Intent(In) :: im, jm, km
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp, v_vp, w_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm, v_vm, w_vm
    Real(8), Dimension(m_min:m_max, m_min:m_max), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    Integer :: m1, m2
    Real(8) :: x, y, z
    Real(8) :: tx, ty, tz, bx, by, bz
    Real(8) :: rho_p, nrg_p, u_p, v_p, w_p, &
               rho_m, nrg_m, u_m, v_m, w_m
    !> Calculate Tangent and Bitangent.
    Write (0,*) 'Not implemented'
    Stop 100
    Do m1 = m_min, m_max
    Do m2 = m_min, m_max
        !> Calculate point.
        x = 0.5D0*h_r*( ip + im + Gauss_x(m_max, m1) + Gauss_x(m_max, m2) )
        y = 0.5D0*h_p*( jp + jm + Gauss_x(m_max, m1) + Gauss_x(m_max, m2) )
        z = 0.5D0*h_t*( kp + km + Gauss_x(m_max, m1) + Gauss_x(m_max, m2) )
        !> Calculate +Values.
        rho_p = mhd_eval3D(rho_vp, ip, jp, kp, x, y, z)
        nrg_p = mhd_eval3D(nrg_vp, ip, jp, kp, x, y, z)/rho_p
        u_p = mhd_eval3D(u_vp, ip, jp, kp, x, y, z)/rho_p
        v_p = mhd_eval3D(v_vp, ip, jp, kp, x, y, z)/rho_p
        w_p = mhd_eval3D(w_vp, ip, jp, kp, x, y, z)/rho_p
        !> Calculate -Values.
        rho_m = mhd_eval3D(rho_vm, im, jm, km, x, y, z)
        nrg_m = mhd_eval3D(nrg_vm, im, jm, km, x, y, z)/rho_m
        u_m = mhd_eval3D(u_vm, im, jm, km, x, y, z)/rho_m
        v_m = mhd_eval3D(v_vm, im, jm, km, x, y, z)/rho_m
        w_m = mhd_eval3D(w_vm, im, jm, km, x, y, z)/rho_m
        !> Calculate Fluxes.
        If ( hydro_flux == 'roe' ) Then
            Call mhd_hydro_calc_flux_roe3D(nx, ny, nz, &
                    rho_p, nrg_p, u_p, v_p, w_p, &
                    rho_m, nrg_m, u_m, v_m, w_m, &
                    flux_rho(m1, m2), flux_nrg(m1, m2), flux_u(m1, m2), flux_v(m1, m2), flux_w(m1, m2))
        Else If ( hydro_flux == 'roe' ) Then
            Call mhd_hydro_calc_flux_hllc3D(nx, ny, nz, &
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
