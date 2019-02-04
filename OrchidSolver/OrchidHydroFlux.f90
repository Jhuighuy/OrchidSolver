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
Subroutine mhd_hydro_calc_flux1D(nx, &
                                 ip, rho_vp, nrg_vp, u_vp, &
                                 im, rho_vm, nrg_vm, u_vm, &
                                 flux_rho, flux_nrg, flux_u)
    !> Calculate the Discontinuous Galerkin Fluxes in 1D.
    !> {{{
    Real(8), Intent(In) :: nx
    Real(8), Intent(In) :: ip
    Real(8), Intent(In) :: im
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    Real(8) :: x
    Real(8) :: rho_p, nrg_p, u_p, &
               rho_m, nrg_m, u_m
    !>-------------------------------------------------------------------------------
    !> Calculate the Fields values.
    !>-------------------------------------------------------------------------------
    x = 0.5D0*( ip + im )*h_r
    !rho_p = mhd_galerkin1D(rho_vp, x, ip)
    !nrg_p = mhd_galerkin1D(nrg_vp, x, ip)/rho_p
    !u_p = mhd_galerkin1D(u_vp, x, ip)/rho_p
    !rho_m = mhd_galerkin1D(rho_vm, x, im)
    !nrg_m = mhd_galerkin1D(nrg_vm, x, im)/rho_m
    !u_m = mhd_galerkin1D(u_vm, x, im)/rho_m
    
    !>-------------------------------------------------------------------------------
    !> Calculate the Fluxes.
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
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_flux2D(nx, ny, &
                                 ip, jp, rho_vp, nrg_vp, u_vp, v_vp, &
                                 im, jm, rho_vm, nrg_vm, u_vm, v_vm, &
                                 flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the Discontinuous Galerkin Fluxes in 2D.
    !> {{{
    Real(8), Intent(In) :: nx, ny
    Real(8), Intent(In) :: ip, jp
    Real(8), Intent(In) :: im, jm
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp, v_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm, v_vm
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_v, flux_u
    !> }}}
    Real(8) :: x, y
    Real(8) :: rho_p, nrg_p, u_p, v_p, &
               rho_m, nrg_m, u_m, v_m
    Real(8) :: rho_pm, nrg_pm, u_pm, v_pm, &
               rho_pp, nrg_pp, u_pp, v_pp
    !>-------------------------------------------------------------------------------
    !> Calculate the Fields values.
    !>-------------------------------------------------------------------------------
    x = 0.5D0*( ip + im )*h_r
    y = 0.5D0*( jp + jm )*h_p
    !If ( N_Funcs <= 1 ) Then
    !    !> Piecewise approximation, no integration is required.
    !    rho_p = mhd_galerkin2D(rho_vp, x, y, ip, jp)
    !    nrg_p = mhd_galerkin2D(nrg_vp, x, y, ip, jp)/rho_p
    !    u_p = mhd_galerkin2D(u_vp, x, y, ip, jp)/rho_p
    !    v_p = mhd_galerkin2D(v_vp, x, y, ip, jp)/rho_p
    !    rho_m = mhd_galerkin2D(rho_vm, x, y, im, jm)
    !    nrg_m = mhd_galerkin2D(nrg_vm, x, y, im, jm)/rho_m
    !    u_m = mhd_galerkin2D(u_vm, x, y, im, jm)/rho_m
    !    v_m = mhd_galerkin2D(v_vm, x, y, im, jm)/rho_m
    !Else
    !    !> Gauss Integration.
    !    rho_p = 0.0D0
    !    rho_p = rho_p &
    !        + mhd_galerkin2D(rho_vp, x + nx*h_r, y + ny*h_p, ip, jp) &
    !        + mhd_galerkin2D(rho_vp, x - nx*h_r, y - ny*h_p, ip, jp) &
    !End If
    
    !>-------------------------------------------------------------------------------
    !> Calculate the Fluxes.
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
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_flux3D(nx, ny, nz, &
                                 ip, jp, kp, rho_vp, nrg_vp, u_vp, v_vp, w_vp, &
                                 im, jm, km, rho_vm, nrg_vm, u_vm, v_vm, w_vm, &
                                 flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the Discontinuous Galerkin Fluxes in 3D.
    !> {{{
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: ip, jp, kp
    Real(8), Intent(In) :: im, jm, km
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vp, nrg_vp, u_vp, v_vp, w_vp
    Real(8), Dimension(m_min:m_max), Intent(In) :: rho_vm, nrg_vm, u_vm, v_vm, w_vm
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    Real(8) :: x, y, z
    Real(8) :: rho_p, nrg_p, u_p, v_p, w_p, &
               rho_m, nrg_m, u_m, v_m, w_m
    !>-------------------------------------------------------------------------------
    !> Calculate the Fields values.
    !>-------------------------------------------------------------------------------
    x = 0.5D0*( ip + im )*h_r
    y = 0.5D0*( jp + jm )*h_p
    z = 0.5D0*( kp + km )*h_t
    
    !>-------------------------------------------------------------------------------
    !> Calculate the Fluxes.
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
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux
    