!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_flux_roe
Use orchid_solver_params
Implicit None
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_roe1D(nx, &
                                     rho_p, nrg_p, u_p, &
                                     rho_m, nrg_m, u_m, &
                                     flux_rho, flux_nrg, flux_u)
    !> Calculate the Roe-Einfeldt Fluxes in 1D.
    !> {{{
    Real(8), Intent(In) :: nx
    Real(8), Intent(In) :: rho_p, nrg_p, u_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, &
               e_s, h_s, ent_s, a_s, c_s, c2_s, rho_s, u_s
    Real(8), Dimension(1:3) :: q_p, f_p, &
                               q_m, f_m, &
                               q_s, f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx
    q_p  = [ rho_p, rho_p*u_p, rho_p*nrg_p ]
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx
    q_m  = [ rho_m, rho_m*u_m, rho_m*nrg_m ]
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*a_m*ent_m ]
    !> Calculate *Values.
    rho_s = Sqrt(rho_p*rho_m)
    ent_s = ( Sqrt(rho_p)*ent_p + Sqrt(rho_m)*ent_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
    u_s  = ( Sqrt(rho_p)*u_p + Sqrt(rho_m)*u_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
    a_s  = u_s*nx
    e_s  = 0.5D0*( u_s**2 )
    h_s  = ent_s + e_s
    c2_s = ( Sqrt(rho_p)*c2_p + Sqrt(rho_m)*c2_m )/( Sqrt(rho_p) + Sqrt(rho_m) ) &
            + 0.5D0*Gamma1*rho_s/(rho_p + 2.0D0*rho_s + rho_m)*( u_p**2 )
    c_s  = Sqrt(Max(c2_s, 1D-10))
    !>-------------------------------------------------------------------------------

    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    q_s = 0.5D0*( q_p - q_m )
    !> Multiply by the Left Eigenvectors.
    q_s = [ Dot_Product([ Gamma1*e_s + c_s*a_s, &
                         -Gamma1*u_s - c_s*nx, Gamma1 ], q_s)/( 2.0D0*c2_s ), &
            Dot_Product([ c2_s - Gamma1*e_s, &
                          Gamma1*u_s, -Gamma1 ], q_s)/( c2_s ), &
            Dot_Product([ Gamma1*e_s - c_s*a_s, &
                         -Gamma1*u_s + c_s*nx, Gamma1 ], q_s)/( 2.0D0*c2_s ) ]
    !> Multiply by the Eigenvalues.
    q_s = [ Min(Abs(a_s - c_s), Abs(a_m - c_m)), Abs(a_s), &
            Max(Abs(a_s + c_s), Abs(a_p + c_p)) ] * q_s
    !> Multiply by the Right Eigenvectors.
    q_s = [ Dot_Product([ 1.0D0, 1.0D0, 1.0D0 ], q_s), &
            Dot_Product([ u_s - c_s*nx,  u_s, u_s + c_s*nx ], q_s), &
            Dot_Product([ h_s - c_s*a_s, e_s, h_s + c_s*a_s ], q_s) ]
    f_s = 0.5D0*( f_p + f_m ) - q_s
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_nrg = f_s(3)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_roe1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_roe2D(nx, ny, &
                                     rho_p, nrg_p, u_p, v_p, &
                                     rho_m, nrg_m, u_m, v_m, &
                                     flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the Roe-Einfeldt Fluxes in 2D.
    !> {{{
    Real(8), Intent(In) :: nx, ny
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, &
               e_s, h_s, ent_s, a_s, c_s, c2_s, rho_s, u_s, v_s
    Real(8), Dimension(1:4) :: q_p, f_p, &
                               q_m, f_m, &
                               q_s, f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny
    q_p  = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*nrg_p ]
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny
    q_m  = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*nrg_m ]
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, rho_m*a_m*ent_m ]
    !> Calculate *Values.
    rho_s = Sqrt(rho_p*rho_m)
    ent_s = ( Sqrt(rho_p)*ent_p + Sqrt(rho_m)*ent_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
    u_s  = ( Sqrt(rho_p)*u_p + Sqrt(rho_m)*u_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
    v_s  = ( Sqrt(rho_p)*v_p + Sqrt(rho_m)*v_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
    a_s  = u_s*nx + v_s*ny
    e_s  = 0.5D0*( u_s**2 + v_s**2 )
    h_s  = ent_s + e_s
    c2_s = ( Sqrt(rho_p)*c2_p + Sqrt(rho_m)*c2_m )/( Sqrt(rho_p) + Sqrt(rho_m) ) &
            + 0.5D0*Gamma1*rho_s/(rho_p + 2.0D0*rho_s + rho_m)*( (u_p - u_m)**2 + (v_p - v_m)**2 )
    c_s  = Sqrt(Max(c2_s, 1D-10))
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    q_s = 0.5D0*( q_p - q_m )
    !> Multiply by the Left Eigenvectors.
    q_s = [ Dot_Product([ Gamma1*e_s + c_s*a_s, &
                         -Gamma1*u_s - c_s*nx, &
                         -Gamma1*v_s - c_s*ny, Gamma1 ], q_s)/( 2.0D0*c2_s ), &
            Dot_Product([ c2_s - Gamma1*e_s, &
                          Gamma1*u_s, &
                          Gamma1*v_s, -Gamma1 ], q_s)/( c2_s ), &
            Dot_Product([ Gamma1*e_s - c_s*a_s, &
                         -Gamma1*u_s + c_s*nx, &
                         -Gamma1*v_s + c_s*ny, Gamma1 ], q_s)/( 2.0D0*c2_s ), &
            Dot_Product([ v_s*nx - u_s*ny, +ny, -nx, 0.0D0 ], q_s) ]
    !> Multiply by the Eigenvalues.
    q_s = [ Min(Abs(a_s - c_s), Abs(a_m - c_m)), Abs(a_s), &
            Max(Abs(a_s + c_s), Abs(a_p + c_p)), Abs(a_s) ] * q_s
    !> Multiply by the Right Eigenvectors.
    q_s = [ Dot_Product([ 1.0D0, 1.0D0, 1.0D0, 0.0D0 ], q_s), &
            Dot_Product([ u_s - c_s*nx,  u_s, u_s + c_s*nx, +ny ], q_s), &
            Dot_Product([ v_s - c_s*ny,  v_s, v_s + c_s*ny, -nx ], q_s), &
            Dot_Product([ h_s - c_s*a_s, e_s, h_s + c_s*a_s, u_s*ny - v_s*nx ], q_s) ]
    f_s = 0.5D0*( f_p + f_m ) - q_s
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_nrg = f_s(4)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_roe2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_roe3D(nx, ny, nz, &
                                     rho_p, nrg_p, u_p, v_p, w_p, &
                                     rho_m, nrg_m, u_m, v_m, w_m, &
                                     flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the Roe-Einfeldt Fluxes in 3D.
    !> {{{
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, &
               e_s, h_s, ent_s, a_s, c_s, c2_s, rho_s, u_s, v_s, w_s
    Real(8), Dimension(1:5) :: q_p, f_p, &
                               q_m, f_m, &
                               q_s, f_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 + w_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny + w_p*nz
    q_p  = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*w_p, rho_p*nrg_p ]
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, &
                        rho_p*w_p*a_p + p_p*nz, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 + w_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny + w_m*nz
    q_m  = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*w_m, rho_m*nrg_m ]
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, &
                        rho_m*w_m*a_m + p_m*nz, rho_m*a_m*ent_m ]
    !> Calculate *Values.
    rho_s = Sqrt(rho_p*rho_m)
    ent_s = ( Sqrt(rho_p)*ent_p + Sqrt(rho_m)*ent_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
    u_s  = ( Sqrt(rho_p)*u_p + Sqrt(rho_m)*u_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
    v_s  = ( Sqrt(rho_p)*v_p + Sqrt(rho_m)*v_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
    w_s  = ( Sqrt(rho_p)*w_p + Sqrt(rho_m)*w_m )/( Sqrt(rho_p) + Sqrt(rho_m) )
    a_s  = u_s*nx + v_s*ny + w_s*nz
    e_s  = 0.5D0*( u_s**2 + v_s**2 + w_s**2 )
    h_s  = ent_s + e_s
    c2_s = ( Sqrt(rho_p)*c2_p + Sqrt(rho_m)*c2_m )/( Sqrt(rho_p) + Sqrt(rho_m) ) &
            + 0.5D0*Gamma1*rho_s/(rho_p + 2.0D0*rho_s + rho_m)*( (u_p - u_m)**2 + (v_p - v_m)**2 + &
                                                                 (w_p - w_m)**2 )
    c_s  = Sqrt(Max(c2_s, 1D-10))
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Fluxes.
    q_s = 0.5D0*( q_p - q_m )
    If ( Abs(nx) .GE. Max(Abs(ny), Abs(nz)) ) Then
        !> Select case with Nx!=0 to prevent singularities.
        !> Multiply by the Left Eigenvectors.
        q_s = [ Dot_Product([ Gamma1*e_s + c_s*a_s, &
                             -Gamma1*u_s - c_s*nx, &
                             -Gamma1*v_s - c_s*ny, &
                             -Gamma1*w_s - c_s*nz, Gamma1 ], q_s)/( 2.0D0*c2_s ), &
                Dot_Product([ c2_s - Gamma1*e_s, &
                              Gamma1*u_s, &
                              Gamma1*v_s, &
                              Gamma1*w_s, -Gamma1 ], q_s)/( c2_s ), &
                Dot_Product([ Gamma1*e_s - c_s*a_s, &
                             -Gamma1*u_s + c_s*nx, &
                             -Gamma1*v_s + c_s*ny, &
                             -Gamma1*w_s + c_s*nz, Gamma1 ], q_s)/( 2.0D0*c2_s ), &
                Dot_Product([ ( v_s - a_s*ny )/nx, +ny, ( ny**2 - 1.0D0 )/nx, ( +ny*nz )/nx, 0.0D0 ], q_s), &
                Dot_Product([ ( a_s*nz - w_s )/nx, -nz, ( -ny*nz )/nx, ( 1.0D0 - nz**2 )/nx, 0.0D0 ], q_s) ]
        !> Multiply by the Eigenvalues.
        q_s = [ Min(Abs(a_s - c_s), Abs(a_m - c_m)), Abs(a_s), &
                Max(Abs(a_s + c_s), Abs(a_p + c_p)), Abs(a_s), Abs(a_s) ] * q_s
        !> Multiply by the Right Eigenvectors.
        q_s = [ Dot_Product([ 1.0D0, 1.0D0, 1.0D0, 0.0D0, 0.0D0 ], q_s), &
                Dot_Product([ u_s - c_s*nx, u_s, u_s + c_s*nx, +ny,   -nz   ], q_s), &
                Dot_Product([ v_s - c_s*ny, v_s, v_s + c_s*ny, -nx,   0.0D0 ], q_s), &
                Dot_Product([ w_s - c_s*nz, w_s, w_s + c_s*nz, 0.0D0, +nx   ], q_s), &
                Dot_Product([ h_s - c_s*a_s, e_s, h_s + c_s*a_s, u_s*ny - v_s*nx, w_s*nx - u_s*nz ], q_s) ]
    Else If ( Abs(ny) .GE. Max(Abs(nx), Abs(nz)) ) Then
        !> Select case with Ny!=0 to prevent singularities.
        !> Multiply by the Left Eigenvectors.
        q_s = [ Dot_Product([ Gamma1*e_s + c_s*a_s, &
                             -Gamma1*u_s - c_s*nx, &
                             -Gamma1*v_s - c_s*ny, &
                             -Gamma1*w_s - c_s*nz, Gamma1 ], q_s)/( 2.0D0*c2_s ), &
                Dot_Product([ c2_s - Gamma1*e_s, &
                              Gamma1*u_s, &
                              Gamma1*v_s, &
                              Gamma1*w_s, -Gamma1 ], q_s)/( c2_s ), &
                Dot_Product([ Gamma1*e_s - c_s*a_s, &
                             -Gamma1*u_s + c_s*nx, &
                             -Gamma1*v_s + c_s*ny, &
                             -Gamma1*w_s + c_s*nz, Gamma1 ], q_s)/( 2.0D0*c2_s ), &
                Dot_Product([ ( a_s*nx - u_s )/nx, ( 1.0D0 - nx**2 )/ny, -nx, ( -nx*nz )/ny, 0.0D0 ], q_s), &
                Dot_Product([ ( w_s - a_s*nz )/ny, ( +nx*nz )/ny, +nz, ( nz**2 - 1.0D0 )/ny, 0.0D0 ], q_s) ]
        !> Multiply by the Eigenvalues.
        q_s = [ Min(Abs(a_s - c_s), Abs(a_m - c_m)), Abs(a_s), &
                Max(Abs(a_s + c_s), Abs(a_p + c_p)), Abs(a_s), Abs(a_s) ] * q_s
        !> Multiply by the Right Eigenvectors.
        q_s = [ Dot_Product([ 1.0D0, 1.0D0, 1.0D0, 0.0D0, 0.0D0 ], q_s), &
                Dot_Product([ u_s - c_s*nx, u_s, u_s + c_s*nx, +ny,   0.0D0 ], q_s), &
                Dot_Product([ v_s - c_s*ny, v_s, v_s + c_s*ny, -nx,   +nz   ], q_s), &
                Dot_Product([ w_s - c_s*nz, w_s, w_s + c_s*nz, 0.0D0, -ny   ], q_s), &
                Dot_Product([ h_s - c_s*a_s, e_s, h_s + c_s*a_s, u_s*ny - v_s*nx, v_s*nz - w_s*ny ], q_s) ]
    Else If ( Abs(nz) .GE. Max(Abs(nx), Abs(ny)) ) Then
        !> Select case with Nz!=0 to prevent singularities.
        !> Multiply by the Left Eigenvectors.
        q_s = [ Dot_Product([ Gamma1*e_s + c_s*a_s, &
                             -Gamma1*u_s - c_s*nx, &
                             -Gamma1*v_s - c_s*ny, &
                             -Gamma1*w_s - c_s*nz, Gamma1 ], q_s)/( 2.0D0*c2_s ), &
                Dot_Product([ c2_s - Gamma1*e_s, &
                              Gamma1*u_s, &
                              Gamma1*v_s, &
                              Gamma1*w_s, -Gamma1 ], q_s)/( c2_s ), &
                Dot_Product([ Gamma1*e_s - c_s*a_s, &
                             -Gamma1*u_s + c_s*nx, &
                             -Gamma1*v_s + c_s*ny, &
                             -Gamma1*w_s + c_s*nz, Gamma1 ], q_s)/( 2.0D0*c2_s ), &
                Dot_Product([ ( u_s - a_s*nx )/nz, ( nx**2 - 1.0D0 )/nz, ( +nx*ny )/nz, +nx, 0.0D0 ], q_s), &
                Dot_Product([ ( a_s*ny - v_s )/nz, ( -nx*ny )/nz, ( 1.0D0 - ny**2 )/nz, +ny, 0.0D0 ], q_s) ]
        !> Multiply by the Eigenvalues.
        q_s = [ Min(Abs(a_s - c_s), Abs(a_m - c_m)), Abs(a_s), &
                Max(Abs(a_s + c_s), Abs(a_p + c_p)), Abs(a_s), Abs(a_s) ] * q_s
        !> Multiply by the Right Eigenvectors.
        q_s = [ Dot_Product([ 1.0D0, 1.0D0, 1.0D0, 0.0D0, 0.0D0 ], q_s), &
                Dot_Product([ u_s - c_s*nx, u_s, u_s + c_s*nx, -nz,   0.0D0 ], q_s), &
                Dot_Product([ v_s - c_s*ny, v_s, v_s + c_s*ny, 0.0D0, +nz   ], q_s), &
                Dot_Product([ w_s - c_s*nz, w_s, w_s + c_s*nz, +nx,   -ny   ], q_s), &
                Dot_Product([ h_s - c_s*a_s, e_s, h_s + c_s*a_s, w_s*nx - u_s*ny, v_s*nz - w_s*ny ], q_s) ]
    End If
    f_s = 0.5D0*( f_p + f_m ) - q_s
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_w   = f_s(4)
    flux_nrg = f_s(5)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_roe3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_roe

Module orchid_solver_hydro_flux_hllc
Use orchid_solver_params
Implicit None
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc1D(nx, &
                                      rho_p, nrg_p, u_p, &
                                      rho_m, nrg_m, u_m, &
                                      flux_rho, flux_nrg, flux_u)
    !> Calculate the HLLC Fluxes in 1D.
    !> {{{
    Real(8), Intent(In) :: nx
    Real(8), Intent(In) :: rho_p, nrg_p, u_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, g_p, s_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, g_m, s_m, &
               rho_s, p_s, c_s, s_s
    Real(8), Dimension(1:3) :: q_p, f_p, &
                               q_m, f_m, &
                               f_s, d_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Wave speeds.
    rho_s = 0.5D0*( rho_p + rho_m )
    c_s  = 0.5D0*( c_p + c_m )
    p_s  = Max(0.5D0*( p_p + p_m ) - 0.5D0*( a_p - a_m )*rho_s*c_s, 0.0D0)
    If ( p_s > p_p ) Then
        g_p = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_p - 1.0D0 )
        g_p = Sqrt(Max(g_p, 1D-10))
    Else
        g_p = 1.0D0
    End If
    If ( p_s > p_m ) Then
        g_m = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_m - 1.0D0 )
        g_m = Sqrt(Max(g_m, 1D-10))
    Else
        g_m = 1.0D0
    End If
    s_p = a_p + c_p*g_p
    s_m = a_m - c_m*g_m
    !> Calculate Fluxes.
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        s_s = ( ( rho_p*a_p*( s_p - a_p ) - p_p ) - ( rho_m*a_m*( s_m - a_m ) - p_m ) )/&
              ( rho_p*( s_p - a_p ) - rho_m*( s_m - a_m ) )
        p_s = ( (p_p + rho_p*( s_p - a_p )*( s_s - a_p ) ) +&
                (p_m + rho_m*( s_m - a_m )*( s_s - a_m ) ) )*0.5D0
        d_s = [ 0.0D0, nx, s_s ]
        If ( s_s <= 0.0D0 .AND. 0.0D0 <= s_p ) Then
            q_p = [ rho_p, rho_p*u_p, rho_p*nrg_p ]
            f_s = ( s_s*( s_p*q_p - f_p ) + s_p*p_s*d_s )/( s_p - s_s )
        Else If ( s_m <= 0.0D0 .AND. 0.0D0 <= s_s ) Then
            q_m = [ rho_m, rho_m*u_m, rho_m*nrg_m ]
            f_s = ( s_s*( s_m*q_m - f_m ) + s_m*p_s*d_s )/( s_m - s_s )
        End If
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_nrg = f_s(3)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hllc1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc2D(nx, ny, &
                                      rho_p, nrg_p, u_p, v_p, &
                                      rho_m, nrg_m, u_m, v_m, &
                                      flux_rho, flux_nrg, flux_u, flux_v)
    !> Calculate the HLLC Fluxes in 2D.
    !> {{{
    Real(8), Intent(In) :: nx, ny
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, g_p, s_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, g_m, s_m, &
               rho_s, p_s, c_s, s_s
    Real(8), Dimension(1:4) :: q_p, f_p, &
                               q_m, f_m, &
                               f_s, d_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Wave speeds.
    rho_s = 0.5D0*( rho_p + rho_m )
    c_s  = 0.5D0*( c_p + c_m )
    p_s  = Max(0.5D0*( p_p + p_m ) - 0.5D0*( a_p - a_m )*rho_s*c_s, 0.0D0)
    If ( p_s > p_p ) Then
        g_p = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_p - 1.0D0 )
        g_p = Sqrt(Max(g_p, 1D-10))
    Else
        g_p = 1.0D0
    End If
    If ( p_s > p_m ) Then
        g_m = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_m - 1.0D0 )
        g_m = Sqrt(Max(g_m, 1D-10))
    Else
        g_m = 1.0D0
    End If
    s_p = a_p + c_p*g_p
    s_m = a_m - c_m*g_m
    !> Calculate Fluxes.
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        s_s = ( ( rho_p*a_p*( s_p - a_p ) - p_p ) - ( rho_m*a_m*( s_m - a_m ) - p_m ) )/&
              ( rho_p*( s_p - a_p ) - rho_m*( s_m - a_m ) )
        p_s = ( (p_p + rho_p*( s_p - a_p )*( s_s - a_p ) ) +&
                (p_m + rho_m*( s_m - a_m )*( s_s - a_m ) ) )*0.5D0
        d_s = [ 0.0D0, nx, ny, s_s ]
        If ( s_s <= 0.0D0 .AND. 0.0D0 <= s_p ) Then
            q_p = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*nrg_p ]
            f_s = ( s_s*( s_p*q_p - f_p ) + s_p*p_s*d_s )/( s_p - s_s )
        Else If ( s_m <= 0.0D0 .AND. 0.0D0 <= s_s ) Then
            q_m = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*nrg_m ]
            f_s = ( s_s*( s_m*q_m - f_m ) + s_m*p_s*d_s )/( s_m - s_s )
        End If
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_nrg = f_s(4)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hllc2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_hydro_calc_flux_hllc3D(nx, ny, nz, &
                                      rho_p, nrg_p, u_p, v_p, w_p, &
                                      rho_m, nrg_m, u_m, v_m, w_m, &
                                      flux_rho, flux_nrg, flux_u, flux_v, flux_w)
    !> Calculate the HLLC Fluxes in 3D.
    !> {{{
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: rho_p, nrg_p, u_p, v_p, w_p
    Real(8), Intent(In) :: rho_m, nrg_m, u_m, v_m, w_m
    Real(8), Intent(Out) :: flux_rho, flux_nrg, flux_u, flux_v, flux_w
    !> }}}
    Real(8) :: e_p, p_p, ent_p, a_p, c_p, c2_p, g_p, s_p, &
               e_m, p_m, ent_m, a_m, c_m, c2_m, g_m, s_m, &
               rho_s, p_s, c_s, s_s
    Real(8), Dimension(1:5) :: q_p, f_p, &
                               q_m, f_m, &
                               f_s, d_s
    !>-------------------------------------------------------------------------------
    !> Calculate +Values.
    e_p  = 0.5D0*( u_p**2 + v_p**2 + w_p**2 )
    p_p  = Gamma1*rho_p*( nrg_p - e_p )
    ent_p = nrg_p + p_p/rho_p
    c2_p = Gamma*p_p/rho_p
    c_p  = Sqrt(Max(c2_p, 1D-10))
    a_p  = u_p*nx + v_p*ny
    f_p  = [ rho_p*a_p, rho_p*u_p*a_p + p_p*nx, rho_p*v_p*a_p + p_p*ny, &
                        rho_p*w_p*a_p + p_p*nz, rho_p*a_p*ent_p ]
    !> Calculate -Values.
    e_m  = 0.5D0*( u_m**2 + v_m**2 + w_m**2 )
    p_m  = Gamma1*rho_m*( nrg_m - e_m )
    ent_m = nrg_m + p_m/rho_m
    c2_m = Gamma*p_m/rho_m
    c_m  = Sqrt(Max(c2_m, 1D-10))
    a_m  = u_m*nx + v_m*ny + w_m*nz
    f_m  = [ rho_m*a_m, rho_m*u_m*a_m + p_m*nx, rho_m*v_m*a_m + p_m*ny, &
                        rho_m*w_m*a_m + p_m*nz, rho_m*a_m*ent_m ]
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate Wave speeds.
    rho_s = 0.5D0*( rho_p + rho_m )
    c_s  = 0.5D0*( c_p + c_m )
    p_s  = Max(0.5D0*( p_p + p_m ) - 0.5D0*( a_p - a_m )*rho_s*c_s, 0.0D0)
    If ( p_s > p_p ) Then
        g_p = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_p - 1.0D0 )
        g_p = Sqrt(Max(g_p, 1D-10))
    Else
        g_p = 1.0D0
    End If
    If ( p_s > p_m ) Then
        g_m = 1.0D0 + ( Gamma + 1.0D0 )/( 2.0D0*Gamma )*( p_s/p_m - 1.0D0 )
        g_m = Sqrt(Max(g_m, 1D-10))
    Else
        g_m = 1.0D0
    End If
    s_p = a_p + c_p*g_p
    s_m = a_m - c_m*g_m
    !> Calculate Fluxes.
    If ( s_p <= 0.0D0 ) Then
        f_s = f_p
    Else If ( s_m >= 0.0D0 ) Then
        f_s = f_m
    Else
        s_s = ( ( rho_p*a_p*( s_p - a_p ) - p_p ) - ( rho_m*a_m*( s_m - a_m ) - p_m ) )/&
              ( rho_p*( s_p - a_p ) - rho_m*( s_m - a_m ) )
        p_s = ( (p_p + rho_p*( s_p - a_p )*( s_s - a_p ) ) +&
                (p_m + rho_m*( s_m - a_m )*( s_s - a_m ) ) )*0.5D0
        d_s = [ 0.0D0, nx, ny, nz, s_s ]
        If ( s_s <= 0.0D0 .AND. 0.0D0 <= s_p ) Then
            q_p = [ rho_p, rho_p*u_p, rho_p*v_p, rho_p*w_p, rho_p*nrg_p ]
            f_s = ( s_s*( s_p*q_p - f_p ) + s_p*p_s*d_s )/( s_p - s_s )
        Else If ( s_m <= 0.0D0 .AND. 0.0D0 <= s_s ) Then
            q_m = [ rho_m, rho_m*u_m, rho_m*v_m, rho_m*w_m, rho_m*nrg_m ]
            f_s = ( s_s*( s_m*q_m - f_m ) + s_m*p_s*d_s )/( s_m - s_s )
        End If
    End If
    flux_rho = f_s(1)
    flux_u   = f_s(2)
    flux_v   = f_s(3)
    flux_w   = f_s(4)
    flux_nrg = f_s(5)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux_hllc3D
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_flux_hllc
