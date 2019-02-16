!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver in spherical/polar coorinates.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro2
Use orchid_solver_params
Use orchid_solver_hydro_flux_hllc
Use orchid_solver_hydro_flux_roe
Implicit None
Type :: MhdHydroSolver
    Class(MhdHydroFlux), Allocatable :: m_flux
    Contains
    Procedure, Public :: init => mhd_hydro_init
    Procedure, Public :: calc_flux => mhd_hydro_calc_flux
    Procedure, Public :: calc_step => mhd_hydro_calc_step
End Type MhdHydroSolver
Private :: mhd_hydro_init, &
           mhd_hydro_calc_flux, &
           mhd_hydro_calc_step
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_init(This, &
                          flux_type)
    !> Initialize the Hydro Solver
    !> {{{
    Class(MhdHydroSolver), Intent(InOut) :: This
    Character(Len=10), Intent(In), Optional :: flux_type
    !> }}}
    If ( .NOT. Present(flux_type) .OR. flux_type == 'hllc' ) Then
        Write (*,*) 'Hydro solver: the HLLC Flux was selected.'
        Allocate(MhdHydroFluxHLLC :: This%m_flux)
    Else If ( flux_type == 'roe' ) Then
        Write (*,*) 'Hydro solver: the Roe Flux was selected.'
        Allocate(MhdHydroFluxRoe :: This%m_flux)
    Else
        Write (0,*) 'Hydro flux type ', Trim(flux_type), &
                    'is invalid. Please, check the manual.'
        Error Stop -100
    End If
End Subroutine mhd_hydro_init
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_flux(This, &
                               g, f)
    !> Calculate the basic first order Fluxes.
    !> {{{
    Class(MhdHydroSolver), Intent(InOut) :: This
    Real(8), Dimension(n_min:n_max, &
                       i_min:i_max, &
                       j_min:j_max, &
                       k_min:k_max), Intent(In) :: g
    Real(8), Dimension(n_min:n_max, &
                       i_min-1:i_max, &
                       j_min-1:j_max, &
                       k_min-1:k_max, f_min:f_max), Intent(Out) :: f
    !> }}}
    Integer :: i, j, k
    Real(8) :: Phi_j, Phi_jph
    Real(8) :: nx_r, nx_p, nx_t
    Real(8) :: ny_r, ny_p, ny_t
    Real(8) :: nz_r, nz_p, nz_t
    !$OMP Parallel Do Private(Phi_j, Phi_jph, nx_r, nx_p, nx_t, ny_r, ny_p, ny_t, nz_r, nz_p, nz_t)
    Do i = i_min, i_max
    Do j = j_min, j_max
    Do k = k_min, k_max
        !> @todo Move this to Grid aux info.
        Phi_jph = Dble(j)*h_p
        Phi_j = Phi_jph - 0.5D0*h_p
        nx_r = Cos(Phi_j); ny_r = Sin(Phi_j); nz_r = 0
        nx_p = -Sin(Phi_jph); ny_p = Cos(Phi_jph); nz_p = 0
        nx_t = 0; ny_t = 0; nz_t = 1
        !nx_r = 1; ny_r = 0; nz_r = 0
        !nx_p = 0; ny_p = 1; nz_p = 0
        !nx_t = 0; ny_t = 0; nz_t = 1

        !>-------------------------------------------------------------------------------
        !> Calculate the Fluxes through R faces.
        If ( i /= i_max ) Then
            !> Domain Interior.
            Call This%m_flux%calc(g(:, i+1, j, k), &
                                  g(:, i+0, j, k), &
                                  f(:, i+0, j, k, 1), nx_r, ny_r, nz_r)
        Else
            If ( MHD_R0_BOUNDARY_COND == 'free' .AND. r_0 >= 0.0D0 ) Then
                !> Free flow boundary conditions.
                Call This%m_flux%calc(g(:, i_min, j, k), &
                                      g(:, i_min, j, k), &
                                      f(:, i_min-1, j, k, 1), nx_r, ny_r, nz_r)
            Else If ( MHD_R0_BOUNDARY_COND == 'wall' .AND. r_0 >= 0.0D0 ) Then
                !> Solid wall boundary conditions.
                Call This%m_flux%calc(g(:, i_min, j, k), &
                                      g(:, i_min, j, k)*[1.0D0, 1.0D0, 0.0D0, 0.0D0], &
                                      f(:, i_min-1, j, k, 1), nx_r, ny_r, nz_r)
            End If
            If ( MHD_R1_BOUNDARY_COND == 'free' ) Then
                !> Free flow boundary conditions.
                Call This%m_flux%calc(g(:, i_max, j, k), &
                                      g(:, i_max, j, k), &
                                      f(:, i_max, j, k, 1), nx_r, ny_r, nz_r)
            Else If ( MHD_R1_BOUNDARY_COND == 'wall' ) Then
                !> Solid wall boundary conditions.
                Call This%m_flux%calc(g(:, i_max, j, k)*[1.0D0, 1.0D0, 0.0D0, 0.0D0], &
                                      g(:, i_max, j, k), &
                                      f(:, i_max, j, k, 1), nx_r, ny_r, nz_r)
            End If
        End If
        !> Calculate the Fluxes through Phi faces.
        If ( dim >= 2 ) Then
            !> Polar Or Spherical Case.
            If ( j /= j_max ) Then
                !> Domain Interior.
                Call This%m_flux%calc(g(:, i, j+1, k), &
                                      g(:, i, j+0, k), &
                                      f(:, i, j+0, k, 2), nx_p, ny_p, nz_p)
            Else
                !> Periodic boundary conditions.
                Call This%m_flux%calc(g(:, i, j_min, k), &
                                      g(:, i, j_max, k), &
                                      f(:, i, j_max, k, 2), nx_p, ny_p, nz_p)
                f(:, i, j_min-1, k, 2) = f(:, i, j_max, k, 2)
            End If
        End If
        !> Calculate the Fluxes through Theta faces.
        If ( dim >= 3 ) Then
            !> Spherical Case.
            If ( k /= k_max ) Then
                !> Domain Interior.
                Call This%m_flux%calc(g(:, i, j, k+1), &
                                      g(:, i, j, k+0), &
                                      f(:, i, j, k+0, 3), nx_t, ny_t, nz_t)
            End If
        End If
        !>-------------------------------------------------------------------------------
    End Do
    End Do
    End Do
    !$OMP End Parallel Do
End Subroutine mhd_hydro_calc_flux
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_step(This, Tau, g, gp, f)
    !> Calculate the Time Step.
    !> {{{
    Class(MhdHydroSolver), Intent(InOut) :: This
    Real(8), Intent(In) :: Tau
    Real(8), Dimension(n_min:n_max, &
                       i_min:i_max, &
                       j_min:j_max, &
                       k_min:k_max), Intent(InOut) :: g, gp
    Real(8), Dimension(n_min:n_max, &
                       i_min-1:i_max, &
                       j_min-1:j_max, &
                       k_min-1:k_max, f_min:f_max), Intent(Out) :: f
    !> }}}
    Integer :: i, j, k
    Real(8) :: r_i, r_iph, r_imh, L_rp, L_rm, L_p, S
    Real(8) :: Phi_j, Phi_jph, Phi_jmh
    Real(8) :: nx_r, nx_p, nx_t
    Real(8) :: ny_r, ny_p, ny_t
    
    gp(:,:,:,:)=g(:,:,:,:)
    Call This%calc_flux(g, f)
    !$OMP Parallel Do Private(r_i, r_iph, r_imh, Phi_jph, Phi_jmh, Phi_j, L_rp, L_rm, L_p, S)
    Do i = i_min, i_max
    Do j = j_min, j_max
    Do k = k_min, k_max

        !> @todo Move this to Grid aux info.
        !> Values below may be incorrect.
        r_iph = r_0 + Dble(i)*h_r
        r_imh = r_iph - h_r
        r_i = r_iph - 0.5D0*h_r
        Phi_jph = Dble(j)*h_p
        Phi_jmh = Phi_jph - h_p
        Phi_j = Phi_jph - 0.5D0*h_p
        L_rp = 2.0D0*r_iph*Sin(0.5D0*h_p)
        L_rm = 2.0D0*r_imh*Sin(0.5D0*h_p)
        L_p = h_r!Sqrt(1.0D0 + Sin(0.5D0*h_p)**2)
        S = r_i*h_r*Sin(h_p)
        
        gp(:, i, j, k) = &
            ( L_rp*f(:, i, j, k, 1) - L_rm*f(:, i-1, j, k, 1) ) + &
            ( L_p* f(:, i, j, k, 2) - L_p* f(:, i, j-1, k, 2) )
        gp(:, i, j, k) = g(:, i, j, k) - Tau/S*gp(:, i, j, k)

        !> @todo The Unstructured grid should be here.
        ! gp(1, i, j, k) = &
        !     ( f(1, i, j, k, 1) - f(1, i-1, j, k, 1) )/h_r + &
        !     ( f(1, i, j, k, 2) - f(1, i, j-1, k, 2) )/h_p/r_i + &
        !     ( f(1, i, j, k, 1) + f(1, i-1, j, k, 1) )/(2*r_i)
        ! gp(2, i, j, k) = &
        !     ( f(2, i, j, k, 1) - f(2, i-1, j, k, 1) )/h_r + &
        !     ( f(2, i, j, k, 2) - f(2, i, j-1, k, 2) )/h_p/r_i + &
        !     ( f(2, i, j, k, 1) + f(2, i-1, j, k, 1) )/(2*r_i)
        ! gp(3, i, j, k) = &
        !     ( f(3, i, j, k, 1) - f(3, i-1, j, k, 1) )/h_r + &
        !     ( f(3, i, j, k, 2) - f(3, i, j-1, k, 2) )/h_p/r_i + &
        !     ( f(3, i, j, k, 1) + f(3, i-1, j, k, 1) )/(2*r_i) - &
        !     ( f(4, i, j, k, 2) + f(4, i, j-1, k, 2) )/(2*r_i)
        ! gp(4, i, j, k) = &
        !     ( f(4, i, j, k, 1) - f(4, i-1, j, k, 1) )/h_r + &
        !     ( f(4, i, j, k, 2) - f(4, i, j-1, k, 2) )/h_p/r_i + &
        !     ( f(4, i, j, k, 1) + f(4, i-1, j, k, 1) )/(2*r_i) + &
        !     ( f(3, i, j, k, 2) + f(3, i, j-1, k, 2) )/(2*r_i)
        ! gp(:, i, j, k) = g(:, i, j, k) - Tau*gp(:, i, j, k)
        If ( Any(IsNan(gp(:, i, j, k))) .OR. Any(gp(1:2,i,j,k) <= 0.0) ) Then
            If ( verbose ) Then
                Write (0,*) 'Invalid flow paramaters were detected at: ', gp(:, i, j, k)
            End If
            Error Stop 1
        End If
    End Do
    End Do
    End Do
    !$OMP End Parallel Do
End Subroutine mhd_hydro_calc_step
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro2


