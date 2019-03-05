!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro2
Use orchid_solver_params
Use orchid_solver_grid
Use orchid_solver_hydro_flux
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
                          flux_type_opt)
    !> Initialize the Hydro Solver.
    !> {{{
    Class(MhdHydroSolver), Intent(InOut) :: This
    Character(Len=*), Intent(In), Optional :: flux_type_opt
    !> }}}
    Character(Len=10) :: flux_type
    !>-------------------------------------------------------------------------------
    !> Initialize the Riemann Solver.
    If ( .Not. Present(flux_type_opt) ) Then
        !> HLL Flux family are good enough for being default.
        If ( mhd ) Then
            !> @todo Here should be HLLD.
            flux_type = 'hll'
        Else
            flux_type = 'hllc'
        End If
    Else
        flux_type(:) = flux_type_opt(:)
    End If
    If ( flux_type == 'llf' ) Then
        Write (*,*) 'Hydro solver: the LLF/Rusanov Flux was selected.'
        Allocate(MhdHydroFluxLLF :: This%m_flux)
    Else If ( flux_type == 'hll' ) Then
        Write (*,*) 'Hydro solver: the HLL Flux was selected.'
        Allocate(MhdHydroFluxHLL :: This%m_flux)
    Else If ( flux_type == 'hllc' ) Then
        Write (*,*) 'Hydro solver: the HLLC Flux was selected.'
        Allocate(MhdHydroFluxHLLC :: This%m_flux)
    Else If ( flux_type == 'hlld' ) Then
        Write (*,*) 'Hydro solver: the HLLD Flux was selected.'
        Allocate(MhdHydroFluxHLLD :: This%m_flux)
    Else If ( flux_type == 'roe' ) Then
        Write (*,*) 'Hydro solver: the Roe Flux was selected.'
        Allocate(MhdHydroFluxRoe :: This%m_flux)
    Else
        Write (0,*) 'Hydro flux type ', Trim(flux_type), &
                    'is invalid. Please, check the manual.'
        Error Stop -100
    End If
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_init
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_flux(This, &
                               ga, g, fl)
    !> Calculate the basic first order Fluxes.
    !> {{{
    Class(MhdHydroSolver), Intent(InOut) :: This
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: g
    Real(8), Dimension(n_min:n_max, ga%nfaces_min:ga%nfaces_max), Intent(InOut) :: fl
    !> }}}
    Integer :: ip, im, j
    Real(8) :: nx, ny, nz
    Real(8), Dimension(n_min:n_max) :: w
    w(:) = 0.0D0
    w(n_min:n_min+1) = 1.0D0
    !>-------------------------------------------------------------------------------
    !> Calculate the Fluxes.
    !$OMP Parallel Do Private(ip, im, nx, ny, nz)
    Do j = ga%nfaces_min, ga%nfaces_max
        ip = ga%faces(j)%ncell_p
        im = ga%faces(j)%ncell_m
        nx = ga%faces(j)%nx
        ny = ga%faces(j)%ny
        nz = ga%faces(j)%nz
        If ( ip >= 0 .AND. im >= 0 ) Then
            !> Domain Interior.
            Call This%m_flux%calc(g(:, ip), g(:, im), fl(:, j), nx, ny, nz)
        Else If ( ip < 0 ) Then
            !> Domain Boundary.
            If ( ip == -1 ) Then
                !> Free flow boundary conditions.
                Call This%m_flux%calc(g(:, im), g(:, im), fl(:, j), nx, ny, nz)
            Else
                !> Wall boundary conditions.
                Call This%m_flux%calc(g(:, im)*w, &
                                      g(:, im), &
                                      fl(:, j), nx, ny, nz)
            End If
        Else If ( im < 0 ) Then
            !> Domain Boundary.
            If ( im == -1 ) Then
                !> Free flow boundary conditions.
                Call This%m_flux%calc(g(:, ip), g(:, ip), fl(:, j), nx, ny, nz)
            Else
                !> Wall boundary conditions.
                Call This%m_flux%calc(g(:, ip), &
                                      g(:, ip)*w, &
                                      fl(:, j), nx, ny, nz)
            End If
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_step(This, Tau, ga, g, gp, fl)
    !> Calculate the Time Step.
    !> {{{
    Class(MhdHydroSolver), Intent(InOut) :: This
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: g
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(Out) :: gp
    Real(8), Dimension(n_min:n_max, ga%nfaces_min:ga%nfaces_max), Intent(InOut) :: fl
    Real(8), Intent(In) :: Tau
    !> }}}
    Integer :: i, j, k
    Call This%calc_flux(ga, g, fl)
    !>-------------------------------------------------------------------------------
    !> Calculate the new Field values.
    !$OMP Parallel Do Private(i, j, k)
    Do i = ga%ncells_min, ga%ncells_max
        !> Update the values.
        gp(:, i) = 0.0D0
        Do k = ga%cells(i)%nface, ga%cells(i)%nface_end
            j = ga%cell2face(k)
            If ( ga%faces(j)%ncell_p == i ) Then
                !> Inner normal case.
                gp(:, i) = gp(:, i) - fl(:, j)*ga%faces(j)%Sface
            Else
                !> Outer normal case.
                gp(:, i) = gp(:, i) + fl(:, j)*ga%faces(j)%Sface
            End If
        End Do
        gp(:, i) = g(:, i) - Tau/ga%cells(i)%Vcell*gp(:, i)        
        !> Check if values are correct and density and energy are positive.
        If ( Any(IsNan(gp(:, i))) .OR. Any(gp(1:2, i) <= 0.0) ) Then
            If ( verbose ) Then
                Write (0,*) 'Invalid flow paramaters were detected at: ', gp(:, i)
            End If
            Error Stop 1
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_step
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro2



