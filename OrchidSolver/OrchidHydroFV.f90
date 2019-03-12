!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_fv
Use orchid_solver_params
Use orchid_solver_grid
Use orchid_solver_hydro_flux
Implicit None
Type :: MhdHydroSolver
    Class(MhdHydroFlux), Allocatable :: m_flux
    Class(MhdHydroViscousFlux), Allocatable :: m_visc_flux
    Contains
    Procedure, Public :: init => mhd_hydro_init
    Procedure, Public :: calc_flux => mhd_hydro_calc_flux
    Procedure, Public :: calc_grad => mhd_hydro_calc_grad
    Procedure, Public :: calc_step => mhd_hydro_calc_step
End Type MhdHydroSolver
Private :: mhd_hydro_init, &
           mhd_hydro_calc_flux, &
           mhd_hydro_calc_step, &
           mhd_hydro_calc_grad
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_init(This, &
                          flux_type_opt, visc_flux_type_opt)
    !> Initialize the Finite Volume Hydro Solver.
    !> {{{
    Class(MhdHydroSolver), Intent(InOut) :: This
    Character(Len=*), Intent(In), Optional :: flux_type_opt, visc_flux_type_opt
    !> }}}
    Character(Len=10) :: flux_type, visc_flux_type
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

    !>-------------------------------------------------------------------------------
    !> Initialize the Viscosity solver.
    If ( .Not. Present(visc_flux_type_opt) ) Then
        visc_flux_type = 'gas'
    Else
        visc_flux_type(:) = visc_flux_type_opt(:)
    End If
    If ( visc_flux_type == 'gas' ) Then
        Write (*,*) 'Hydro solver: the gas Viscous Flux was selected.'
        Allocate(MhdHydroViscousFluxGas :: This%m_visc_flux)
    Else
        Write (0,*) 'Hydro viscous flux type ', Trim(flux_type), &
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
    !> Calculate the Convective Fluxes.
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
    !$OMP Parallel Do Private(ip, im, j, nx, ny, nz)
    Do j = ga%nfaces_min, ga%nfaces_max
        ip = ga%faces(j)%ncell_p
        im = ga%faces(j)%ncell_m
        nx = ga%faces(j)%nx
        ny = ga%faces(j)%ny
        nz = ga%faces(j)%nz
        If ( ip > 0 .AND. im > 0 ) Then
            !> Domain Interior.
            Call This%m_flux%calc(g(:, ip), g(:, im), fl(:, j), nx, ny, nz)
        Else If ( ip < 0 ) Then
            !> Domain Boundary.
            If ( ip == -1 ) Then
                !> Free flow boundary conditions.
                Call This%m_flux%calc(g(:, im), g(:, im), fl(:, j), nx, ny, nz)
            Else
                !> Wall boundary conditions.
                Call This%m_flux%calc(g(:, im)*w, g(:, im), fl(:, j), nx, ny, nz)
            End If
        Else If ( im < 0 ) Then
            !> Domain Boundary.
            If ( im == -1 ) Then
                !> Free flow boundary conditions.
                Call This%m_flux%calc(g(:, ip), g(:, ip), fl(:, j), nx, ny, nz)
            Else
                !> Wall boundary conditions.
                Call This%m_flux%calc(g(:, ip), g(:, ip)*w, fl(:, j), nx, ny, nz)
            End If
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_flux
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_grad(This, ga, gp, fg, fh)
    !> Calculate the Gradients on faces (and Hessians in cells).
    !> {{{
    Class(MhdHydroSolver), Intent(InOut) :: This
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: gp
    Real(8), Dimension(1:3, n_min:n_max, ga%nfaces_min:ga%nfaces_max), Intent(Out) :: fg
    Real(8), Dimension(1:3, 1:3, n_min:n_max, ga%nfaces_min:ga%nfaces_max), Intent(Out), Optional :: fh
    !> }}}
    Integer :: i, ip, im, j, n
    Real(8) :: d
    Real(8) :: nx, ny, nz
    Real(8), Dimension(1:3) :: r_p, r_m
    Real(8), Dimension(n_min:n_max) :: w
    w(:) = 0.0D0
    w(n_min:n_min+1) = 1.0D0
    !>-------------------------------------------------------------------------------
    !> Calculate the Gradients on faces.
    !> ( @todo This is incorrect in some cases (non-orthogonal grids), 
    !>   interpolate to nodes and use different gradient approximation. )
    !$OMP Parallel Do Private(ip, im, j, n, r_p, r_m, nx, ny, nz, d)
    Do j = ga%nfaces_min, ga%nfaces_max
        ip = ga%faces(j)%ncell_p
        im = ga%faces(j)%ncell_m
        nx = ga%faces(j)%nx
        ny = ga%faces(j)%ny
        nz = ga%faces(j)%nz
        If ( ip > 0 ) Then
            !> Domain Interior.
            r_p = [ga%cells(ip)%x, ga%cells(ip)%y, ga%cells(ip)%z]
        Else
            !> Domain Boundary.
            r_p = [ga%faces(j)%x, ga%faces(j)%y, ga%faces(j)%z]
        End If
        If ( im > 0 ) Then
            !> Domain Interior.
            r_m = [ga%cells(im)%x, ga%cells(im)%y, ga%cells(im)%z]
        Else
            !> Domain Boundary.
            r_m = [ga%faces(j)%x, ga%faces(j)%y, ga%faces(j)%z]
        End If
        d = Sqrt(Dot_Product(r_p - r_m, r_p - r_m))
        If ( ip > 0 .AND. im > 0 ) Then
            !> Domain Interior.
            Do n = n_min, n_max
                fg(:, n, j) = ( gp(n, ip) - gp(n, im) )/d*[nx, ny, nz]
            End Do
        Else If ( ip < 0 ) Then
            !> Domain Boundary.
            If ( ip == -1 ) Then
                !> Free flow boundary conditions.
                fg(:, :, j) = 0.0D0
            Else
                !> Wall boundary conditions.
                Do n = n_min, n_max
                    fg(:, n, j) = ( gp(n, im)*w(n) - gp(n, im) )/d*[nx, ny, nz]
                End Do
            End If
        Else If ( im < 0 ) Then
            !> Domain Boundary.
            If ( im == -1 ) Then
                !> Free flow boundary conditions.
                fg(:, :, j) = 0.0D0
            Else
                !> Wall boundary conditions.
                Do n = n_min, n_max
                    fg(:, n, j) = ( gp(n, ip) - gp(n, ip)*w(n) )/d*[nx, ny, nz]
                End Do
            End If
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Calculate the Hessians in cells.
    If ( Present(fh) ) Then
        !$OMP Parallel Do
        Do i = ga%ncells_min, ga%ncells_max
            !> @todo Implement me.
        End Do
        !$OMP End Parallel Do
    End If
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_grad
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_calc_step(This, Tau, ga, g, gp)
    !> Calculate the Time Step.
    !> {{{
    Class(MhdHydroSolver), Intent(InOut) :: This
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(InOut) :: g
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(InOut) :: gp
    Real(8), Intent(In) :: Tau
    !> }}}
    Integer :: i, j, jj
    Real(8), Dimension(n_min:n_max) :: dg, df, df_conv, df_visc
    Real(8), Dimension(:, :), Allocatable, Save :: fl
    Real(8), Dimension(:, :, :), Allocatable, Save :: fg
    !>-------------------------------------------------------------------------------
    !> Calculate the Convective Fluxes and Gradients.
    If ( .NOT. Allocated(fl) ) Then
        Allocate(fl(n_min:n_max, ga%nfaces_min:ga%nfaces_max))
        Allocate(fg(1:3, n_min:n_max, ga%nfaces_min:ga%nfaces_max))
        fl(:, :) = 0.0D0
        fg(:, :, :) = 0.0D0
    End If
    Call This%calc_flux(ga, g, fl)
    Call This%calc_grad(ga, g, fg)
    !> Calculate the updated Field values.
    !$OMP Parallel Do Private(i, j, jj, dg, df, df_conv, df_visc)
    Do i = ga%ncells_min, ga%ncells_max
        !> Update the values.               .
        dg(:) = 0.0D0
        Do jj = ga%cells(i)%nface, ga%cells(i)%nface_end
            j = ga%cell2face(jj)
            df_visc(:) = 0.0D0
            df_conv(:) = fl(:, j)
            If ( Allocated(This%m_visc_flux) ) Then
                Call This%m_visc_flux%calc(g(:, i), fg(1, :, j)*ga%faces(j)%nx + &
                                                    fg(2, :, j)*ga%faces(j)%ny + &
                                                    fg(3, :, j)*ga%faces(j)%nz, df_visc(:))
            End If
            df = df_conv(:) - df_visc(:)
            If ( ga%faces(j)%ncell_p == i ) Then
                !> Inner normal case.
                dg(:) = dg(:) - df(:)*ga%faces(j)%Sface
            Else
                !> Outer normal case.
                dg(:) = dg(:) + df(:)*ga%faces(j)%Sface
            End If
        End Do
        dg(:) = dg(:)/ga%cells(i)%Vcell
        gp(:, i) = g(:, i) - Tau*dg(:)      
        !> Check if values are correct and density and energy are positive.
        If ( Any(IsNan(gp(:, i))) .OR. Any(gp(1:2, i) <= 0.0) ) Then
            !$OMP Critical
            If ( verbose ) Then
                Write (0,*) 'Invalid flow paramaters were detected at (conv): ', gp(:, i)
            End If
            Error Stop 1
            !$OMP End Critical
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_calc_step
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_fv



