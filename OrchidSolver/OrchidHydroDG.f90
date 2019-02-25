!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_dg
Use orchid_solver_params
Use orchid_solver_grid_gauss_legendre
Use orchid_solver_hydro2
Implicit None
Type, Extends(MhdHydroSolver) :: MhdHydroSolverDG
    Contains
    Procedure, Public :: calc_flux_dg => mhd_hydro_dg_calc_flux
    Procedure, Public :: calc_step_dg => mhd_hydro_dg_calc_step
End Type MhdHydroSolverDG
Private :: mhd_hydro_dg_calc_flux, &
           mhd_hydro_dg_calc_step
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_dg_calc_flux(This, &
                                  ga, g, fl)
    !> Calculate the basic first order Fluxes.
    !> {{{
    Class(MhdHydroSolverDG), Intent(InOut) :: This
    Class(MhdGridGaussLegendre), Intent(In) :: ga
    Real(8), Dimension(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: g
    Real(8), Dimension(n_min:n_max, ga%gfnodes_min:ga%gfnodes_max), Intent(InOut) :: fl
    !> }}}
    Integer :: ip, im, j, k, n
    Real(8) :: nx, ny, nz
    Real(8), Dimension(n_min:n_max) :: w
    Real(8), Dimension(n_min:n_max) :: qp, qm
    w(:) = 0.0D0
    w(n_min:n_min+1) = 1.0D0
    !>-------------------------------------------------------------------------------
    !> Calculate the Fluxes.
    !$OMP Parallel Do Private(k, n, ip, im, nx, ny, nz, qp, qm)
    Do j = ga%nfaces_min, ga%nfaces_max
        ip = ga%faces(j)%ncell_p
        im = ga%faces(j)%ncell_m
        nx = ga%faces(j)%nx
        ny = ga%faces(j)%ny
        nz = ga%faces(j)%nz
        If ( ip >= 0 .AND. im >= 0 ) Then
            !> Domain Interior.
            Do k = ga%gfaces(j)%nnode, ga%gfaces(j)%nnode_end
                Do n = n_min, n_max
                    qp(n) = ga%lface_nodes(k)%calc(g(:, n, ip))
                    qm(n) = ga%lface_nodes(k)%calc(g(:, n, im))
                End Do
                Call This%m_flux%calc(qp, qm, fl(:, k), nx, ny, nz)
            End Do
        Else If ( ip < 0 ) Then
            !> Domain Boundary.
            Do k = ga%gfaces(j)%nnode, ga%gfaces(j)%nnode_end
                Do n = n_min, n_max
                    qm(n) = ga%lface_nodes(k)%calc(g(:, n, im))
                End Do
                If ( ip == -1 ) Then
                    !> Free flow boundary conditions.
                    Call This%m_flux%calc(qm, qm, fl(:, k), nx, ny, nz)
                Else
                    !> Wall boundary conditions.
                    Call This%m_flux%calc(qm*w, qm, fl(:, k), nx, ny, nz)
                End If
            End Do
        Else If ( im < 0 ) Then
            !> Domain Boundary.
            Do k = ga%gfaces(j)%nnode, ga%gfaces(j)%nnode_end
                Do n = n_min, n_max
                    qp(n) = ga%lface_nodes(k)%calc(g(:, n, ip))
                End Do
                If ( im == -1 ) Then
                    !> Free flow boundary conditions.
                    Call This%m_flux%calc(qp, qp, fl(:, k), nx, ny, nz)
                Else
                    !> Wall boundary conditions.
                    Call This%m_flux%calc(qp, qp*w, fl(:, k), nx, ny, nz)
                End If
            End Do
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_dg_calc_flux
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_dg_calc_step(This, Tau, ga, g, gp, fl)
    !> Calculate the Time Step.
    !> {{{
    Class(MhdHydroSolverDG), Intent(InOut) :: This
    Class(MhdGridGaussLegendre), Intent(In) :: ga
    Real(8), Dimension(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: g
    Real(8), Dimension(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(Out) :: gp
    Real(8), Dimension(n_min:n_max, ga%gfnodes_min:ga%gfnodes_max), Intent(InOut) :: fl
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
End Subroutine mhd_hydro_dg_calc_step
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_dg


