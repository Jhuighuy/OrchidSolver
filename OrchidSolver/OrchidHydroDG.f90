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
    Real(8), Dimension(n_min:n_max, ga%nface_nodes_min:ga%nface_nodes_max), Intent(InOut) :: fl
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
                    qp(n) = Dot_Product(g(:, n, ip), ga%face_node_funcs(:, 1, k)%psi)
                    qm(n) = Dot_Product(g(:, n, im), ga%face_node_funcs(:, 2, k)%psi)
                End Do
                Call This%m_flux%calc(qp, qm, fl(:, k), nx, ny, nz)
            End Do
        Else If ( ip < 0 ) Then
            !> Domain Boundary.
            Do k = ga%gfaces(j)%nnode, ga%gfaces(j)%nnode_end
                Do n = n_min, n_max
                    qm(n) = Dot_Product(g(:, n, im), ga%face_node_funcs(:, 2, k)%psi)
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
                    qp(n) = Dot_Product(g(:, n, ip), ga%face_node_funcs(:, 1, k)%psi)
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
    Real(8), Dimension(n_min:n_max, ga%nface_nodes_min:ga%nface_nodes_max), Intent(InOut) :: fl
    Real(8), Intent(In) :: Tau
    !> }}}
    Integer :: i, j, jj, k, n, m
    Real(8) :: grad, grad_nx, grad_ny, grad_nz
    Real(8), Dimension(n_min:n_max) :: qk, fk
    Call This%calc_flux_dg(ga, g, fl)
    !>-------------------------------------------------------------------------------
    !> Calculate the new Field values.
    !$OMP Parallel Do Private(j, jj, k, n, m, grad, grad_nx, grad_ny, grad_nz, qk, fk)
    Do i = ga%ncells_min, ga%ncells_max
        gp(:, :, i) = 0.0D0
        !> Calculate the face flux increment.
        Do jj = ga%cells(i)%nface, ga%cells(i)%nface_end
            j = ga%cell2face(jj)
            Do k = ga%gfaces(j)%nnode, ga%gfaces(j)%nnode_end
                If ( ga%faces(j)%ncell_p == i ) Then
                    !> Inner normal case.
                    Do n = n_min, n_max
                        gp(:, n, i) = gp(:, n, i) - fl(n, k)*ga%faces(j)%Sface* &
                                                             ga%face_nodes(k)%w* &
                                                             ga%face_node_funcs(:, 1, k)%psi
                    End Do
                Else
                    !> Outer normal case.
                    Do n = n_min, n_max
                        gp(:, n, i) = gp(:, n, i) + fl(n, k)*ga%faces(j)%Sface* &
                                                             ga%face_nodes(k)%w* &
                                                             ga%face_node_funcs(:, 2, k)%psi
                    End Do
                End If
            End Do
        End Do
        If ( m_max > m_min ) Then
            !> Calculate the cell flux increment.
            Do k = ga%gcells(i)%nnode, ga%gcells(i)%nnode_end
                Do n = n_min, n_max
                    qk(n) = Dot_Product(g(:, n, i), ga%cell_node_funcs(:, k)%psi)
                End Do
                Do m = m_min, m_max
                    grad = ga%cell_node_funcs(m, k)%grad
                    If ( grad > 0.0D0 ) Then
                        !> Non-zero gradient.
                        grad_nx = ga%cell_node_funcs(m, k)%grad_nx
                        grad_ny = ga%cell_node_funcs(m, k)%grad_ny
                        grad_nz = ga%cell_node_funcs(m, k)%grad_nz
                        Call This%m_flux%calc(qk, qk, fk, grad_nx, grad_ny, grad_nz)
                        Do n = n_min, n_max
                            gp(m, n, i) = gp(m, n, i) - fk(n)*ga%cells(i)%Vcell* &
                                                              ga%cell_nodes(k)%w*grad
                        End Do
                    End If
                End Do
            End Do
        End If
        gp(:, :, i) = g(:, :, i) - Tau/ga%cells(i)%Vcell*gp(:, :, i)        
        !> Check if values are correct and density and energy are positive.
        !If ( Any(IsNan(gp(:, i))) .OR. Any(gp(1:2, i) <= 0.0) ) Then
        !    If ( verbose ) Then
        !        Write (0,*) 'Invalid flow paramaters were detected at: ', gp(:, i)
        !    End If
        !    Error Stop 1
        !End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_dg_calc_step
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_dg


