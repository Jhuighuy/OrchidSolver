!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_hydro_dg
Use orchid_solver_params
Use orchid_solver_grid_gauss_legendre
Use orchid_solver_hydro_fv
Implicit None
Type, Extends(MhdHydroSolver) :: MhdHydroSolverDG
    Contains
    Procedure, Public :: calc_flux_dg => mhd_hydro_dg_calc_flux
    Procedure, Public :: calc_grad_dg => mhd_hydro_dg_calc_grad
    Procedure, Public :: calc_step_dg => mhd_hydro_dg_calc_step
    Procedure, Public :: calc_limiter_dg => mhd_hydro_dg_calc_limiter
End Type MhdHydroSolverDG
Private :: mhd_hydro_dg_calc_flux, &
           mhd_hydro_dg_calc_grad, &
           mhd_hydro_dg_calc_step, &
           mhd_hydro_dg_calc_limiter
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_dg_calc_flux(This, &
                                  ga, g, fl)
    !> Calculate the Convective Fluxes.
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
        If ( ip > 0 .AND. im > 0 ) Then
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
Subroutine mhd_hydro_dg_calc_step(This, Tau, ga, g, gp)
    !> Calculate the Time Step.
    !> {{{
    Class(MhdHydroSolverDG), Intent(InOut) :: This
    Class(MhdGridGaussLegendre), Intent(In) :: ga
    Real(8), Dimension(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(InOut) :: g
    Real(8), Dimension(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(InOut) :: gp
    Real(8), Intent(In) :: Tau
    !> }}}
    Integer :: i, j, jj, k, n, m
    Real(8) :: grad, grad_nx, grad_ny, grad_nz
    Real(8), Dimension(n_min:n_max) :: qk, fk
    Real(8), Dimension(:, :), Allocatable, Save :: fl
    Real(8), Dimension(:, :, :), Allocatable, Save :: fg
    !>-------------------------------------------------------------------------------
    !> Calculate the Convective Fluxes.
    If ( .NOT. Allocated(fl) ) Then
       Allocate(fl(n_min:n_max, ga%nfaces_min:ga%nfaces_max))
       fl(:, :) = 0.0D0
    End If
    Call This%calc_flux_dg(ga, g, fl)
    !> Calculate the updated Field values (convectivity).
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
    End Do
    !$OMP End Parallel Do
    !> Calculate the Limited Values.
    If ( m_max > m_min ) Then
        Call This%calc_limiter_dg(ga, gp)
    End If
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_dg_calc_step
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_dg_calc_grad(This, ga, gp, fg, fh)
    !> Calculate the Gradients on faces (and Hessians in cells).
    !> {{{
    Class(MhdHydroSolverDG), Intent(InOut) :: This
    Class(MhdGridGaussLegendre), Intent(In) :: ga
    Real(8), Dimension(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: gp
    Real(8), Dimension(1:3, n_min:n_max, ga%nfaces_min:ga%nfaces_max), Intent(Out) :: fg
    Real(8), Dimension(1:3, 1:3, n_min:n_max, ga%nfaces_min:ga%nfaces_max), Intent(Out), Optional :: fh
    !> }}}
    Integer :: i, k, n
    Real(8), Dimension(n_min:n_max) :: qk
    Real(8), Dimension(:, :), Allocatable, Save :: gc
    !>-------------------------------------------------------------------------------
    !> Calculate the Cell Averages.
    If ( .NOT. Allocated(gc) ) Then
        Allocate(gc(n_min:n_max, ga%ncells_min:ga%ncells_max))
        gc(:, :) = 0.0D0
    End If
    !$OMP Parallel Do Private(i, k, n, qk)
    Do i = ga%ncells_min, ga%ncells_max
        gc(:, i) = 0.0D0
        Do k = ga%gcells(i)%nnode, ga%gcells(i)%nnode_end
            Do n = n_min, n_max
                qk(n) = Dot_Product(gp(:, n, i), ga%cell_node_funcs(:, k)%psi)
            End Do
            gc(:, i) = gc(:, i) + qk(:)*ga%cell_nodes(k)%w
        End Do
        !> Check if average values are correct and density and energy are positive.
        If ( Any(IsNan(gc(:, i))) .OR. Any(gc(1:2, i) <= 0.0) ) Then
            !$OMP Critical
            If ( verbose ) Then
                Write (0,*) 'Invalid average flow paramaters were detected at: ', gc(:, i)
            End If
            Error Stop 1
            !$OMP End Critical
        End If
    End Do
    !$OMP End Parallel Do
    !> Calculate the Gradients and Hessians on the cell averages.
    Call This%calc_grad(ga, gc, fg, fh)
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_dg_calc_grad
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_dg_calc_limiter(This, ga, gp)
    !> Calculate the Limited coefficients.
    !> {{{
    Class(MhdHydroSolverDG), Intent(InOut) :: This
    Class(MhdGridGaussLegendre), Intent(In) :: ga
    Real(8), Dimension(m_min:m_max, n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(InOut) :: gp
    !> }}}
    Integer :: i, j, jj, k, m, n
    Real(8), Dimension(n_min:n_max) :: qk
    Real(8), Dimension(n_min:n_max) :: grad_x, grad_y, grad_z
    Real(8), Dimension(:, :, :), Allocatable, Save :: fg
    !>-------------------------------------------------------------------------------
    !> Calculate the Gradients.
    If ( .NOT. Allocated(fg) ) Then
        Allocate(fg(1:3, n_min:n_max, ga%nfaces_min:ga%nfaces_max))
        fg(:, :, :) = 0.0D0
    End If
    Call This%calc_grad_dg(ga, gp, fg)
    !> Calculate the Linear Limited coefficients.
    !$OMP Parallel Do Private(i, j, jj, k, m, n, grad_x, grad_y, grad_z, qk)
    Do i = ga%ncells_min, ga%ncells_max
        !> Calculate the unlimited DG gradient.
        grad_x(:) = 0.0D0
        grad_y(:) = 0.0D0
        grad_z(:) = 0.0D0
        Do m = m_min+1, m_max
            Do n = n_min, n_max
                k = ga%gcells(i)%nnode
                grad_x(n) = grad_x(n) + gp(m, n, i)*ga%cell_node_funcs(m, k)%grad* &
                                                    ga%cell_node_funcs(m, k)%grad_nx
                grad_y(n) = grad_y(n) + gp(m, n, i)*ga%cell_node_funcs(m, k)%grad* &
                                                    ga%cell_node_funcs(m, k)%grad_ny
                grad_z(n) = grad_z(n) + gp(m, n, i)*ga%cell_node_funcs(m, k)%grad* &
                                                    ga%cell_node_funcs(m, k)%grad_nz
            End Do
        End Do
        !> Calculate the limited DG gradient.
        !> ( assuming the Limiter is symmetric ).
        Do jj = ga%cells(i)%nface, ga%cells(i)%nface_end
            j = ga%cell2face(jj)
            Do n = n_min, n_max
                !> @todo Replace minmod2 with more general 
                !>       limiter.
                grad_x(n) = minmod2(grad_x(n), +fg(1, n, j))
                grad_y(n) = minmod2(grad_y(n), +fg(2, n, j))
                grad_z(n) = minmod2(grad_z(n), +fg(3, n, j))
            End Do
        End Do
        !> Decompose the limited DG function.
        !> ( assuming the Basis functions are orthogonal ).
        !> ( @todo Not working )
        !gp(m_min+1:m_max, :, i) = 0.0D0
        !Do k = ga%gcells(i)%nnode, ga%gcells(i)%nnode_end
        !    Do n = n_min, n_max
        !        qk(n) = gp(m_min, n, i)*ga%cell_node_funcs(m_min, k)%psi + &
        !                    ( grad_x(n)*( ga%cell_nodes(k)%x - ga%cells(i)%x ) + & 
        !                      grad_y(n)*( ga%cell_nodes(k)%y - ga%cells(i)%y ) + &
        !                      grad_z(n)*( ga%cell_nodes(k)%z - ga%cells(i)%z ) )
        !    End Do
        !    Do m = m_min+1, m_max
        !        Do n = n_min, n_max
        !            gp(m, n, i) = gp(m, n, i) + qk(n)*ga%cell_node_funcs(m, k)%psi* &
        !                                              ga%cell_nodes(k)%w
        !        End Do
        !    End Do
        !End Do
        !k = ga%gcells(i)%nnode
        !Write(*,*) gp(1, 1, i), grad_x(1)/ga%cell_node_funcs(1, k)%grad
        Do n = n_min, n_max
            k = ga%gcells(i)%nnode
            gp(1, n, i) = grad_x(n)/ga%cell_node_funcs(1, k)%grad
        End Do
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------

    !>-------------------------------------------------------------------------------
    !> Calculate the Hessians.
    !> ( @todo Implement me. )
    !> Calculate the Quadratic Limited coefficients.
    !> ( @todo Implement me. )
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_dg_calc_limiter
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_dg


