!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.
    
!> MUSCL Hydrodynamics Solver (Second Order).
Module orchid_solver_hydro_fv_muscl
Use orchid_solver_params
Use orchid_solver_grid_gauss
Use orchid_solver_hydro_fv
Implicit None
Type, Extends(MhdHydroSolver) :: MhdHydroSolverMUSCL
    Contains
    Procedure, Public :: calc_flux_muscl => mhd_hydro_muscl_calc_flux
    Procedure, Public :: calc_step_muscl => mhd_hydro_muscl_calc_step
End Type
Private :: mhd_hydro_muscl_calc_flux, &
           mhd_hydro_muscl_calc_step
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_muscl_calc_flux(This, &
                                     ga, g, grad_g, flux)
    !> Calculate the Convective Fluxes.
    !> {{{
    Class(MhdHydroSolverMUSCL), Intent(InOut) :: This
    Class(MhdGridGauss), Intent(In) :: ga
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: g
    Real(8), Dimension(1:3, n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: grad_g
    Real(8), Dimension(n_min:n_max, ga%nfaces_min:ga%nfaces_max), Intent(Out) :: flux
    !> }}}
    Integer :: ip, im, j, k
    Real(8) :: nx, ny, nz
    Real(8), Dimension(n_min:n_max) :: w
    Real(8), Dimension(n_min:n_max) :: qp, qm, flux_q
    w(:) = 0.0D0
    w(n_min:n_min+1) = 1.0D0
    !>-------------------------------------------------------------------------------
    !> Calculate the Convective Fluxes.
    !$OMP Parallel Do &
    !$OMP& Private(ip, im, j, k, nx, ny, nz, qp, qm, flux_q)
    Do j = ga%nfaces_min, ga%nfaces_max
        ip = ga%faces(j)%ncell_p
        im = ga%faces(j)%ncell_m
        nx = ga%faces(j)%nx
        ny = ga%faces(j)%ny
        nz = ga%faces(j)%nz
        flux(:, j) = 0.0D0
        If ( ip > 0 .AND. im > 0 ) Then
            !> Domain Interior.
            Do k = ga%gfaces(j)%nnode, ga%gfaces(j)%nnode_end
                qp(:) = g(:, ip) + ( ga%face_nodes(k)%x - ga%cells(ip)%x )*grad_g(1, :, ip) + &
                                   ( ga%face_nodes(k)%y - ga%cells(ip)%y )*grad_g(2, :, ip) + &
                                   ( ga%face_nodes(k)%z - ga%cells(ip)%z )*grad_g(3, :, ip)
                qm(:) = g(:, im) + ( ga%face_nodes(k)%x - ga%cells(im)%x )*grad_g(1, :, im) + &
                                   ( ga%face_nodes(k)%y - ga%cells(im)%y )*grad_g(2, :, im) + &
                                   ( ga%face_nodes(k)%z - ga%cells(im)%z )*grad_g(3, :, im)
                Call This%m_flux%calc(qp, qm, flux_q, nx, ny, nz)
                flux(:, j) = flux(:, j) + ga%face_nodes(k)%w*flux_q(:)
            End Do
        Else If ( ip < 0 ) Then
            !> Domain Boundary.
            Do k = ga%gfaces(j)%nnode, ga%gfaces(j)%nnode_end
                qm(:) = g(:, im) + ( ga%face_nodes(k)%x - ga%cells(im)%x )*grad_g(1, :, im) + &
                                   ( ga%face_nodes(k)%y - ga%cells(im)%y )*grad_g(2, :, im) + &
                                   ( ga%face_nodes(k)%z - ga%cells(im)%z )*grad_g(3, :, im)
                If ( ip == -1 ) Then
                    !> Free flow boundary conditions.
                    Call This%m_flux%calc(qm, qm, flux_q, nx, ny, nz)
                Else
                    !> Wall boundary conditions.
                    Call This%m_flux%calc(qm*w, qm, flux_q, nx, ny, nz)
                End If
                flux(:, j) = flux(:, j) + ga%face_nodes(k)%w*flux_q(:)
            End Do
        Else If ( im < 0 ) Then
            !> Domain Boundary.
            Do k = ga%gfaces(j)%nnode, ga%gfaces(j)%nnode_end
                qp(:) = g(:, ip) + ( ga%face_nodes(k)%x - ga%cells(ip)%x )*grad_g(1, :, ip) + &
                                   ( ga%face_nodes(k)%y - ga%cells(ip)%y )*grad_g(2, :, ip) + &
                                   ( ga%face_nodes(k)%z - ga%cells(ip)%z )*grad_g(3, :, ip)
                If ( im == -1 ) Then
                    !> Free flow boundary conditions.
                    Call This%m_flux%calc(qp, qp, flux_q, nx, ny, nz)
                Else
                    !> Wall boundary conditions.
                    Call This%m_flux%calc(qp, qp*w, flux_q, nx, ny, nz)
                End If
                flux(:, j) = flux(:, j) + ga%face_nodes(k)%w*flux_q(:)
            End Do
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_muscl_calc_flux
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_hydro_muscl_calc_step(This, Tau, &
                                     ga, g, gp)
    !> Calculate the Time Step.
    !> {{{
    Class(MhdHydroSolverMUSCL), Intent(InOut) :: This
    Class(MhdGridGauss), Intent(In) :: ga
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(In) :: g
    Real(8), Dimension(n_min:n_max, ga%ncells_min:ga%ncells_max), Intent(Out) :: gp
    Real(8), Intent(In) :: Tau
    !> ---
    Real(8), Dimension(:, :, :), Allocatable, Save :: grad_g_face, grad_g_cell
    Real(8), Dimension(:, :), Allocatable, Save :: flux
    !> }}}
    Integer :: i, j, jj, n
    Real(8), Dimension(n_min:n_max) :: dg, df, df_conv, df_visc
    If ( .NOT. Allocated(flux) ) Then
        Allocate(grad_g_face(1:3, n_min:n_max, ga%nfaces_min:ga%nfaces_max))
        Allocate(grad_g_cell(1:3, n_min:n_max, ga%ncells_min:ga%ncells_max))
        Allocate(flux(n_min:n_max, ga%nfaces_min:ga%nfaces_max))
        grad_g_face(:, :, :) = 0.0D0
        grad_g_cell(:, :, :) = 0.0D0
        flux(:, :) = 0.0D0
    End If
    !>-------------------------------------------------------------------------------
    !> Calculate the Gradients and Convective Fluxes.
    Call This%calc_grad(ga, g, grad_g_face)
    Do i = ga%ncells_min, ga%ncells_max
        Do n = n_min, n_max
            grad_g_cell(:, n, i) = grad_g_face(:, n, ga%cell2face(ga%cells(i)%nface))
            Do jj = ga%cells(i)%nface + 1, ga%cells(i)%nface_end
                j = ga%cell2face(jj)
                grad_g_cell(:, n, i) = minmod2(grad_g_cell(:, n, i), grad_g_face(:, n, j))
            End Do
        End Do
    End Do
    Call This%calc_flux_muscl(ga, g, grad_g_cell, flux)
    !> Calculate the Updated Field values.
    Do i = ga%ncells_min, ga%ncells_max
        !> Update the values.
        dg(:) = 0.0D0
        Do jj = ga%cells(i)%nface, ga%cells(i)%nface_end
            j = ga%cell2face(jj)
            df_visc(:) = 0.0D0
            df_conv(:) = flux(:, j)
            !If ( Allocated(This%m_visc_flux) ) Then
            !    Call This%m_visc_flux%calc(g(:, i), fg(1, :, j)*ga%faces(j)%nx + &
            !                                        fg(2, :, j)*ga%faces(j)%ny + &
            !                                        fg(3, :, j)*ga%faces(j)%nz, df_visc(:))
            !End If
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
    !>-------------------------------------------------------------------------------
End Subroutine mhd_hydro_muscl_calc_step
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_hydro_fv_muscl
    
    
