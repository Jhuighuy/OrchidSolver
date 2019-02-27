!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_grid_gauss_legendre_poly
Implicit None
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Function mhd_legendre_poly(m, x) Result(y)
    !> Legendre polynomial value.
    !> {{{
    Integer, Intent(In) :: m
    Real(8), Intent(In) :: x
    Real(8) :: y
    !> }}}
    Select Case ( m )
        Case ( 0 )
            y = 1.0D0
        Case ( 1 )
            y = x
        Case ( 2 )
            y = 1.5D0*x**2 - 0.5D0
        Case ( 3 )
            y = x*( 2.5D0*x**2 - 1.5D0 )
        Case ( 4 )
            y = x**2
            y = 4.375D0*y**2 - 3.75D0*y + 0.375D0
        Case ( 5 )
            y = x**2
            y = x*( 7.875D0*y**2 - 8.75*y + 1.875D0 )
        Case Default
            Write (0,*) 'Too large Legendre polynomial number.'
            Error Stop -1
    End Select
End Function mhd_legendre_poly
!########################################################################################################
!########################################################################################################
!########################################################################################################
Function mhd_legendre_func(m, x) Result(y)
    !> Legendre function (:= orhonormalized polynomial) value.
    !> {{{
    Integer, Intent(In) :: m
    Real(8), Intent(In) :: x
    Real(8) :: y
    !> }}}
    y = mhd_legendre_poly(m, x)
    y = Sqrt(0.5D0*( 2.0D0*Dble(m) + 1.0D0 ))*y
End Function mhd_legendre_func
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_grid_gauss_legendre_poly


    
Module orchid_solver_grid_gauss_legendre
Use orchid_solver_params
Use orchid_solver_grid_gauss
Implicit None
Type :: MhdGridGaussLegendreFunc
    Real(8) :: Psi
    Real(8) :: grad
    Real(8) :: grad_nx, grad_ny, grad_nz
    Contains
End Type MhdGridGaussLegendreFunc
Type, Extends(MhdGridGauss) :: MhdGridGaussLegendre
    Type(MhdGridGaussLegendreFunc), Dimension(:, :), Allocatable :: cell_node_funcs
    Type(MhdGridGaussLegendreFunc), Dimension(:, :, :), Allocatable :: face_node_funcs
    Contains
    Procedure, Public, Non_Overridable :: init_legendre_dummy => mhd_grid_gauss_legendre_init_dummy
    Procedure, Public, Non_Overridable :: init_legendre1D => mhd_grid_gauss_legendre_init1D
End Type MhdGridGaussLegendre
Private :: mhd_grid_gauss_legendre_init_dummy
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_grid_gauss_legendre_init_dummy(This)
    !> Initialize a dummy Legendre-Gauss grid.
    !> {{{
    Class(MhdGridGaussLegendre), Intent(InOut) :: This
    !> }}}
    Integer :: n
    !>-------------------------------------------------------------------------------
    !> Allocate the Gauss Grid.
    Allocate(This%cell_node_funcs(m_min:m_max, This%ncell_nodes_min:This%ncell_nodes_max))
    Allocate(This%face_node_funcs(m_min:m_max, 1:2, This%nface_nodes_min:This%nface_nodes_max))
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Initialize functions.
    This%cell_node_funcs(:, :)%psi = 1.0D0
    This%cell_node_funcs(:, :)%grad = 0.0D0
    This%cell_node_funcs(:, :)%grad_nx = 0.0D0
    This%cell_node_funcs(:, :)%grad_ny = 0.0D0
    This%cell_node_funcs(:, :)%grad_nz = 0.0D0    
    This%face_node_funcs(:, :, :)%psi = 1.0D0
    This%face_node_funcs(:, :, :)%grad = 0.0D0
    This%face_node_funcs(:, :, :)%grad_nx = 0.0D0
    This%face_node_funcs(:, :, :)%grad_ny = 0.0D0
    This%face_node_funcs(:, :, :)%grad_nz = 0.0D0    
    !>-------------------------------------------------------------------------------
End Subroutine mhd_grid_gauss_legendre_init_dummy
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_grid_gauss_legendre_init1D(This)
    !> Initialize a dummy Legendre-Gauss grid.
    !> {{{
    Class(MhdGridGaussLegendre), Intent(InOut) :: This
    !> }}}
    Integer :: n
    Real(8) :: h
    If (n_funcs == 1) Then
       Call this%init_legendre_dummy()
       Return
    End If

    !>-------------------------------------------------------------------------------
    !> Allocate the Gauss Grid.
    Allocate(This%cell_node_funcs(m_min:m_max, This%ncell_nodes_min:This%ncell_nodes_max))
    Allocate(This%face_node_funcs(m_min:m_max, 1:2, This%nface_nodes_min:This%nface_nodes_max))
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Initialize functions.
    This%cell_node_funcs(0, :)%psi = 1.0D0
    This%cell_node_funcs(0, :)%grad = 0.0D0
    This%cell_node_funcs(0, :)%grad_nx = 0.0D0
    This%cell_node_funcs(0, :)%grad_ny = 0.0D0
    This%cell_node_funcs(0, :)%grad_nz = 0.0D0    
    This%face_node_funcs(0, :, :)%psi = 1.0D0
    This%face_node_funcs(0, :, :)%grad = 0.0D0
    This%face_node_funcs(0, :, :)%grad_nx = 0.0D0
    This%face_node_funcs(0, :, :)%grad_ny = 0.0D0
    This%face_node_funcs(0, :, :)%grad_nz = 0.0D0
    Do n = This%ncells_min, This%ncells_max
        h = This%cells(n)%Vcell
        This%cell_node_funcs(1, This%gcells(n)%nnode)%psi = &
             2.0D0/This%cells(n)%Vcell*( This%cell_nodes(This%gcells(n)%nnode)%x - This%cells(n)%x )
        This%cell_node_funcs(1, This%gcells(n)%nnode)%grad = 2.0D0/This%cells(n)%Vcell
        This%cell_node_funcs(1, This%gcells(n)%nnode)%grad_nx = 1.0D0
        This%cell_node_funcs(1, This%gcells(n)%nnode)%grad_ny = 0.0D0
        This%cell_node_funcs(1, This%gcells(n)%nnode)%grad_nz = 0.0D0
        This%cell_node_funcs(1, This%gcells(n)%nnode_end)%psi = &
             2.0D0/This%cells(n)%Vcell*( This%cell_nodes(This%gcells(n)%nnode_end)%x - This%cells(n)%x )
        This%cell_node_funcs(1, This%gcells(n)%nnode_end)%grad = 2.0D0/This%cells(n)%Vcell
        This%cell_node_funcs(1, This%gcells(n)%nnode_end)%grad_nx = 1.0D0
        This%cell_node_funcs(1, This%gcells(n)%nnode_end)%grad_ny = 0.0D0
        This%cell_node_funcs(1, This%gcells(n)%nnode_end)%grad_nz = 0.0D0        
    End Do
    Do n = This%nfaces_min, This%nfaces_max
        This%face_node_funcs(1, 1, This%gfaces(n)%nnode)%psi = -1.0D0
        This%face_node_funcs(1, 2, This%gfaces(n)%nnode)%psi = +1.0D0
        This%face_node_funcs(1, :, This%gfaces(n)%nnode)%grad = 2.0D0/h
        This%face_node_funcs(1, :, This%gfaces(n)%nnode)%grad_nx = 1.0D0
        This%face_node_funcs(1, :, This%gfaces(n)%nnode)%grad_ny = 0.0D0
        This%face_node_funcs(1, :, This%gfaces(n)%nnode)%grad_nz = 0.0D0
    End Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_grid_gauss_legendre_init1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_grid_gauss_legendre
    

