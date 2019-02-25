!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.
    
Module orchid_solver_grid_gauss_legendre
Use orchid_solver_params
Use orchid_solver_grid_gauss
Implicit None
Type :: MhdGridGaussLegendreFunc
    Real(8) :: Psi
    Real(8) :: gradPsi_x, gradPsi_y, gradPsi_z
    Contains
End Type MhdGridGaussLegendreFunc
Type :: MhdGridGaussLegendreNode
    Integer :: nfunc, nfunc_end
    Contains
    Procedure, Public, Non_Overridable :: calc => mhd_grid_gauss_legendre_calc
End Type MhdGridGaussLegendreNode
Type, Extends(MhdGridGauss) :: MhdGridGaussLegendre
    Integer :: nfuncs_min, nfuncs_max
    Type(MhdGridGaussLegendreFunc), Dimension(:), Allocatable :: lfuncs
    Type(MhdGridGaussLegendreNode), Dimension(:), Allocatable :: lcell_nodes, lface_nodes
    Contains
    Procedure, Public, Non_Overridable :: init_legendre_dummy => mhd_grid_gauss_legendre_init_dummy
End Type MhdGridGaussLegendre
Private :: mhd_grid_gauss_legendre_init_dummy
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Function mhd_grid_gauss_legendre_calc(This, coef) Result(value)
    !> {{{
    Class(MhdGridGaussLegendreNode), Intent(In) :: This
    Real(8), Dimension(m_min:m_max), Intent(In) :: coef
    Real(8) :: value
    !> }}}
End Function mhd_grid_gauss_legendre_calc
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
    This%nfuncs_min = 1
    This%nfuncs_max = 1
    Allocate(This%lfuncs(This%nfuncs_min:This%nfuncs_max))
    Allocate(This%lcell_nodes(This%gcnodes_min:This%gcnodes_max))
    Allocate(This%lface_nodes(This%gfnodes_min:This%gfnodes_max))
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Initialize functions.
    This%lfuncs(1)%Psi = 1.0D0
    This%lfuncs(1)%gradPsi_x = 0.0D0
    This%lfuncs(1)%gradPsi_y = 0.0D0
    This%lfuncs(1)%gradPsi_z = 0.0D0
    !> Initialize Cells and CellNodes.
    !$OMP Parallel Do
    Do n = This%ncells_min, This%ncells_max
        This%lcell_nodes(n)%nfunc = 1
        This%lcell_nodes(n)%nfunc_end = 1
    End Do
    !$OMP End Parallel Do
    !> Initialize Faces and FaceNodes.
    !$OMP Parallel Do
    Do n = This%nfaces_min, This%nfaces_max
        This%lface_nodes(n)%nfunc = 1
        This%lface_nodes(n)%nfunc_end = 1
    End Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_grid_gauss_legendre_init_dummy
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_grid_gauss_legendre
    

