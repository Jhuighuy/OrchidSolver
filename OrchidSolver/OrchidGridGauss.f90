!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.
    
Module orchid_solver_grid_gauss
Use orchid_solver_params
Use orchid_solver_grid
Implicit None
Type :: MhdGridGaussNode
    Real(8) :: x, y, z, w
    Contains
End Type MhdGridGaussNode
Type :: MhdGridGaussCell
    Integer :: nnode, nnode_end
    Contains
End Type MhdGridGaussCell
Type :: MhdGridGaussFace
    Integer :: nnode, nnode_end
    Contains
End Type MhdGridGaussFace
Type, Extends(MhdGrid) :: MhdGridGauss
    Integer :: gcnodes_min, gcnodes_max
    Integer :: gfnodes_min, gfnodes_max
    Type(MhdGridGaussCell), Dimension(:), Allocatable :: gcells
    Type(MhdGridGaussFace), Dimension(:), Allocatable :: gfaces
    Type(MhdGridGaussNode), Dimension(:), Allocatable :: gcell_nodes, gface_nodes
    Contains
    Procedure, Public, Non_Overridable :: init_gauss_dummy => mhd_grid_gauss_init_dummy
End Type MhdGridGauss
Private :: mhd_grid_gauss_init_dummy
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_grid_gauss_init_dummy(This)
    !> Initialize a dummy Gauss grid (for first-order integration).
    !> {{{
    Class(MhdGridGauss), Intent(InOut) :: This
    !> }}}
    Integer :: n
    !>-------------------------------------------------------------------------------
    !> Allocate the Gauss Grid.
    This%gcnodes_min = This%ncells_min
    This%gcnodes_max = This%ncells_max
    This%gfnodes_min = This%nfaces_min
    This%gfnodes_max = This%nfaces_max
    Allocate(This%gcells(This%ncells_min:This%ncells_max))
    Allocate(This%gfaces(This%nfaces_min:This%nfaces_max))
    Allocate(This%gcell_nodes(This%gcnodes_min:This%gcnodes_max))
    Allocate(This%gface_nodes(This%gfnodes_min:This%gfnodes_max))
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Initialize Cells and CellNodes.
    !$OMP Parallel Do
    Do n = This%ncells_min, This%ncells_max
        This%gcells(n)%nnode = n
        This%gcells(n)%nnode_end = n
        This%gcell_nodes(n)%x = This%cells(n)%x
        This%gcell_nodes(n)%y = This%cells(n)%y
        This%gcell_nodes(n)%z = This%cells(n)%z
        This%gcell_nodes(n)%w = 1.0D0
    End Do
    !$OMP End Parallel Do
    !> Initialize Faces and FaceNodes.
    !$OMP Parallel Do
    Do n = This%nfaces_min, This%nfaces_max
        This%gfaces(n)%nnode = n
        This%gfaces(n)%nnode_end = n
        This%gface_nodes(n)%x = This%faces(n)%x
        This%gface_nodes(n)%y = This%faces(n)%y
        This%gface_nodes(n)%z = This%faces(n)%z
        This%gface_nodes(n)%w = 1.0D0
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_grid_gauss_init_dummy
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_grid_gauss


