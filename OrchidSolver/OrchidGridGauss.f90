!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.

Module orchid_solver_grid_gauss_const
Implicit None
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Function mhd_grid_gauss_point(Npoints, k) Result(x)
    !> Points of the Gauss quadrature.
    !> {{{
    Integer, Intent(In) :: Npoints
    Integer, Intent(In) :: k
    Real(8) :: x
    !> }}}
    Real(8), Dimension(1:1), Parameter :: x1 = [ +0.000000000D0 ]
    Real(8), Dimension(1:2), Parameter :: x2 = [ -0.577350269D0, +0.577350269D0 ]
    Real(8), Dimension(1:3), Parameter :: x3 = [ -0.774596669D0, +0.000000000D0, +0.774596669D0 ]
    Real(8), Dimension(1:4), Parameter :: x4 = [ -0.861136312D0, -0.339981044D0, +0.339981044D0, &
                                                 +0.861136312D0 ]
    Real(8), Dimension(1:5), Parameter :: x5 = [ -0.906179846D0, -0.538469310D0, +0.000000000D0, &
                                                 +0.538469310D0, +0.906179846D0 ]
    Select Case ( Npoints )
        Case ( 1 )
            x = x1(k)
        Case ( 2 )
            x = x2(k)
        Case ( 3 )
            x = x3(k)
        Case ( 4 )
            x = x4(k)
        Case ( 5 )
            x = x5(k)
        Case Default
            Write (0,*) 'Too many points for Gauss quadratures.'
            Error Stop -1
    End Select
End Function mhd_grid_gauss_point
!########################################################################################################
!########################################################################################################
!########################################################################################################
Function mhd_grid_gauss_weight(Npoints, k) Result(w)
    !> Weights of the Gauss quadrature.
    !> {{{
    Integer, Intent(In) :: Npoints
    Integer, Intent(In) :: k
    Real(8) :: w
    !> }}}
    Real(8), Dimension(1:1), Parameter :: w1 = [ +1.000000000D0 ]
    Real(8), Dimension(1:2), Parameter :: w2 = [ +0.500000000D0, +0.50000000D0 ]
    Real(8), Dimension(1:3), Parameter :: w3 = [ +0.277777778D0, +0.444444444D0, +0.277777778D0 ]
    Real(8), Dimension(1:4), Parameter :: w4 = [ +0.173927423D0, +0.326072577D0, +0.326072577D0, &
                                                 +0.173927423D0 ]
    Real(8), Dimension(1:5), Parameter :: w5 = [ +0.118463443D0, +0.239314335D0, +0.284444444D0, &
                                                 +0.239314335D0, +0.118463443D0 ]
    Select Case ( Npoints )
        Case ( 1 )
            w = w1(k)
        Case ( 2 )
            w = w2(k)
        Case ( 3 )
            w = w3(k)
        Case ( 4 )
            w = w4(k)
        Case ( 5 )
            w = w5(k)
        Case Default
            Write (0,*) 'Too many points for Gauss quadratures.'
            Error Stop -1
    End Select
End Function mhd_grid_gauss_weight
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_grid_gauss_const


    
Module orchid_solver_grid_gauss
Use orchid_solver_params
Use orchid_solver_grid
Use orchid_solver_grid_gauss_const
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
    Integer :: ncell_nodes_min, ncell_nodes_max
    Integer :: nface_nodes_min, nface_nodes_max
    Type(MhdGridGaussCell), Dimension(:), Allocatable :: gcells
    Type(MhdGridGaussFace), Dimension(:), Allocatable :: gfaces
    Type(MhdGridGaussNode), Dimension(:), Allocatable :: cell_nodes, face_nodes
    Contains
    Procedure, Public, Non_Overridable :: init_gauss => mhd_grid_gauss_init
    Procedure, Public, Non_Overridable :: init_gauss1D => mhd_grid_gauss_init1D
    Procedure, Public, Non_Overridable :: init_gauss_dummy => mhd_grid_gauss_init_dummy
End Type MhdGridGauss
Private :: mhd_grid_gauss_init1D, &
           mhd_grid_gauss_init_dummy
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_grid_gauss_init(This, Npoints)
    !> Initialize a Gauss grid.
    !> {{{
    Class(MhdGridGauss), Intent(InOut) :: This
    Integer, Intent(In), Optional :: Npoints
    !> }}}
    If ( Present(Npoints) ) Then
        Select Case ( This%ndims )
            Case ( 1 )
                Call This%init_gauss1D(Npoints)
            !Case ( 2 )
            !    Call This%init_gauss2D(Npoints)
            !Case ( 3 )
            !    Call This%init_gauss3D(Npoints)
            Case Default
                Write (0,*) 'Invalid amount of the grid dimensions. ', &
                            'Was grid initialized?'
                Error Stop -1
        End Select
    Else
        Call This%init_gauss_dummy()
    End If
End Subroutine mhd_grid_gauss_init
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_grid_gauss_init1D(This, Npoints)
    !> Initialize a 1D Gauss grid.
    !> {{{
    Class(MhdGridGauss), Intent(InOut) :: This
    Integer, Intent(In) :: Npoints
    !> }}}
    Integer :: n, k, kk
    Real(8) :: h, x
    !>-------------------------------------------------------------------------------
    !> Allocate the Gauss Grid.
    This%ncell_nodes_min = This%ncells_min
    This%ncell_nodes_max = This%ncells_max*Npoints
    This%nface_nodes_min = This%nfaces_min
    This%nface_nodes_max = This%nfaces_max
    Allocate(This%gcells(This%ncells_min:This%ncells_max))
    Allocate(This%gfaces(This%nfaces_min:This%nfaces_max))
    Allocate(This%cell_nodes(This%ncell_nodes_min:This%ncell_nodes_max))
    Allocate(This%face_nodes(This%nface_nodes_min:This%nface_nodes_max))
    !>-------------------------------------------------------------------------------

    !>-------------------------------------------------------------------------------
    !> Initialize Cells and CellNodes.
    !$OMP Parallel Do Private(k, kk, h, x)
    Do n = This%ncells_min, This%ncells_max
        h = This%cells(n)%Vcell
        x = This%nodes(This%cell2node(This%cells(n)%nnode))%x
        This%gcells(n)%nnode = Npoints*( n - 1 ) + 1
        This%gcells(n)%nnode_end = Npoints*n
        Do k = This%gcells(n)%nnode, This%gcells(n)%nnode_end
            kk = k - This%gcells(n)%nnode + 1
            This%cell_nodes(k)%w = mhd_grid_gauss_weight(Npoints, kk)
            This%cell_nodes(k)%x = mhd_grid_gauss_point(Npoints, kk)
            This%cell_nodes(k)%x = x + 0.5D0*h*( This%cell_nodes(k)%x + 1.0D0 )
            This%cell_nodes(k)%y = 0.0D0
            This%cell_nodes(k)%z = 0.0D0
        End Do
    End Do
    !$OMP End Parallel Do
    !> Initialize Faces and FaceNodes.
    !$OMP Parallel Do
    Do n = This%nfaces_min, This%nfaces_max
        This%gfaces(n)%nnode = n
        This%gfaces(n)%nnode_end = n
        This%face_nodes(n)%x = This%faces(n)%x
        This%face_nodes(n)%y = This%faces(n)%y
        This%face_nodes(n)%z = This%faces(n)%z
        This%face_nodes(n)%w = 1.0D0
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_grid_gauss_init1D
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
    This%ncell_nodes_min = This%ncells_min
    This%ncell_nodes_max = This%ncells_max
    This%nface_nodes_min = This%nfaces_min
    This%nface_nodes_max = This%nfaces_max
    Allocate(This%gcells(This%ncells_min:This%ncells_max))
    Allocate(This%gfaces(This%nfaces_min:This%nfaces_max))
    Allocate(This%cell_nodes(This%ncell_nodes_min:This%ncell_nodes_max))
    Allocate(This%face_nodes(This%nface_nodes_min:This%nface_nodes_max))
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Initialize Cells and CellNodes.
    !$OMP Parallel Do
    Do n = This%ncells_min, This%ncells_max
        This%gcells(n)%nnode = n
        This%gcells(n)%nnode_end = n
        This%cell_nodes(n)%x = This%cells(n)%x
        This%cell_nodes(n)%y = This%cells(n)%y
        This%cell_nodes(n)%z = This%cells(n)%z
        This%cell_nodes(n)%w = 1.0D0
    End Do
    !$OMP End Parallel Do
    !> Initialize Faces and FaceNodes.
    !$OMP Parallel Do
    Do n = This%nfaces_min, This%nfaces_max
        This%gfaces(n)%nnode = n
        This%gfaces(n)%nnode_end = n
        This%face_nodes(n)%x = This%faces(n)%x
        This%face_nodes(n)%y = This%faces(n)%y
        This%face_nodes(n)%z = This%faces(n)%z
        This%face_nodes(n)%w = 1.0D0
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_grid_gauss_init_dummy
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_grid_gauss



