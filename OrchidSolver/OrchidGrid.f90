!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.
    
Module orchid_solver_grid
Use orchid_solver_params
Use orchid_solver_helpers
Implicit None
Type :: MhdGridCell
    Integer :: nface, nface_end
    Real(8) :: x, y, z
    Real(8) :: Vcell
    Contains
End Type MhdGridCell
Type :: MhdGridFace
    Integer :: ncell_p, ncell_m
    Real(8) :: nx, ny, nz
    Real(8) :: Sface
End Type MhdGridFace
Type :: MhdGrid
    Integer :: ndims
    Integer :: ncells_min, ncells_max
    Integer :: nfaces_min, nfaces_max
    Integer, Dimension(:), Allocatable :: cell2face
    Type(MhdGridCell), Dimension(:), Allocatable :: cells
    Type(MhdGridFace), Dimension(:), Allocatable :: faces
    Contains
    Procedure, Public, Non_Overridable :: init1D => mhd_grid_init1D
    Procedure, Public, Non_Overridable :: init2D => mhd_grid_init2D
    Procedure, Public, Non_Overridable :: init2D_polar => mhd_grid_init2D_polar
End Type MhdGrid
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_grid_init1D(This, L, N, Bp, Bm)
    !> Initialize a simple 1D cartesian grid.
    !> {{{
    Class(MhdGrid), Intent(InOut) :: This
    Real(8), Intent(In) :: L
    Integer, Intent(In) :: N
    Integer, Intent(In), Optional :: Bp, Bm
    !> }}}
    Real(8) :: h
    Integer :: i
    !>-------------------------------------------------------------------------------
    !> Allocate the Grid.
    This%ndims = 1
    This%ncells_min = 1
    This%ncells_max = N
    This%nfaces_min = 0
    This%nfaces_max = N
    Allocate(This%cells(This%ncells_min:This%ncells_max))
    Allocate(This%cell2face(This%ncells_min:2*This%ncells_max))
    Allocate(This%faces(This%nfaces_min:This%nfaces_max))
    !>-------------------------------------------------------------------------------

    !>-------------------------------------------------------------------------------
    !> Initialize Cells and Cell2Face.
    h = L/Dble(N)
    !$OMP Parallel Do
    Do i = This%ncells_min, This%ncells_max
        This%cells(i)%nface = 2*i - 1
        This%cells(i)%nface_end = 2*i
        This%cell2face(This%cells(i)%nface) = i - 1
        This%cell2face(This%cells(i)%nface_end) = i
        This%cells(i)%x = h*( Dble(i) - 0.5D0 )
        This%cells(i)%y = 0.0D0
        This%cells(i)%z = 0.0D0
        This%cells(i)%Vcell = h
    End Do
    !$OMP End Parallel Do
    !> Initialize Faces.
    !$OMP Parallel Do
    Do i = This%nfaces_min, This%nfaces_max
        If ( i == N ) Then
            !> Domain Boundary.
            If ( Present(Bp) ) Then
                If ( Bp == -3 ) Then
                    !> Periodic boundary conditions.
                    This%faces(i)%ncell_p = 1
                Else
                    !> Free flow or wall boundary conditions.
                    This%faces(i)%ncell_p = Bp
                End If
            Else
                !> Force Free-Flow boundary conditions.
                This%faces(i)%ncell_p = -1
            End If
        Else
            !> Domain Interior.
            This%faces(i)%ncell_p = i + 1
        End If
        If ( i == 0 ) Then
            !> Domain Boundary.
            If ( Present(Bm) ) Then
                If ( Bm == -3 ) Then
                    !> Periodic boundary conditions.
                    This%faces(i)%ncell_m = N
                Else
                    !> Free flow or wall boundary conditions.
                    This%faces(i)%ncell_m = Bm
                End If
            Else
                !> Force Free-Flow boundary conditions.
                This%faces(i)%ncell_m = -1
            End If
        Else
            !> Domain Interior.
            This%faces(i)%ncell_m = i
        End If
        This%faces(i)%nx = 1.0D0
        This%faces(i)%ny = 0.0D0
        This%faces(i)%nz = 0.0D0
        This%faces(i)%Sface = 1.0D0
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_grid_init1D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_grid_init2D(This, Lx, Ly, Nx, Ny, Bpx, Bmx, Bpy, Bmy)
    !> Initialize a simple 2D cartesian grid.
    !> {{{
    Class(MhdGrid), Intent(InOut) :: This
    Real(8), Intent(In) :: Lx, Ly
    Integer, Intent(In) :: Nx, Ny
    Integer, Intent(In), Optional :: Bpx, Bmx, Bpy, Bmy
    !> }}}
    Real(8) :: hx, hy
    Integer :: i, j, n, m
    !>-------------------------------------------------------------------------------
    !> Allocate the Grid.
    This%ndims = 2
    This%ncells_min = 1
    This%ncells_max = Nx*Ny
    This%nfaces_min = 0
    This%nfaces_max = Nx*(Ny + 1) + Ny*(Nx + 1) - 1
    Allocate(This%cells(This%ncells_min:This%ncells_max))
    Allocate(This%cell2face(This%ncells_min:4*This%ncells_max))
    Allocate(This%faces(This%nfaces_min:This%nfaces_max))
    !>-------------------------------------------------------------------------------

    !>-------------------------------------------------------------------------------
    !> Initialize Cells and Cell2Face.
    hx = Lx/Dble(Nx)
    hy = Ly/Dble(Ny)
    !$OMP Parallel Do Private(i, j)
    Do n = This%ncells_min, This%ncells_max
        i = Mod(n - 1, Nx) + 1
        j = ( n - i )/Nx + 1
        This%cells(n)%nface = 4*n - 3
        This%cells(n)%nface_end = 4*n
        This%cell2face(This%cells(n)%nface + 0) = ( i - 1 ) + ( j - 1 )*( Nx + 1 )
        This%cell2face(This%cells(n)%nface + 1) = ( i - 0 ) + ( j - 1 )*( Nx + 1 )
        This%cell2face(This%cells(n)%nface + 2) = i + ( j - 1 )*Nx + Ny*( Nx + 1 ) - 1
        This%cell2face(This%cells(n)%nface_end) = i + ( j - 0 )*Nx + Ny*( Nx + 1 ) - 1        
        This%cells(n)%x = hx*( Dble(i) - 0.5D0 )
        This%cells(n)%y = hy*( Dble(j) - 0.5D0 )
        This%cells(n)%z = 0.0D0
        This%cells(n)%Vcell = hx*hy
    End Do
    !$OMP End Parallel Do
    !> Initialize Faces.
    !$OMP Parallel Do Private(i, j, m)
    Do n = This%nfaces_min, This%nfaces_max
        If ( n < Ny*( Nx + 1 ) ) Then
            !> Initialize the Horizontal Faces.
            m = n
            i = Mod(m, Nx + 1)
            j = ( m - i )/( Nx + 1 ) + 1
            If ( i == Nx ) Then
                !> Domain Boundary.
                If ( Present(Bpx) ) Then
                   If ( Bpx == -3 ) Then
                       !> Periodic boundary conditions.
                       This%faces(n)%ncell_p = 1 + ( j - 1 )*Nx
                   Else
                       !> Free flow or wall boundary conditions.
                       This%faces(n)%ncell_p = Bpx
                   End If
                Else
                    !> Force Free-Flow boundary conditions.
                    This%faces(n)%ncell_p = -1
                End If
            Else
                !> Domain Interior.
                This%faces(n)%ncell_p = i + 1 + ( j - 1 )*Nx
            End If
            If ( i == 0 ) Then
                !> Domain Boundary.
                If ( Present(Bmx) ) Then
                   If ( Bmx == -3 ) Then
                       !> Periodic boundary conditions.
                       This%faces(n)%ncell_m = Nx + ( j - 1 )*Nx
                   Else
                       !> Free flow or wall boundary conditions.
                       This%faces(n)%ncell_m = Bmx
                   End If
                Else
                    !> Force Free-Flow boundary conditions.
                    This%faces(n)%ncell_m = -1
                End If
            Else
                !> Domain Interior.
                This%faces(n)%ncell_m = i + 0 + ( j - 1 )*Nx
            End If
            This%faces(n)%nx = 1.0D0
            This%faces(n)%ny = 0.0D0
            This%faces(n)%nz = 0.0D0
            This%faces(n)%Sface = hx
        Else
            !> Initialize the Vertical Faces.
            m = n - Ny*( Nx + 1 ) + 1
            i = Mod(m - 1, Nx) + 1
            j = ( m - i )/Nx
            If ( j == Ny ) Then
                !> Domain Boundary.
                If ( Present(Bpy) ) Then
                    If ( Bpy == -3 ) Then
                        !> Periodic boundary conditions.
                        This%faces(n)%ncell_p = i + ( 1 - 1 )*Nx
                    Else
                        !> Free flow or wall boundary conditions.
                        This%faces(n)%ncell_p = Bpy
                    End If
                Else
                    !> Force Free-Flow boundary conditions.
                    This%faces(n)%ncell_p = -1
                End If
            Else
                !> Domain Interior.
                This%faces(n)%ncell_p = i + ( j - 0 )*Nx
            End If
            If ( j == 0 ) Then
                !> Domain Boundary.
                If ( Present(Bmy) ) Then
                    If ( Bmy == -3 ) Then
                        !> Periodic boundary conditions.
                        This%faces(n)%ncell_m = i + ( Ny - 1 )*Nx
                    Else
                        !> Free flow or wall boundary conditions.
                        This%faces(n)%ncell_m = Bmy
                    End If
                Else
                    !> Force Free-Flow boundary conditions.
                    This%faces(n)%ncell_m = -1
                End If
            Else
                !> Domain Interior.
                This%faces(n)%ncell_m = i + ( j - 1 )*Nx
            End If            
            This%faces(n)%nx = 0.0D0
            This%faces(n)%ny = 1.0D0
            This%faces(n)%nz = 0.0D0
            This%faces(n)%Sface = hy 
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_grid_init2D
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_grid_init2D_polar(This, Rinner, Router, Nr, Nphi, Bpr, Bmr)
    !> Initialize a simple 2D polar grid.
    !> {{{
    Class(MhdGrid), Intent(InOut) :: This
    Real(8), Intent(In) :: Rinner, Router
    Integer, Intent(In) :: Nr, Nphi
    Integer, Intent(In), Optional :: Bpr, Bmr
    !> }}}
    Real(8) :: hr, hphi
    Real(8) :: r, phi
    Integer :: i, j, n, m
    !>-------------------------------------------------------------------------------
    !> Allocate the Grid.
    This%ndims = 2
    This%ncells_min = 1
    This%ncells_max = Nr*Nphi
    This%nfaces_min = 0
    This%nfaces_max = Nr*(Nphi + 1) + Nphi*(Nr + 1) - 1
    Allocate(This%cells(This%ncells_min:This%ncells_max))
    Allocate(This%cell2face(This%ncells_min:4*This%ncells_max))
    Allocate(This%faces(This%nfaces_min:This%nfaces_max))
    !>-------------------------------------------------------------------------------

    !>-------------------------------------------------------------------------------
    !> Initialize Cells and Cell2Face.
    hr = ( Router - Rinner )/Dble(Nr)
    hphi = 2.0D0*Pi/Dble(Nphi)
    !$OMP Parallel Do Private(i, j, r, phi)
    Do n = This%ncells_min, This%ncells_max
        i = Mod(n - 1, Nr) + 1
        j = ( n - i )/Nr + 1
        This%cells(n)%nface = 4*n - 3
        This%cells(n)%nface_end = 4*n
        This%cell2face(This%cells(n)%nface + 0) = ( i - 1 ) + ( j - 1 )*( Nr + 1 )
        This%cell2face(This%cells(n)%nface + 1) = ( i - 0 ) + ( j - 1 )*( Nr + 1 )
        This%cell2face(This%cells(n)%nface + 2) = i + ( j - 1 )*Nr + Nphi*( Nr + 1 ) - 1
        This%cell2face(This%cells(n)%nface_end) = i + ( j - 0 )*Nr + Nphi*( Nr + 1 ) - 1
        r = Rinner + hr*( Dble(i) - 0.5D0 )
        phi = hphi*( Dble(j) - 0.5D0 )
        This%cells(n)%x = r*Cos(phi)
        This%cells(n)%y = r*Sin(phi)
        This%cells(n)%z = 0.0D0
        This%cells(n)%Vcell = r*hr*Sin(hphi)
    End Do
    !$OMP End Parallel Do
    !> Initialize Faces.
    !$OMP Parallel Do Private(i, j, m)
    Do n = This%nfaces_min, This%nfaces_max
        If ( n < Nphi*( Nr + 1 ) ) Then
            !> Initialize the Radial Faces.
            m = n
            i = Mod(m, Nr + 1)
            j = ( m - i )/( Nr + 1 ) + 1
            If ( i == Nr ) Then
                !> Domain Boundary.
                If ( Present(Bpr) ) Then
                    !> Free flow or wall boundary conditions.
                    This%faces(n)%ncell_p = Bpr
                Else
                    !> Force Free-Flow boundary conditions.
                    This%faces(n)%ncell_p = -1
                End If
            Else
                !> Domain Interior.
                This%faces(n)%ncell_p = i + 1 + ( j - 1 )*Nr
            End If
            If ( i == 0 ) Then
                !> Domain Boundary.
                If ( Present(Bmr) ) Then
                    !> Free flow or wall boundary conditions.
                    This%faces(n)%ncell_m = Bmr
                Else
                    !> Force Free-Flow boundary conditions.
                    This%faces(n)%ncell_m = -1
                End If
            Else
                !> Domain Interior.
                This%faces(n)%ncell_m = i + 0 + ( j - 1 )*Nr
            End If
            r = Rinner + hr*Dble(i)
            phi = hphi*( Dble(j) - 0.5D0 )
            This%faces(n)%nx = +Cos(phi)
            This%faces(n)%ny = +Sin(phi)
            This%faces(n)%nz = 0.0D0
            This%faces(n)%Sface = 2.0D0*r*Sin(0.5D0*hphi)
        Else
            !> Initialize the Angular Faces.
            m = n - Nphi*( Nr + 1 ) + 1
            i = Mod(m - 1, Nr) + 1
            j = ( m - i )/Nr
            If ( j == Nphi ) Then
                !> Periodic boundary conditions.
                This%faces(n)%ncell_p = i + ( 1 - 1 )*Nr
            Else
                !> Domain Interior.
                This%faces(n)%ncell_p = i + ( j - 0 )*Nr
            End If
            If ( j == 0 ) Then
                !> Periodic boundary conditions.
                This%faces(n)%ncell_m = i + ( Nphi - 1 )*Nr
            Else
                !> Domain Interior.
                This%faces(n)%ncell_m = i + ( j - 1 )*Nr
            End If            
            phi = hphi*Dble(j)
            This%faces(n)%nx = -Sin(phi)
            This%faces(n)%ny = +Cos(phi)
            This%faces(n)%nz = 0.0D0
            This%faces(n)%Sface = hr
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_grid_init2D_polar
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_grid


