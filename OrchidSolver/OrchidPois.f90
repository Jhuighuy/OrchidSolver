!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.
    
Module orchid_solver_pois_flux
Use orchid_solver_params
Implicit None
Type :: MhdPoisFluxTPFA
    Contains
    Procedure, Public, Non_Overridable :: calc => mhd_pois_flux_calc
End Type MhdPoisFluxTPFA
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Pure &
Subroutine mhd_pois_flux_calc(This, &
                              nx, ny, nz, &
                              u_p, x_p, y_p, z_p, &
                              u_m, x_m, y_m, z_m, &
                              flux_u)
    !> Calculate the TPFA Fluxes.
    !> {{{
    Class(MhdPoisFluxTPFA), Intent(In) :: This
    Real(8), Intent(In) :: nx, ny, nz
    Real(8), Intent(In) :: u_p, x_p, y_p, z_p
    Real(8), Intent(In) :: u_m, x_m, y_m, z_m
    Real(8), Intent(Out) :: flux_u
    !> }}}
    Real(8) :: d
    d = Sqrt(( x_m - x_p )**2 + &
             ( y_m - y_p )**2 + &
             ( z_m - z_p )**2)
    flux_u = ( u_m - u_p )/d
End Subroutine mhd_pois_flux_calc
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_pois_flux
    
    
    
Module orchid_solver_pois
Use orchid_solver_params
Use orchid_solver_grid
Use orchid_solver_pois_flux
Implicit None
Type :: MhdPoisSolver
    Class(MhdPoisFluxTPFA), Allocatable :: m_flux
    Contains
    Procedure, Public :: init => mhd_pois_init
    Procedure, Public :: calc => mhd_pois_calc
    Procedure, Public :: calc_flux => mhd_pois_calc_flux
    Procedure, Public :: calc_iter => mhd_pois_calc_iter
    Procedure, Public :: calc_dotp => mhd_pois_calc_dotp
End Type MhdPoisSolver
Private :: mhd_pois_init, &
           mhd_pois_calc, &
           mhd_pois_calc_flux, &
           mhd_pois_calc_iter
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_pois_init(This)
    !> Initialize the Poisson solver.
    !> {{{
    Class(MhdPoisSolver), Intent(InOut) :: This
    !> }}}
    Allocate(MhdPoisFluxTPFA :: This%m_flux)
End Subroutine mhd_pois_init
!########################################################################################################
!########################################################################################################
!########################################################################################################    
Subroutine mhd_pois_calc_flux(This, &
                              ga, u, fl)
    !> Calculate the basic first order Fluxes.
    !> {{{
    Class(MhdPoisSolver), Intent(InOut) :: This
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(ga%ncells_min:ga%ncells_max), Intent(In) :: u
    Real(8), Dimension(ga%nfaces_min:ga%nfaces_max), Intent(Out) :: fl
    !> }}}
    Integer :: ip, im, j
    Real(8) :: x_p, y_p, z_p, x_m, y_m, z_m
    Real(8) :: nx, ny, nz
    !>-------------------------------------------------------------------------------
    !> Calculate the Fluxes.
    !$OMP Parallel Do Private(ip, im, x_p, y_p, z_p, x_m, y_m, z_m, nx, ny, nz)
    Do j = ga%nfaces_min, ga%nfaces_max
        ip = ga%faces(j)%ncell_p
        im = ga%faces(j)%ncell_m
        nx = ga%faces(j)%nx
        ny = ga%faces(j)%ny
        nz = ga%faces(j)%nz
        If ( ip > 0 ) Then
            !> Domain Interior.
            x_p = ga%cells(ip)%x
            y_p = ga%cells(ip)%y
            z_p = ga%cells(ip)%z
        End If
        If ( im > 0 ) Then
            !> Domain Interior.
            x_m = ga%cells(im)%x
            y_m = ga%cells(im)%y
            z_m = ga%cells(im)%z
        End If
        If ( ip >= 0 .AND. im >= 0 ) Then
            !> Domain Interior.
            Call This%m_flux%calc(nx, ny, nz, &
                                  u(ip), x_p, y_p, z_p, &
                                  u(im), x_m, y_m, z_m, &
                                  fl(j))
        Else If ( ip < 0 ) Then
            !> Domain Boundary.
            x_p = ga%faces(j)%x
            y_p = ga%faces(j)%y
            z_p = ga%faces(j)%z
            If ( ip == -1 ) Then
                !> Neumann boundary conditions.
                Call This%m_flux%calc(nx, ny, nz, &
                                      u(im), x_p, y_p, z_p, &
                                      u(im), x_m, y_m, z_m, &
                                      fl(j))
            Else
                !> Dirichlet boundary conditions.
                Call This%m_flux%calc(nx, ny, nz, &
                                      0.0D0, x_p, y_p, z_p, &
                                      u(im), x_m, y_m, z_m, &
                                      fl(j))
            End If
        Else If ( im < 0 ) Then
            !> Domain Boundary.
            x_m = ga%faces(j)%x
            y_m = ga%faces(j)%y
            z_m = ga%faces(j)%z
            If ( im == -1 ) Then
                !> Neumann boundary conditions.
                Call This%m_flux%calc(nx, ny, nz, &
                                      u(ip), x_p, y_p, z_p, &
                                      u(ip), x_m, y_m, z_m, &
                                      fl(j))
            Else
                !> Dirichlet boundary conditions.
                Call This%m_flux%calc(nx, ny, nz, &
                                      u(ip), x_p, y_p, z_p, &
                                      0.0D0, x_m, y_m, z_m, &
                                      fl(j))
            End If
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_pois_calc_flux
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_pois_calc_iter(This, Tau, ga, u, up, fl)
    !> Calculate the Time Step.
    !> {{{
    Class(MhdPoisSolver), Intent(InOut) :: This
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(ga%ncells_min:ga%ncells_max), Intent(In) :: u
    Real(8), Dimension(ga%ncells_min:ga%ncells_max), Intent(Out) :: up
    Real(8), Dimension(ga%nfaces_min:ga%nfaces_max), Intent(InOut) :: fl
    Real(8), Intent(In) :: Tau
    !> }}}
    Integer :: i, j, k
    Call This%calc_flux(ga, u, fl)
    !>-------------------------------------------------------------------------------
    !> Calculate the new Field values.
    !$OMP Parallel Do Private(i, j, k)
    Do i = ga%ncells_min, ga%ncells_max
        !> Update the values.
        up(i) = 0.0D0
        Do k = ga%cells(i)%nface, ga%cells(i)%nface_end
            j = ga%cell2face(k)
            If ( ga%faces(j)%ncell_p == i ) Then
                !> Inner normal case.
                up(i) = up(i) - fl(j)*ga%faces(j)%Sface
            Else
                !> Outer normal case.
                up(i) = up(i) + fl(j)*ga%faces(j)%Sface
            End If
        End Do
        up(i) = Tau/ga%cells(i)%Vcell*up(i)
        !> Check if values are correct.
        If ( IsNan(up(i)) ) Then
            If ( verbose ) Then
                Write (0,*) 'Invalid Poisson value was detected: ', up(i)
            End If
            Error Stop 1
        End If
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Subroutine mhd_pois_calc_iter
!########################################################################################################
!########################################################################################################
!########################################################################################################
Function mhd_pois_calc_dotp(This, ga, u, v) Result(dotp)
    !> Calculate Dot Product of the two functions.
    !> {{{
    Class(MhdPoisSolver), Intent(In) :: This
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(ga%ncells_min:ga%ncells_max), Intent(In) :: u, v
    !> }}}
    Real(8) :: dotp
    Integer :: i
    !>-------------------------------------------------------------------------------
    !> Calculate the Dot Product.
    dotp = 0.0D0
    !$OMP Parallel Do Private(i) Reduction(+:dotp) 
    Do i = ga%ncells_min, ga%ncells_max
        dotp = dotp + ( u(i)*v(i) )*ga%cells(i)%Vcell
    End Do
    !$OMP End Parallel Do
    !>-------------------------------------------------------------------------------
End Function mhd_pois_calc_dotp
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_pois_calc(This, ga, u_km, u_k, fl, f, n)
    !> Initialize the Poisson solver.
    !> {{{
    Class(MhdPoisSolver), Intent(InOut) :: This
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(ga%ncells_min:ga%ncells_max), Intent(In) :: f
    Real(8), Dimension(ga%ncells_min:ga%ncells_max), Intent(InOut) :: u_km, u_k
    Real(8), Dimension(ga%nfaces_min:ga%nfaces_max), Intent(InOut) :: fl
    Integer, Intent(Out) :: n
    !> }}}
    Integer :: i
    Logical :: Symmetric
    Real(8), Parameter :: Eps = 1D-6
    Real(8) :: Rho_k, Rho_km, &
               Omega_k, Omega_km, &
               Alpha_k, Alpha_km, &
               Beta_k, Delta, Delta1
    Real(8), Dimension(:), Allocatable :: r_k, r_km, r_0, &
                                          z_k, z_km, Az_km, &
                                          p_k, p_km, &
                                          v_k, v_km, &
                                          s_k, t_k
    
    !>-------------------------------------------------------------------------------
    !> Perallocate the intermediate fields.
    symmetric = .FALSE.
    If ( Symmetric ) Then
        Allocate(r_k(ga%ncells_min:ga%ncells_max)) 
        Allocate(z_k(ga%ncells_min:ga%ncells_max)) 
        Allocate(r_km(ga%ncells_min:ga%ncells_max)) 
        Allocate(z_km(ga%ncells_min:ga%ncells_max)) 
        Allocate(Az_km(ga%ncells_min:ga%ncells_max)) 
    Else
        Allocate(r_k(ga%ncells_min:ga%ncells_max)) 
        Allocate(p_k(ga%ncells_min:ga%ncells_max)) 
        Allocate(v_k(ga%ncells_min:ga%ncells_max)) 
        Allocate(s_k(ga%ncells_min:ga%ncells_max)) 
        Allocate(t_k(ga%ncells_min:ga%ncells_max)) 
        Allocate(r_0(ga%ncells_min:ga%ncells_max)) 
        Allocate(r_km(ga%ncells_min:ga%ncells_max)) 
        Allocate(p_km(ga%ncells_min:ga%ncells_max)) 
        Allocate(v_km(ga%ncells_min:ga%ncells_max))
    End If
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    Delta1 = Sqrt(This%calc_dotp(ga, f, f))
    If ( Symmetric ) Then
        !> Symmetric operator. Solve the Linear system using the CG.
        Call this%calc_iter(1.0D0, ga, u_km, u_k, fl)
        !$OMP Parallel Do
        Do i = ga%ncells_min, ga%ncells_max
            r_km(i) = f(i) - u_k(i)
            z_km(i) = r_km(i)
        End Do
        !$OMP End Parallel Do
        Delta = Sqrt(This%calc_dotp(ga, r_km, r_km))/Delta1
        If ( Delta >= Eps ) Then
            Do n = ga%ncells_min, 2*ga%ncells_max
                !> Perform the iteration.
                Call this%calc_iter(1.0D0, ga, z_km, Az_km, fl)
                Alpha_k = This%calc_dotp(ga, r_km, r_km)/This%calc_dotp(ga, Az_km, z_km)
                !$OMP Parallel Do
                Do i = ga%ncells_min, ga%ncells_max
                    u_k(i) = u_km(i) + Alpha_k*z_km(i)
                    r_k(i) = r_km(i) - Alpha_k*Az_km(i)
                End Do
                !$OMP End Parallel Do
                Beta_k = This%calc_dotp(ga, r_k, r_k)/This%calc_dotp(ga, r_km, r_km)
                !$OMP Parallel Do
                Do i = ga%ncells_min, ga%ncells_max
                    z_k(i) = r_k(i) + Beta_k*z_km(i)
                End Do
                !$OMP End Parallel Do
                !> Check convergence.
                Delta = Sqrt(This%calc_dotp(ga, r_k, r_k))/Delta1
                If ( Delta < Eps ) Then
                    !> Convergence was reached.
                    Exit
                End If
                !> Perpare for the next iteration.
                !$OMP Parallel Do
                Do i = ga%ncells_min, ga%ncells_max 
                    r_km(i) = r_k(i)
                    z_km(i) = z_k(i)
                    u_km(i) = u_k(i)
                End Do
                !$OMP End Parallel Do
            End Do
        End If
    Else
        !> Non-Symmetric operator. Solve the Linear system using the Good Old BiCGStab.
        Rho_km = 1.0D0
        Alpha_km = 1.0D0
        Omega_km = 1.0D0
        Call this%calc_iter(1.0D0, ga, u_km, u_k, fl)
        !$OMP Parallel Do
        Do i = ga%ncells_min, ga%ncells_max
            v_km(i) = 0.0D0
            p_km(i) = 0.0D0
            r_km(i) = f(i) - u_k(i)
            r_0(i) = r_km(i)
        End Do
        !$OMP End Parallel Do
        Delta = Sqrt(This%calc_dotp(ga, r_km, r_km))/Delta1
        If ( Delta >= Eps ) Then
            Do n = ga%ncells_min, 2*ga%ncells_max
                !> Perform the iteration.
                Rho_k = This%calc_dotp(ga, r_0, r_km)
                Beta_k = Rho_k/Rho_km * Alpha_km/Omega_km
                !$OMP Parallel Do
                Do i = ga%ncells_min, ga%ncells_max 
                    p_k(i) = r_km(i) + Beta_k*( p_km(i) - Omega_km*v_km(i) )
                End Do
                !$OMP End Parallel Do
                Call this%calc_iter(1.0D0, ga, p_k, v_k, fl)
                Alpha_k = Rho_k/This%calc_dotp(ga, r_0, v_k)
                !$OMP Parallel Do
                Do i = ga%ncells_min, ga%ncells_max 
                    s_k(i) = r_km(i) - Alpha_k*v_k(i)
                End Do
                !$OMP End Parallel Do
                Call this%calc_iter(1.0D0, ga, s_k, t_k, fl)
                Omega_k = This%calc_dotp(ga, t_k, s_k)/This%calc_dotp(ga, t_k, t_k)
                !$OMP Parallel Do
                Do i = ga%ncells_min, ga%ncells_max 
                    u_k(i) = u_km(i) + Omega_k*s_k(i) + Alpha_k*p_k(i)
                    r_k(i) = s_k(i) - Omega_k*t_k(i)
                End Do
                !> Check convergence.
                !$OMP End Parallel Do
                Delta = Sqrt(This%calc_dotp(ga, r_k, r_k))/Delta1
                If ( Delta < Eps ) Then
                    !> Convergence was reached.
                    Exit
                End If
                !> Perpare for the next iteration.
                Rho_km = Rho_k
                Alpha_km = Alpha_k
                Omega_km = Omega_k
                !$OMP Parallel Do
                Do i = ga%ncells_min, ga%ncells_max 
                    p_km(i) = p_k(i)
                    r_km(i) = r_k(i)
                    v_km(i) = v_k(i)
                    u_km(i) = u_k(i)
                End Do
                !$OMP End Parallel Do
            End Do
        End If
    End If
    !>-------------------------------------------------------------------------------
    
    !>-------------------------------------------------------------------------------
    !> Deallocate the intermediate fields.
    If ( Symmetric ) Then
        Deallocate(r_k)
        Deallocate(r_km)
        Deallocate(z_k)
        Deallocate(z_km)
        Deallocate(Az_km)
    Else
        Deallocate(r_k)
        Deallocate(p_k)
        Deallocate(v_k)
        Deallocate(s_k)
        Deallocate(t_k)
        Deallocate(r_0)
        Deallocate(r_km)
        Deallocate(p_km)
        Deallocate(v_km)
    End If
    !>-------------------------------------------------------------------------------
End Subroutine mhd_pois_calc
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_pois


