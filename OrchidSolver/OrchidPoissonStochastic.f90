!> Orchid -- 2D/3D Euler/MagnetoHydroDynamics solver.
!> Copyright (C) Butakov Oleg 2019.
    
Module orchid_solver_poisson_stochastic
Use orchid_solver_params
Use orchid_solver_grid
Implicit None
Type :: MhdPoissonFixedRandomWalk
    Contains
    Procedure, Public :: calc => mhd_poisson_fixed_random_walk
End Type MhdPoissonFixedRandomWalk
Contains
!########################################################################################################
!########################################################################################################
!########################################################################################################
Subroutine mhd_poisson_fixed_random_walk(This, ga, u, f)
    !> Solve the Poisson Equation using the Fixed Random walk method.
    !> {{{
USE IFPORT
    Class(MhdPoissonFixedRandomWalk), Intent(In) :: This
    Class(MhdGrid), Intent(In) :: ga
    Real(8), Dimension(ga%ncells_min:ga%ncells_max), Intent(Out) :: u
    Real(8), Dimension(ga%ncells_min:ga%ncells_max), Intent(In) :: f
    !> }}}
    Integer :: i, ic, ip, id, j, k, m, n, s
    Real(8) :: e, d, rx, ry, rz, w, h, Eps, l, x, y
    !$OMP Parallel Do Private(ic, ip, j, k, m, n, s, e, d, rx, ry, w, h, Eps, l, x, y)
    Do i = ga%ncells_min, ga%ncells_max
        e = 0.0D0
        d = 0.0D0
        l = 0.0D0
        x = ga%cells(i)%x
        y = ga%cells(i)%y
        ic = i
        Do n = 1, 1000
            w = 0.0D0
            Do While ( .TRUE. )
                !> Determine what cell is next.
                Call Random_Number(rx)
                Call Random_Number(ry)
                Call Random_Number(rz)
                m = ga%cells(ic)%nface + Int(rx * 2) + Int(ry * 2) + Int(rz * 2)
                m = ga%cell2face(m)
                If ( ga%faces(m)%ncell_p == ic ) Then
                    ip = ga%faces(m)%ncell_m
                Else
                    ip = ga%faces(m)%ncell_p
                End If
                If ( ip < 0 ) Then
                    !> Boundary is reached,
                    !> the particle is absorbed.
                    l = l + 1.0D0
                    Exit
                Else
                    !> Integrate the RHS.
                    h = Sqrt(( ga%cells(ip)%x - ga%cells(ic)%x )**2 + &
                             ( ga%cells(ip)%y - ga%cells(ic)%y )**2 + &
                             ( ga%cells(ip)%z - ga%cells(ic)%z )**2)
                    w = w + 0.5D0*h*( f(ic) + f(ip) )
                    ic = ip
                End If
            End Do
            e = e + w
            d = d + w**2
            Eps = Abs(e/l - exp(x)*exp(y)*(x-x**2)*(y-y**2) )!Sqrt(Abs(d/l - e*e/l/l))/l
            If ( Eps < 2D-3 ) Then
                Exit
            End If
        End Do
        Write(*,*) i, '/', ga%ncells_max, ':', eps, ' ', l
        u(i) = e/l
    End Do
    !$OMP End Parallel Do
End Subroutine mhd_poisson_fixed_random_walk
!########################################################################################################
!########################################################################################################
!########################################################################################################
End Module orchid_solver_poisson_stochastic 
    