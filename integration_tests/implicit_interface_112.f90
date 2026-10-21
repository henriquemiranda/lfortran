! A module procedure calls an external procedure defined later in the same file,
! a subroutine and a function.
module implicit_interface_112_m
contains
    subroutine a(x)
        integer :: x
        call b(x)
    end subroutine
    real function f(x)
        real, intent(in) :: x
        f = h(x)
    end function
end module

subroutine b(x)
    integer :: x
    x = x + 1
end subroutine

real function h(y)
    real :: y
    h = y + 1
end function

program implicit_interface_112
    use implicit_interface_112_m
    integer :: i
    i = 1
    call a(i)
    if (i /= 2) error stop
    if (abs(f(1.0) - 2.0) > 1e-6) error stop
end program
