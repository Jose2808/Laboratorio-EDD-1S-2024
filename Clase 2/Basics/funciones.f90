function vector_norm(n, vec) result(norm)
    implicit none

    integer, intent(in) :: n
    real, intent(in) :: vec(n)
    real :: norm

    norm = sqrt(sum(vec**2))
    
end function vector_norm

program main
    implicit none
    
    real :: v(9)
    real :: vector_norm

    v(:) = 9

    print *, vector_norm(9,v)
end program main