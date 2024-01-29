subroutine routine(n, m, A)

    integer, intent(in) :: n
    integer, intent(in) :: m
    real, intent(in) :: A(n, m)

    integer :: i

    do i = 1, n
        print *, A(i, 1:m)
    end do

end subroutine routine

program name
    implicit none
    
    real :: mat(10, 20)
    mat(:,:) = 0.0

    call  routine(10,20,mat)
end program name