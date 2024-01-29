module mi_modulo
    implicit none
    
    private
    public public_var, print_matrix
    
    real, parameter :: public_var = 2
    integer :: privat_var

contains

    subroutine print_matrix(A)
        real, intent(in) :: A(:,:)
        integer :: i

        do i = 1, size(A,1)
            print *, A(i,:)
        end do
    end subroutine print_matrix
end module mi_modulo

program main
    use mi_modulo
    implicit none

    real :: mat(10,10)

    mat(:, :) = 9
    call print_matrix(mat)
    
end program main