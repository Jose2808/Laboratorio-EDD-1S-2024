program main
    implicit none
    integer :: a(10)
    a = [50,23,80,10,5,6,21,100,1,0]
    print *, a
    call bubbleSort(a)
    print *, a
contains
    subroutine bubbleSort(array)
        integer, intent(inout) :: array(:)
        integer :: n
        integer :: i
        integer :: j
        integer :: temp

        n = size(array)
        do i = 1, n
            do j = 1, n - i
                if(array(j) < array(j + 1)) then
                    temp = array(j)
                    array(j) = array(j+1)
                    array(j+1) = temp
                end if
            end do
        end do
    end subroutine
end program main