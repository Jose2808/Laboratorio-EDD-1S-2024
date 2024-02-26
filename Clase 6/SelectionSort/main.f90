program main
    implicit none
    integer :: a(15)
    a = [50,23,80,10,5,6,21,100,1,0,20,30,80,60,40]
    print *, a
    call selectionSort(a)
    print *, a
contains
    subroutine selectionSort(array)
        integer, intent(inout) :: array(:)
        integer :: n
        integer :: i
        integer :: j
        integer :: min_idx
        integer :: temp

        n = size(array)
        do i = 1, n
            min_idx = i
            do j = i + 1, n
                if(array(j) > array(min_idx)) then
                    min_idx = j
                end if
            end do

            temp = array(min_idx)
            array(min_idx) = array(i)
            array(i) = temp
        end do
    end subroutine selectionSort
end program main