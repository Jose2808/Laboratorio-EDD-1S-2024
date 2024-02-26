program main
    implicit none
    integer :: a(15)
    a = [50,23,80,10,5,6,21,100,1,0,20,30,80,60,40]
    print *, a
    call insertionSort(a)
    print *, a
contains
    subroutine insertionSort(array)
        integer, intent(inout) :: array(:)
        integer :: n
        integer :: i
        integer :: j
        integer :: key

        n = size(array)
        do i = 2, n
            key = array(i)
            j = i - 1

            do while((j >= 1) .and. (array(j) < key))
                array(j+1) = array(j)
                j = j - 1
            end do
            array(j + 1) = key
        end do
    end subroutine insertionSort
end program main