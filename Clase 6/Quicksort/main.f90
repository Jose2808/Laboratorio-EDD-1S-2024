program main
    implicit none
    integer :: a(15)
    a = [50,23,80,10,5,6,21,100,1,0,20,30,80,60,40]
    print *, a
    call quicksort(a, 1, size(a))
    print *, a
contains
    recursive subroutine quicksort(arr, first, last)
        integer arr(:), x, t
        integer first, last
        integer i,j

        x = arr((first+last)/2)
        i = first
        j = last

        do
            do while(arr(i) < x)
                i = i + 1
            end do

            do while(x < arr(j))
                j = j - 1
            end do
            if(i >= j) exit
            t = arr(i)
            arr(i) = arr(j)
            arr(j) = t
            i = i + 1
            j = j -1
        end do
        if (first < i - 1) call quicksort(arr, first, i-1)
        if (j+1 < last) call quicksort(arr, j+1, last)
    end subroutine quicksort
end program main