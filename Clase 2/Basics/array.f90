program array
    implicit none
    
    integer, dimension(10) :: array_1
    integer :: array_2(10)
    integer, dimension(10,10) :: array_3
    integer :: i
    integer :: array_4(0:9)

    i = 0

    array_1 = [1,2,3,4,5,6,7,8,9,10]
    print *, array_1

    print *, array_1(1:10:2)
    print *, array_1(10:1:-1)

    array_2(:) = 0

    array_2 = [(i, i=1, 10)]
    print *, array_2

end program array