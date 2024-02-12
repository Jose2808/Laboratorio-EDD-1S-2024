program main
    use Circular_linked_list_m
    implicit none

    type(circular_linked_list) :: list
    call list%push(1)
    call list%push(2)
    call list%push(3)
    call list%push(4)
    call list%push(5)
    call list%push(6)

    call list%append(10)
    call list%append(20)
    call list%append(30)
    call list%append(40)
    call list%append(50)
    call list%append(60)
    
    call list%print()
end program main