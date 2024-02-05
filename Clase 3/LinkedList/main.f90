program main
    use linked_list_m
    implicit none
    
    type(linked_list) :: list

    call list%push(1)
    call list%push(3)
    call list%push(5)
    call list%push(7)
    call list%push(9)

    call list%append(11)
    call list%append(13)
    call list%append(15)
    
    call list%print()
end program main