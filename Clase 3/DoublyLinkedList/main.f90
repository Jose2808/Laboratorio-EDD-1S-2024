program main
    use doubly_linked_list_m
    implicit none

    type(doubly_linked_list) :: list

    call list%insert(0, 10)
    call list%insert(0, 20)
    call list%insert(0, 30)
    call list%insert(10, 50)
    call list%insert(2, 1000)

    print *, "Imprimiendo lista:"
    call list%print()

    print *, "Imprimiendo lista al contrario:"
    call list%printRev()
end program main