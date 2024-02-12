program main
    use list_of_lists_m
    implicit none

    type(List_of_lists) :: list

    call list%insert(0, "Hello")
    call list%insert(0, "World")
    call list%insert(0, "!")

    call list%insert(0, "Hello")
    call list%insert(1, "World")
    call list%insert(1, "!")

    call list%insert(4, "Test")
    call list%insert(4, "2")

    call list%insert(3, "New")
    call list%insert(3, "Test")

    call list%insert(4, "Test")
    call list%insert(4, "new")

    call list%insert(2, "Test")
    call list%insert(2, "new")

    call list%print()
end program main