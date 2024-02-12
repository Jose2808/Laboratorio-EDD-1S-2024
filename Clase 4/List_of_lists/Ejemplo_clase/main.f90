program name
    use List_of_lists_m
    implicit none

    type(List_of_lists):: list
    call list%insert(10, "Hello")
    call list%insert(10, "World")
    call list%insert(10, "!")

    call list%insert(0, "Hola")
    call list%insert(0, "mundo")
    call list%insert(0, "!")

    call list%insert(5, "Prueba")

    call list%insert(3, "Prueba")

    call list%printList()
    
end program name