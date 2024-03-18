program main
    use avl_m
    implicit none
    type(avl) :: a

    call a%insert(20)
    call a%insert(10)
    call a%insert(8)
    call a%insert(19)
    call a%insert(30)
    call a%insert(29)
    call a%insert(35)
    call a%insert(9)
    call a%insert(7)

    call a%delete(35)
    call a%delete(7)
    call a%delete(9)

    print *, "Imprimiendo en preorden: "
    call a%preorden()
    call a%graficar()
end program main