program main
    use abb_m
    implicit none
    type(abb) :: a

    call a%insert(20)
    call a%insert(10)


    print *, "Imprimiendo en preorden: "
    call a%preorden()
    call a%graficar()
end program main