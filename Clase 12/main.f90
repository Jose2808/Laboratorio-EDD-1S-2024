program main
    use merkle_m
    implicit none
    
    type(merkle) :: m
    character(len=:), allocatable :: val

    val = "prueba1"
    call m%agregar(val)
    val = "prueba2"
    call m%agregar(val)
    val = "prueba3"
    call m%agregar(val)
    val = "prueba4"
    call m%agregar(val)
    val = "prueba5"
    call m%agregar(val)
    val = "prueba6"
    call m%agregar(val)

    call m%generar()

    call m%dot()
end program main