program main
    use matrix_m
    implicit none
    
    type(matrix) :: m

    call m%insert(1,3,.false.)
    call m%insert(8,4,.false.)
    call m%insert(5,0,.false.)
    call m%insert(2,6,.false.)
    call m%insert(9,7,.false.)
    call m%insert(0,1,.true.)
    call m%insert(1,1,.true.)

    call m%graficar()
    call m%print()
end program main