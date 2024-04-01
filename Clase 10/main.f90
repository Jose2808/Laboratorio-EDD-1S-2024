program main
    use tabla_hash_m
    implicit none
    
    type(TablaHash) :: tabla
    integer, parameter :: long = selected_int_kind(4)

    call tabla%insert(int(5, kind=long))
    call tabla%insert(int(522, kind=long))
    call tabla%insert(int(16, kind=long))
    call tabla%insert(int(1, kind=long))
    call tabla%insert(int(18, kind=long))
    call tabla%insert(int(29, kind=long))
    call tabla%insert(int(42, kind=long))
    call tabla%insert(int(500, kind=long))
    call tabla%insert(int(60, kind=long))
    call tabla%insert(int(7, kind=long))
    call tabla%print()
    call tabla%search(522)
end program main