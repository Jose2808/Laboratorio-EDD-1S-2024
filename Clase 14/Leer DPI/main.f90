program main
    use json_module
    use iso_fortran_env, only:int64
    implicit none
    
    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: object_p, dpi_p
    character(:), allocatable :: dpi
    integer(kind=int64) :: dpi_int
    call json%initialize()
    call json%load('./test.json')
    
    call json%get_core(jsonc)
    call json%get('', object_p)
    call jsonc%get_child(object_p, 'dpi', dpi_p)
    call jsonc%get(dpi_p, dpi)
    print *, dpi

    read(dpi, *) dpi_int    

    print *, dpi_int - 12
    call json%destroy()
end program main