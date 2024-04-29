module interfazC
    use iso_c_binding, only : c_char, c_ptr
    implicit none
    
    interface
        function encode(plain) bind(C, name='base64_encode') result(res)
            import c_char, c_ptr
            character(kind=c_char), dimension(*) :: plain
            type(c_ptr) :: res
        end function encode

        function decode(cipher) bind(C, name='base64_decode') result(res)
            import c_char, c_ptr
            character(kind=c_char), dimension(*) :: cipher
            type(c_ptr) :: res
        end function decode
    end interface    
end module interfazC

program main
    use interfazC
    use iso_c_binding, only : c_ptr, c_f_pointer, c_null_char
    implicit none
    type(c_ptr) :: e
    character(len=:), pointer :: e_str
    character(len=:), allocatable :: route

    route = "D:\monte\OneDrive\Escritorio\Universidad\"
    route = route//"Practicas Finales\Estructuras de Datos\Fortran\"
    route = route//"encode-decode\cat.jpg"

    e = encode(route)
    call c_f_pointer(CPTR=e, FPTR=e_str)
    print *, 'IMPRIMIENDO LA IMAGEN EN BASE64'
    print *, e_str
end program main