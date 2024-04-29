program main
    use bitsy
    implicit none

    print *, nonce('Estructuras de datos seccion A')
    
contains 
    function nonce(s) result(res)
        character(len=*), intent(in) :: s
        integer :: res
        character(len=64) :: hash

        hash = sha256(s)
        res = 0

        do while (index(hash, "0000000") /= 1)
            hash = sha256(hash)
            res = res + 1
        end do 
        print *, hash
    end function
end program main