program nonce
    use bitsy 
    implicit none
    
    integer :: n
    character(len=:), allocatable :: hash
    character(len=:), allocatable :: d
    d = "Esta es informacion"


    hash = sha256(d)
    do while(hash(0) /= '0' .and. hash(1) /= '0')
        hash = sha256(hash)
        n = n + 1 
    end do
end program nonce