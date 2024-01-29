program string
    implicit none
    
    character(len=4) :: first_name
    character(len=4) :: last_name
    character(len=8) :: complete
    character(:), allocatable :: first_name1

    first_name = "John"
    last_name = "John"

    first_name1 = "John"

    complete = first_name//last_name
end program string