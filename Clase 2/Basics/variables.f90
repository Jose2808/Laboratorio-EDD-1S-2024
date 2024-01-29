program main
    implicit none
    
    integer :: age
    age = 0

    print *, 'Hello World!'
    print *, 'El valor de age es: ', age

    print *, "Ingrese su edad: "
    read(*,*) age
    print *, "Su edad es: ", age

end program main