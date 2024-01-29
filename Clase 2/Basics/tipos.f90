program main
    implicit none
    
    type :: t
        integer :: i
        logical :: bool
    end type t

    type(t) :: var

    var%i = 9
    var%bool = .true.

    print *, "Atributos: ", var%i, var%bool

    var = t(9, .true.)
    var = t(i = 9, bool = .true.)
end program main