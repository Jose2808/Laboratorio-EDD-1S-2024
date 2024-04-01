module tabla_hash_m
    implicit none
    private
    integer :: M = 13
    real :: R = 0.618034
    integer, parameter :: long = selected_int_kind(4)
    integer, parameter :: dp = selected_real_kind(15)

    type, public :: TablaHash
        integer :: n = 0
        integer, allocatable :: tabla(:)

    contains
        procedure :: insert
        procedure :: print
        procedure :: search
        procedure, private :: pruebaLineal
    end type TablaHash

contains
    subroutine insert(self, key)
        class(TablaHash), intent(inout) :: self
        type(TablaHash) :: ret
        integer, allocatable :: temp(:)
        integer(long), intent(in) :: key
        integer :: pos

        if(.not. allocated(self%tabla)) then
            allocate(self%tabla(0:M-1))
            self%tabla(:) = -1
        end if

        pos = funcion_hash(key)

        if(self%tabla(pos) /= -1 .and. self%tabla(pos) /= key) then
            call self%pruebaLineal(pos)
        end if

        self%tabla(pos) = key
        self%n = self%n + 1

        if(self%n * 1.0_dp/M > 0.75) then
            temp = self%tabla
            deallocate(self%tabla)
            ret = rehashing(temp)
            self%tabla = ret%tabla
            self%n = ret%n
        end if
    end subroutine

    function rehashing(temp) result(val)
        integer, intent(in) :: temp(:)
        integer :: i
        type(TablaHash) :: val

        M = M*2
        allocate(val%tabla(0:M-1))
        val%tabla(:) = -1

        do i = 1, size(temp)
            if(temp(i) /= -1) then
                call val%insert(int(temp(i), kind=long))
            end if
        end do
    end function

    subroutine pruebaLineal(self, pos)
        class(TablaHash), intent(inout) :: self
        integer, intent(inout) :: pos

        do while(self%tabla(pos) /= -1)
            pos = pos + 1
            pos = mod(pos, M)
        end do
    end subroutine pruebaLineal

    subroutine search(self, val)
        class(TablaHash), intent(in) :: self
        integer, intent(in) :: val
        integer :: pos

        pos = funcion_hash(int(val, kind=long))
        print *, self%tabla(pos)
    end subroutine search

    subroutine print(self)
        class(TablaHash), intent(in) :: self
        print *, self%tabla
    end subroutine print

    function funcion_hash(x) result(v)
        integer(long), intent(in) :: x
        real :: d
        integer :: v

        d = R*x - floor(R*x)
        v = floor(M*d)
    end function funcion_hash
end module tabla_hash_m