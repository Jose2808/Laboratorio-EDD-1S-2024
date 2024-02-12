module linked_list_m
    implicit none
    private
    type :: node
        character(:), allocatable :: value
        type(node), pointer :: next => null()
    end type node

    type, public :: linked_list
        type(node), pointer :: head => null()

    contains
        procedure :: append
        procedure :: print
    end type linked_list
contains
    subroutine append(self, string)
        class(linked_list), intent(inout) :: self
        character(len=*), intent(in) :: string

        type(node), pointer :: aux
        type(node), pointer :: new

        allocate(new)
        new%value = string

        if(.not. associated(self%head)) then
            self%head => new
        else
            aux => self%head
            do while(associated(aux%next))
                aux => aux%next
            end do
            aux%next => new
        end if
    end subroutine append

    subroutine print(self)
        class(linked_list), intent(inout) :: self

        type(node), pointer :: aux
        aux => self%head

        do while(associated(aux))
            print *, aux%value
            aux => aux%next
        end do
    end subroutine print
end module linked_list_m