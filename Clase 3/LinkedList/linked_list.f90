module linked_list_m
    implicit none
    private

    type :: node
        private
        integer :: value
        type(node), pointer :: next => null()
    end type node

    type, public :: linked_list
        private
        type(node), pointer :: head => null()

    contains
        procedure :: push
        procedure :: append
        ! procedure :: insert
        procedure :: print
        final :: destructor
    end type linked_list

contains
    subroutine push(self, value)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: value

        type(node), pointer :: new
        allocate(new)

        new%value = value

        if(.not. associated(self%head)) then
            self%head => new
        else    
            new%next => self%head
            self%head => new
        end if
    end subroutine push

    subroutine append(self, value)
        class(linked_list), intent(inout) :: self
        integer, intent(in) :: value

        type(node), pointer :: current
        type(node), pointer :: new
        allocate(new)

        new%value = value

        if(.not. associated(self%head)) then
            self%head => new
        else
            current => self%head
            do while(associated(current%next))
                current => current%next
            end do

            current%next => new
        end if

    end subroutine append

    subroutine print(self)
        class(linked_list), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            print *, current%value, ","
            current => current%next
        end do
    end subroutine print

    subroutine destructor(self)
        type(linked_list), intent(inout) :: self
        type(node), pointer :: aux

        do while(associated(self%head))
            aux => self%head%next
            deallocate(self%head)
            self%head = aux
        end do
    end subroutine destructor


end module linked_list_m