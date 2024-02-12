module Circular_linked_list_m
    implicit none
    private
    
    type :: node
        private
        integer :: value
        type(node), pointer :: next => null()
    end type node

    type, public :: circular_linked_list
        private
        type(node), pointer :: head => null()

    contains
        procedure :: push
        procedure :: append
        procedure :: print
        ! final :: destructor
    end type circular_linked_list

contains
    subroutine push(self, value)
        class(circular_linked_list), intent(inout) :: self
        integer, intent(in) :: value

        type(node), pointer :: new
        type(node), pointer :: aux
        allocate(new)

        new = node(value=value, next=null())

        if(associated(self%head)) then
            aux => self%head
            do while(.not. associated(aux%next, self%head))
                aux => aux%next
            end do
            new%next => self%head
            aux%next => new
        else
            new%next => new
        end if
        self%head => new
    end subroutine push

    subroutine append(self, value)
        class(circular_linked_list), intent(inout) :: self
        integer, intent(in) :: value

        type(node), pointer :: new
        type(node), pointer :: aux
        allocate(new)

        new = node(value=value, next=null())

        if(associated(self%head)) then
            aux => self%head
            do while(.not. associated(aux%next, self%head))
                aux => aux%next
            end do
            new%next => self%head
            aux%next => new
        else
            new%next => new
            self%head => new
        end if

    end subroutine append

    subroutine print(self)
        class(circular_linked_list), intent(in) :: self
        type(node), pointer :: aux
        aux => self%head

        if(associated(self%head)) then
            do while(.true.)
                print *, aux%value, ','

                if(associated(aux%next, self%head)) then
                    print *, aux%next%value, ','
                    exit
                end if
                aux => aux%next
            end do
        end if
    end subroutine print
end module Circular_linked_list_m
