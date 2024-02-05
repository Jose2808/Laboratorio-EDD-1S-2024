module doubly_linked_list_m
    implicit none
    
    type :: node
        private
        integer :: value
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
    end type node

    type, public :: doubly_linked_list
        private 
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()

    contains
        procedure :: append
        procedure :: push
        procedure :: insert
        procedure :: print
        procedure :: printRev
    end type
contains

    subroutine push(self, value)
        class(doubly_linked_list), intent(inout) :: self 
        integer, intent(in) :: value  

        type(node), pointer :: new
        allocate(new)
        new%value = value

        if(.not. associated(self%head)) then
            self%head => new
            self%tail => new
        else
            new%next => self%head
            self%head%prev => new
            self%head => new
        end if
    end subroutine push

    subroutine append(self, value)
        class(doubly_linked_list), intent(inout) :: self 
        integer, intent(in) :: value  

        type(node), pointer :: new
        allocate(new)
        new%value = value

        if(.not. associated(self%head)) then
            self%head => new
            self%tail => new  
        else
            new%prev => self%tail
            self%tail%next => new
            self%tail => new
        end if
    end subroutine

    subroutine insert(self, index, value)
        class(doubly_linked_list), intent(inout) :: self
        type(node), pointer :: actual
        integer, intent(in) :: index
        integer, intent(in) :: value
        integer :: i

        type(node), pointer :: new
        allocate(new)
        new%value = value
        i = 0
        
        actual => self%head

        if((.not. associated(self%head)) .or. (index == 0)) then
            call self%push(value)
        else
            do while((associated(actual%next)) .and. (i < index -1))
                actual => actual%next
                i = i + 1
            end do

            if(i < index - 1) then
                call self%append(value)
                return
            end if

            new%next => actual%next
            new%prev => actual
            actual%next%prev => new
            actual%next => new
        end if
    end subroutine insert

    subroutine print(self)
        class(doubly_linked_list), intent(in) :: self
        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            print *, current%value, ","
            current => current%next
        end do
    end subroutine print

    subroutine printRev(self)
        class(doubly_linked_list), intent(in) :: self
        type(node), pointer :: current
        current => self%tail

        do while(associated(current))
            print *, current%value, ","
            current => current%prev
        end do
    end subroutine printRev
    
end module doubly_linked_list_m