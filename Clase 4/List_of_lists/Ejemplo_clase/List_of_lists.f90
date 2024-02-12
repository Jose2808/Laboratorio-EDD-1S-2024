module List_of_lists_m
    implicit none
    private

    type :: string_node
        character(:), allocatable :: value
        type(string_node), pointer :: next => null()
    end type string_node

    type :: node
        integer :: index
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
        type(string_node), pointer :: head => null()
    contains
        procedure :: append
        procedure :: print
    end type node
    
    type, public :: List_of_lists
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    
    contains
        procedure :: insert
        procedure :: printList
    end type List_of_lists

contains
!index = 1
!0 -> ...  <- aux
!1 ->
!4 -> ...  <- aux%next
    subroutine insert(self, index, value)
        class(List_of_lists), intent(inout) :: self
        integer, intent(in) :: index
        character(len=*), intent(in) :: value

        type(node), pointer :: aux
        type(node), pointer :: new
        allocate(new)

        if(.not. associated(self%head)) then
            new%index = index
            self%head => new
            self%tail => new
            call new%append(value)
        else
            if(index < self%head%index) then
                self%head%prev => new
                new%next => self%head
                self%head => new

                new%index = index
                call new%append(value)
            else
                aux => self%head
                do while (associated(aux%next))
                    if(index < aux%next%index) then
                        if(index == aux%index) then
                            call aux%append(value)
                        else
                            new%next => aux%next
                            new%prev => aux
                            aux%next%prev => new
                            aux%next => new

                            new%index = index
                            call new%append(value)
                        end if
                        return
                    end if
                    aux => aux%next
                end do

                if(index == aux%index) then
                    call aux%append(value)
                else
                    self%tail%next => new
                    new%prev => self%tail
                    self%tail => new

                    new%index = index
                    call new%append(value)
                end if
            end if
        end if
    end subroutine insert

    subroutine printList(self)
        class(List_of_lists), intent(in) :: self
        type(node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'INDICE: ', aux%index
            call aux%print()
            print *, ""
            aux => aux%next
        end do
    end subroutine printList

    subroutine append(self, value)
        class(node), intent(inout) :: self
        character(len=*), intent(in) :: value

        type(string_node), pointer :: aux
        type(string_node), pointer :: new
        allocate(new)
        new%value = value

        if(.not. associated(self%head)) then
            self%head => new
        else
            aux => self%head
            do while(associated(aux%next))
                aux => aux%next
            end do
            aux%next => new
        end if
    end subroutine

    subroutine print(self)
        class(node), intent(in) :: self
        type(string_node), pointer :: aux
        aux => self%head

        do while(associated(aux))
            print *, aux%value
            aux => aux%next
        end do
    end subroutine print

end module List_of_lists_m