module list_of_lists_m
    use linked_list_m
    implicit none
    private

    type node
        integer :: index
        type(linked_list) :: list
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
    end type node

    type, public :: List_of_lists
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()

    contains
        procedure :: insert
        procedure :: print
    end type List_of_lists
    
contains
    subroutine insert(self, index, value)
        class(List_of_lists), intent(inout) :: self
        character(len=*), intent(in) :: value
        integer, intent(in) :: index

        type(node), pointer :: aux
        type(node), pointer :: new
        allocate(new)

        if(.not. associated(self%head)) then
            allocate(aux)
            aux%index = index
            self%head => aux
            self%tail => aux
            call aux%list%append(value)
        else
            if(index < self%head%index) then
                self%head%prev => new
                new%next => self%head
                self%head => new

                new%index = index
                call new%list%append(value)
            else
                aux => self%head
                do while (associated(aux%next))
                    if(index < aux%next%index) then
                        if(index == aux%index) then
                            call aux%list%append(value)
                        else
                            new%next => aux%next
                            new%prev => aux
                            aux%next%prev => new
                            aux%next => new

                            new%index = index
                            call new%list%append(value)
                        end if
                        return
                    end if
                    aux => aux%next
                end do

                if(index == aux%index) then
                    call aux%list%append(value)
                else
                    self%tail%next => new
                    new%prev => self%tail
                    self%tail => new

                    new%index = index
                    call new%list%append(value)
                end if
            end if
        end if
    end subroutine insert

    subroutine print(self)
        class(List_of_lists) :: self
        type(Node), pointer :: aux

        aux => self%head

        do while(associated(aux))
            print *, 'índice: ', aux%index
            call aux%list%print()
            print *, ""
            aux => aux%next
        end do
    end subroutine print

end module list_of_lists_m