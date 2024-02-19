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
        procedure :: graph
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

    subroutine graph(self, io)
        class(circular_linked_list), intent(in) :: self
        integer, intent(out) :: io

        integer :: i
        integer :: index
        character(len=100), allocatable :: command
        character(:), allocatable :: connections
        character(:), allocatable :: firsts
        character(len=8) :: name
        type(node), pointer :: current

        current => self%head
        command = "dot -Tpng ./circular_linked_list.dot -o ./circular_linked_list.png"
        io = 1
        index = 0

        connections = ""
        firsts = ""
        !"Nodo 1" -> "Nodo 2"
        open(newunit=io, file='./circular_linked_list.dot')
        write(io, *) "digraph G {"

        if(associated(self%head)) then
            do while(.true.)
                write(name, '(I5)') index

                if(firsts == "") then
                    firsts = trim(name)
                end if

                if(associated(current%next, self%head)) then
                    print *, current%next%value, ","
                    connections = connections//'"Nodo'//trim(firsts)//'"'
                    exit
                end if
                
                write(io, *) '"Nodo'//trim(name)//'"[label = "', current%value, '"]'
                connections = connections//'"Nodo'//trim(name)//'"->'
                current => current%next
                index = index + 1
            end do
        end if

        write(io, *) connections
        write(io, *) "rankdir = LR"
        write(io, *) "}"
        close(io)

        call execute_command_line(command, exitstat=i)

        if(i == 1) then
            print *, "Ocurrió un error"
        else
            print *, "Imagen generada satisfactoriamente"
        end if
    end subroutine graph
end module Circular_linked_list_m
