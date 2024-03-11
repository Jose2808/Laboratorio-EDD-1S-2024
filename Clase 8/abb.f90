module abb_m
    use uuid_module
    implicit none
    private

    type :: nodo
        integer :: valor
        type(nodo), pointer :: derecha => null()
        type(nodo), pointer :: izquierda => null() 
    end type

    type, public :: abb
        type(nodo), pointer :: raiz => null()
    
    contains
        procedure :: insert
        procedure :: preorden
        procedure :: graficar
    end type abb

contains
    subroutine insert(self, val)
        class(abb), intent(inout) :: self
        integer, intent(in) :: val

        call insertRec(self%raiz, val)
    end subroutine insert

    subroutine preorden(self)
        class(abb), intent(in) :: self
        
        call preordenRec(self%raiz)
    end subroutine preorden

    subroutine graficar(self)
        class(abb), intent(in) :: self
        integer :: io
        integer :: i
        character(len=100) :: comando

        io = 1
        open(newunit=io, file="./abb_tree.dot")
        comando = "dot -Tpng ./abb_tree.dot -o ./abb_tree.png"

        write(io, *) "digraph G {"
            !Graficar
        if(associated(self%raiz)) then
            call imprimirRec(self%raiz, generate_uuid(), io)
        end if
        write(io, *) "}"
        close(io)

        call execute_command_line(comando, exitstat=i)

        if(i == 1) then
            print *, "Error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graficar

    recursive subroutine insertRec(raiz, val)
        type(nodo), pointer, intent(inout) :: raiz
        integer, intent(in) :: val

        if(.not. associated(raiz)) then
            allocate(raiz)
            raiz = nodo(valor=val)
        
        else if(val < raiz%valor) then 
            call insertRec(raiz%izquierda, val)

        else if(val > raiz%valor) then
            call insertRec(raiz%derecha, val)
        end if

        
    end subroutine insertRec

    recursive subroutine preordenRec(raiz)
        type(nodo), pointer, intent(in) :: raiz

        if(associated(raiz)) then
            print *, raiz%valor
            call preordenRec(raiz%izquierda)
            call preordenRec(raiz%derecha)
        end if
    end subroutine preordenRec

    recursive subroutine imprimirRec(raiz, nombre, io)
        type(nodo), pointer, intent(in) :: raiz
        character(len=36), intent(in) :: nombre
        integer :: io

        character(len=36) :: derecha
        character(len=36) :: izquierda

        derecha = generate_uuid()
        izquierda = generate_uuid()

        if(associated(raiz)) then
            !"Nodo_uuid"[Label="1"]
            write(io, *) '"Nodo'//nombre//'"[label= "', raiz%valor, '"]'

            if(associated(raiz%izquierda)) then
                !"Nodo_uuid"->"Nodo_uuidHijoIzquierdo"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//izquierda//'"'
            end if

            if(associated(raiz%derecha)) then
                !"Nodo_uuid"->"Nodo_uuidHijoDerecho"
                write(io, *) '"Nodo'//nombre//'"->"Nodo'//derecha//'"'
            end if
            call imprimirRec(raiz%izquierda, izquierda, io)
            call imprimirRec(raiz%derecha, derecha, io)
        end if
    end subroutine imprimirRec
end module abb_m