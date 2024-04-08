module lista_adyacencia_m
    implicit none
    
    type, private :: a_nodo
        integer :: destino
        type(a_nodo), pointer :: sig => null()
    end type a_nodo

    type, private :: v_nodo
        integer :: val
        type(v_nodo), pointer :: sig => null()
        type(a_nodo), pointer :: raiz => null()

    contains
        procedure :: insertarArista
        procedure :: graficarArista
    end type v_nodo

    type :: ListaAdyacencia
        private
        type(v_nodo), pointer :: raiz => null()

    contains
        procedure :: insert
        procedure :: crearConexion
        procedure :: crearGrafo
    end type ListaAdyacencia

contains
    !Funcionalidades de la lista de adyacencia
    subroutine insert(self, valor)
        class(ListaAdyacencia), intent(inout) :: self
        integer, intent(in) :: valor

        type(v_nodo), pointer :: aux
        type(v_nodo), pointer :: nuevo
        allocate(nuevo)
        nuevo%val = valor

        if(.not. associated(self%raiz)) then
            self%raiz => nuevo
        else
            aux => self%raiz
            if(valor < self%raiz%val) then
                nuevo%sig => self%raiz
                self%raiz => nuevo

            else
                do while(associated(aux%sig)) 
                    if(valor < aux%sig%val) then
                        nuevo%sig => aux%sig
                        aux%sig => nuevo
                        exit
                    end if
                    aux => aux%sig
                end do

                if(.not. associated(aux%sig)) then
                    aux%sig => nuevo
                end if
            end if
        end if
    end subroutine insert

    subroutine crearConexion(self, origen, destino)
        class(ListaAdyacencia), intent(inout) :: self
        integer, intent(in) :: origen
        integer, intent(in) :: destino

        type(v_nodo), pointer :: aux
        aux => self%raiz

        do while(associated(aux))
            if(aux%val == origen) then
                call aux%insertarArista(destino)
                exit
            end if
            aux => aux%sig
        end do
    end subroutine crearConexion

    subroutine crearGrafo(self)
        class(ListaAdyacencia), intent(inout) :: self

        type(v_nodo), pointer :: aux
        character(len=150) :: nodo_dec
        character(len=100) :: comando
        character(len=20) :: nombre
        character(len=10) :: str_aux
        integer :: io
        integer :: i

        aux => self%raiz
        io = 1
        comando = "dot -Tpng ./grafo.dot -o ./grafo.png"
        open(newunit=io, file='./grafo.dot')
        write(io, *) 'digraph g {'
        do while(associated(aux))
            write(str_aux, '(I10)') aux%val
            nombre = '"Nodo'//trim(adjustl(str_aux))//'"'
            nodo_dec = trim(adjustl(nombre))//'[label="'//trim(adjustl(str_aux))//'"]'
            write(io, *) nodo_dec
            call aux%graficarArista(io)
            aux => aux%sig
        end do
        write(io, *) '}'
        close(io)

        call execute_command_line(comando, exitstat=i)

        if (i == 1) then
            print *, "Ocurrió un error al generar la imagen"

        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine crearGrafo

    !Funcionalidades del nodo que contiene los vértices (v_node)
    subroutine insertarArista(self, destino)
        class(v_nodo), intent(inout) :: self
        integer, intent(in) :: destino

        type(a_nodo), pointer :: nuevo
        type(a_nodo), pointer :: aux

        allocate(nuevo)
        nuevo%destino = destino

        if(.not. associated(self%raiz)) then
            self%raiz => nuevo

        else
            aux => self%raiz
            do while(associated(aux%sig))
                if(aux%destino == destino) then
                    return
                end if
                aux => aux%sig
            end do
            aux%sig => nuevo
        end if
    end subroutine insertarArista

    subroutine graficarArista(self, io)
        class(v_nodo), intent(inout) :: self
        integer, intent(in) :: io

        type(a_nodo), pointer :: aux
        character(len=20) :: nombre_origen
        character(len=20) :: nombre_destino
        character(len=10) :: str_aux
        aux => self%raiz

        write(str_aux, '(I10)') self%val
        nombre_origen = '"Nodo'//trim(adjustl(str_aux))//'"'

        do while(associated(aux))
            if(aux%destino < self%val) then
                write(str_aux, '(I10)') aux%destino
                nombre_destino = '"Nodo'//trim(adjustl(str_aux))//'"'
                
                write(io, *) nombre_origen//'->'//nombre_destino//'[dir = both]'
            end if
            aux => aux%sig
        end do
    end subroutine graficarArista
end module lista_adyacencia_m