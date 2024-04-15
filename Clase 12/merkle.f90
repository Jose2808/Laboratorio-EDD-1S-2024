module merkle_m
    use bitsy
    implicit none
    
    integer :: uid = 1

    type :: nodo_data
        integer :: uid
        character(len = :), allocatable :: valor
        type(nodo_data), pointer :: sig => null()
    end type nodo_data

    type :: nodo_hash
        integer :: uid
        character(len= : ), allocatable :: hash
        type(nodo_hash), pointer :: izquierda => null()
        type(nodo_hash), pointer :: derecha => null()
        type(nodo_data), pointer :: data => null()
    end type nodo_hash

    type :: merkle
        type(nodo_hash), pointer :: tophash => null()
        type(nodo_data), pointer :: raiz_data => null()
        type(nodo_data), pointer :: ultimo_data => null()
        integer :: pos = 0
    
    contains 
        procedure :: agregar
        procedure :: generar
        procedure :: largo_data
        procedure :: obtener_data
        procedure :: crear_arbol
        procedure :: generar_hash
        procedure :: dot
        procedure :: dot_rec
    end type
contains
    
    subroutine agregar(self, val)
        class(merkle), intent(inout) :: self
        character(len = :), allocatable, intent(in) :: val
        type(nodo_data), pointer :: nuevo

        allocate(nuevo)
        allocate(nuevo%valor, source = val)
        nuevo%uid = uid
        uid = uid + 1

        if(.not. associated(self%raiz_data)) then
            self%raiz_data => nuevo
            self%ultimo_data => nuevo
        
        else
            self%ultimo_data%sig => nuevo
            self%ultimo_data => nuevo
        end if
    end subroutine agregar

    subroutine generar(self)
        class(merkle), intent(inout) :: self
        integer :: exponente = 1
        integer :: i
        integer :: p
        character(len=:), allocatable :: val
        val = "-1"

        do while(2**exponente < self%largo_data())
            exponente = exponente + 1
        end do

        p = 2**exponente
        self%pos = p
        i = self%largo_data()

        do while(i<p)
            call  self%agregar(val)
            i = i + 1
        end do

        allocate(self%tophash)
        call self%crear_arbol(self%tophash, exponente)
        call self%generar_hash(self%tophash, p)
    end subroutine generar

    recursive subroutine crear_arbol(self, nodo, expo)
        class(merkle), intent(inout) :: self 
        type(nodo_hash), pointer, intent(inout) :: nodo
        integer, intent(in) :: expo
        
        nodo%uid = uid
        uid = uid + 1

        if(expo > 0) then
            allocate(nodo%izquierda)
            allocate(nodo%derecha)
            call self%crear_arbol(nodo%izquierda, expo - 1)
            call self%crear_arbol(nodo%derecha, expo - 1)
        end if
    end subroutine crear_arbol

    recursive subroutine generar_hash(self, nodo, p)
        class(merkle), intent(inout) :: self
        type(nodo_hash), pointer, intent(inout) :: nodo 
        integer, intent(in) :: p
        integer :: temp
        type(nodo_data), pointer :: data
        character(len = :), allocatable :: hash

        if(associated(nodo)) then
            call self%generar_hash(nodo%izquierda, p)
            call self%generar_hash(nodo%derecha, p)

            if(.not. associated(nodo%izquierda) .and. .not. associated(nodo%derecha)) then
                temp = p - self%pos
                nodo%data => self%obtener_data(temp)
                self%pos = self%pos - 1
                hash = nodo%data%valor
                nodo%hash = sha256(hash)

            else
                hash = adjustl(nodo%izquierda%hash(1:len(nodo%izquierda%hash)/2))
                hash = hash//adjustl(nodo%derecha%hash(1:len(nodo%derecha%hash)/2))
                nodo%hash = sha256(hash) 
            end if
        end if
    end subroutine generar_hash

    function largo_data(self) result(res)
        class(merkle), intent(inout) :: self
        type(nodo_data), pointer :: aux
        integer :: res    

        res = 0
        aux => self%raiz_data
        do while(associated(aux)) 
            res = res + 1
            aux => aux%sig
        end do
    end function largo_data

    function obtener_data(self, pos) result(aux)
        class(merkle), intent(inout) :: self
        integer, intent(inout) :: pos
        type(nodo_data), pointer :: aux

        aux => self%raiz_data
        do while(associated(aux))
            if(pos == 0) return
            pos = pos - 1
            aux => aux%sig
        end do
    end function obtener_data

    subroutine dot(self)
        class(merkle), intent(inout) :: self
        integer :: io
        io = 1
        
        open(io, file='merkle.dot', status='replace')
        write(io, '(A)') 'graph{'
        call self%dot_rec(self%tophash, io)
        write(io, '(A)') '}'
        close(io)

        call execute_command_line('dot -Tpng merkle.dot > merkle.png')
    end subroutine dot

    subroutine dot_rec(self, temp, io)
        class(merkle), intent(in) :: self
        class(nodo_hash), intent(in), pointer :: temp
        integer, intent(in) :: io

        if(.not. associated(temp)) return
        write (io, '(A,I5,A,A,A)') ' ', temp%uid, ' [label="', temp%hash, '"];'

        if (associated(temp%izquierda)) then
            write (io, '(A,I5,A,I5,A)') ' ', temp%uid, ' -- ', temp%izquierda%uid, ';'
        end if

        if (associated(temp%derecha)) then
            write (io, '(A,I5,A,I5,A)') ' ', temp%uid, ' -- ', temp%derecha%uid, ';'
        end if
    
        call self%dot_rec(temp%izquierda, io)
        call self%dot_rec(temp%derecha, io)

        if (associated(temp%data)) then
            write (io, '(A,I5,A,A,A)') ' ', temp%data%uid, ' [label="', temp%data%valor, '" shape=rect];'
            write (io, '(A,I5,A,I5,A)') ' ', temp%uid, ' -- ', temp%data%uid, ';'
        end if
    end subroutine dot_rec

end module merkle_m