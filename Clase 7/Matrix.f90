module matrix_m
    implicit none
    private
    type :: node_val
        private
        logical :: exists = .false.
        logical :: valor
    end type node_val

    type :: node
        private
        integer :: i,j
        logical :: valor = .false.
        type(node), pointer :: arriba => null()
        type(node), pointer :: abajo => null()
        type(node), pointer :: derecha => null()
        type(node), pointer :: izquierda => null()
    end type node

    type, public :: matrix
        private
        type(node), pointer :: root => null()
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: insert
        procedure :: insertarCabeceraFila
        procedure :: insertarCabeceraColumna
        procedure :: buscarFila
        procedure :: buscarColumna
        procedure :: existeNodo
        procedure :: print
        procedure :: imprimirEncabezadoColumnas
        procedure :: obtenerValor
    end type matrix

contains
    subroutine insert(self, i, j, valor)
        class(matrix), intent(inout) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        logical, intent(in) :: valor

        type(node), pointer :: nuevo
        type(node), pointer :: fila
        type(node), pointer :: columna

        allocate(nuevo)
        nuevo = node(i=i, j=j, valor=valor)

        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = node(i=-1, j=-1)
        end if

        fila => self%buscarFila(j)
        columna => self%buscarColumna(i)

        if(i > self%width) self%width = i
        if(j > self%height) self%height = j

        if(.not. self%existeNodo(nuevo)) then
            if(.not. associated(columna)) then
                columna => self%insertarCabeceraColumna(i)
            end if

            if(.not. associated(fila)) then
                fila => self%insertarCabeceraFila(j)
            end if
            call insertarEnColumna(nuevo, fila)
            call insertarEnFila(nuevo, columna)
        end if
    end subroutine insert

    function insertarCabeceraColumna(self, i) result(nuevaCabeceraColumna)
        class(matrix), intent(inout) :: self
        integer, intent(in) :: i

        type(node), pointer :: nuevaCabeceraColumna
        allocate(nuevaCabeceraColumna)

        nuevaCabeceraColumna = node(i=i, j=-1)
        call insertarEnColumna(nuevaCabeceraColumna, self%root)
    end function insertarCabeceraColumna
    
    function insertarCabeceraFila(self, j) result(nuevaCabeceraFila)
        class(matrix), intent(inout) :: self
        integer, intent(in) :: j

        type(node), pointer :: nuevaCabeceraFila
        allocate(nuevaCabeceraFila)

        nuevaCabeceraFila = node(i=-1, j=j)
        call insertarEnFila(nuevaCabeceraFila, self%root)
    end function insertarCabeceraFila

    subroutine insertarEnFila(nuevo, CabeceraFila)
        type(node), pointer :: nuevo
        type(node), pointer :: cabeceraFila !head 

        type(node), pointer :: actual
        actual => cabeceraFila

        do while(associated(actual%abajo))
            if(nuevo%j < actual%abajo%j) then

                nuevo%abajo => actual%abajo
                nuevo%arriba => actual
                actual%abajo%arriba => nuevo
                actual%abajo => nuevo
                exit
            end if
            actual => actual%abajo
        end do

        if(.not. associated(actual%abajo)) then
            actual%abajo => nuevo
            nuevo%arriba => actual
        end if
    end subroutine insertarEnFila

    subroutine insertarEnColumna(nuevo, CabeceraColumna)
        type(node), pointer :: nuevo
        type(node), pointer :: CabeceraColumna !head 

        type(node), pointer :: actual
        actual => CabeceraColumna

        do while(associated(actual%derecha))
            if(nuevo%i < actual%derecha%i) then

                nuevo%derecha => actual%derecha
                nuevo%izquierda => actual
                actual%derecha%izquierda => nuevo
                actual%derecha => nuevo
                exit
            end if
            actual => actual%derecha
        end do

        if(.not. associated(actual%derecha)) then
            actual%derecha => nuevo
            nuevo%izquierda => actual
        end if
    end subroutine insertarEnColumna  

    function buscarFila(self, j) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: j

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual)) 
            if(actual%j == j) return
            actual => actual%abajo
        end do
    end function buscarFila

    function buscarColumna(self, i) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i

        type(node), pointer :: actual
        actual => self%root 
        
        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%derecha
        end do
    end function buscarColumna

    function existeNodo(self, nodo) result(existe)
        class(matrix), intent(inout) :: self
        type(node), pointer, intent(in) :: nodo

        logical :: existe
        type(node), pointer :: encabezadoFila
        type(node), pointer :: columna
        encabezadoFila => self%root
        existe = .false.

        do while(associated(encabezadoFila))
            if(encabezadoFila%j == nodo%j) then
                columna => encabezadoFila
                do while(associated(columna)) 
                    if(columna%i == nodo%i) then
                        columna%valor = nodo%valor
                        existe = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            encabezadoFila => encabezadoFila%abajo
        end do
        return
    end function existeNodo

    subroutine print(self)
        class(matrix), intent(in) :: self
        integer :: i
        integer :: j
        type(node), pointer :: aux
        type(node_val) :: val
        aux => self%root%abajo

        call self%imprimirEncabezadoColumnas()

        do j = 0, self%height
            print *, ""
            write(*, fmt='(I3)', advance='no') j

            do i = 0, self%width
                val = self%obtenerValor(i,j)
                if(.not. val%exists) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%valor
                end if

            end do
        end do
    end subroutine print

    subroutine imprimirEncabezadoColumnas(self)
        class(matrix), intent(in) :: self
        integer :: i

        do i=-1, self%width
            write(*, fmt='(I3)', advance='no') i
        end do
    end subroutine imprimirEncabezadoColumnas


    function obtenerValor(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j

        type(node), pointer :: cabeceraFila
        type(node), pointer :: columna
        type(node_val) :: val
        cabeceraFila => self%root

        do while(associated(cabeceraFila))
            if(cabeceraFila%j == j) then
                columna => cabeceraFila
                do while(associated(columna)) 
                    if(columna%i == i) then
                        val%valor = columna%valor
                        val%exists = .true.
                        return
                    end if
                    columna => columna%derecha
                end do
                return
            end if
            cabeceraFila => cabeceraFila%abajo
        end do
        return
    end function obtenerValor
end module matrix_m