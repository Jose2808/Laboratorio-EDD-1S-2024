program main
    use lista_adyacencia_m
    implicit none
    
    type(ListaAdyacencia) :: grafo
    call grafo%insert(36)
    call grafo%insert(25)
    call grafo%insert(10)
    call grafo%insert(100)
    call grafo%insert(150)
    call grafo%insert(1)
    call grafo%insert(6)

    call grafo%crearConexion(36, 25)
    call grafo%crearConexion(25, 36)

    call grafo%crearConexion(25, 10)
    call grafo%crearConexion(10, 25)

    call grafo%crearConexion(10, 100)
    call grafo%crearConexion(100, 10)

    call grafo%crearConexion(10, 150)
    call grafo%crearConexion(150, 10)

    call grafo%crearConexion(25, 100)
    call grafo%crearConexion(100, 25)

    call grafo%crearConexion(150, 36)
    call grafo%crearConexion(36, 150)

    call grafo%crearConexion(6, 1)
    call grafo%crearConexion(1, 6)

    call grafo%crearConexion(1, 36)
    call grafo%crearConexion(36, 1)

    call grafo%crearGrafo()
end program main