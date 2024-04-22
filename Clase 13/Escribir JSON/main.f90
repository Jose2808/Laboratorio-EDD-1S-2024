program main
    use json_module
    use bitsy
    implicit none
    
    type(json_core) :: json
    type(json_value), pointer :: lista_bloques, p, data, camino

    !Inicializando archivo json
    call json%initialize()

    call json%create_array(lista_bloques, '')

    !Creando puntero a cuerpo de bloque
    call json%create_object(p, '')

    !Creando atributo DATA de un tipo arreglo
    call json%create_array(data, 'DATA')

    !Creando primer camino
    call json%create_object(camino, '')
    call json%add(camino, 'sucursal_o', 1)
    call json%add(camino, 'direccion_o', "5ta av 4 calle zona 18, Guatemala, Guatemala")
    call json%add(camino, 'sucursal_d', 2)
    call json%add(camino, 'direccion_d', "6ta av 12 calle zona 1, Guatemala, Guatemala")
    call json%add(camino, 'total', -60)

    !Agregando camino a data
    call json%add(data, camino)

    !Creando primer camino
    call json%create_object(camino, '')
    call json%add(camino, 'sucursal_o', 2)
    call json%add(camino, 'direccion_o', "6ta av 12 calle zona 1, Guatemala, Guatemala")
    call json%add(camino, 'sucursal_d', 3)
    call json%add(camino, 'direccion_d', "9na av 5 calle zona 6, Guatemala, Guatemala")
    call json%add(camino, 'total', 400)

    !Agregando camino a data
    call json%add(data, camino)

    !Agregando atributo data a bloque
    call json%add(p, 'INDEX', 0)
    call json%add(p, 'NONCE', 4560)
    call json%add(p, data)
    call json%add(p, 'ROOTMERKLE', sha256('Aqui va la raiz de merkle'))
    call json%add(p, 'PREVIOUSHASH', "0000")
    call json%add(p, 'HASH', sha256("primer bloque"))

    call json%add(lista_bloques, p)
    call json%add(lista_bloques, p)

    nullify(camino)
    nullify(data)
    nullify(p)

    ! call json%create_object(p, '')

    ! call json%create_object(inp, 'inputs')
    ! call json%add(p, inp)

    ! call json%add(inp, 't0', 0.1)
    ! call json%add(inp, 'tf', 1.1)
    ! call json%add(inp, 'x0', 9999)
    ! call json%add(inp, 'integer_scalar', 787)
    ! call json%add(inp, 'integer_array', [2,4,99])
    ! call json%add(inp, 'names', ['aaa', 'bbb', 'ccc'])
    ! call json%add(inp, 'logical_scalar', .true.)
    ! call json%add(inp, 'logical_vector', [.true., .false., .true.])

    ! nullify(inp)
    call json%print(lista_bloques, './clase13_blockchain.json')
    call json%destroy(lista_bloques)
end program main

