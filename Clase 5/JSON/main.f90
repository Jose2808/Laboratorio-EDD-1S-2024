program main
    use json_module
    implicit none

    type(json_file) :: json
    type(json_core) :: jsonc
    type(json_value), pointer :: listPointer, animalPointer, attributePointer
    logical :: found
    integer :: size
    integer :: i
    character(:), allocatable :: nombre
    character(:), allocatable :: id_str
    integer :: id
    integer :: id_int

    call json%initialize()
    call json%load(filename='data.json')
    call json%info('', n_children=size)

    call json%get_core(jsonc)
    call json%get('', listPointer, found)

    do i = 1, size
        call jsonc%get_child(listPointer, i, animalPointer, found)

        !imprimiendo el atributo nombre
        call jsonc%get_child(animalPointer, 'nombre', attributePointer, found)
        call jsonc%get(attributePointer, nombre)
        print *, nombre

        !imprimiendo el atributo id
        call jsonc%get_child(animalPointer, 'id', attributePointer, found)
        call jsonc%get(attributePointer, id)
        print *, id

        !imprimiendo el atributo id_str
        call jsonc%get_child(animalPointer, 'id_str', attributePointer, found) 
        call jsonc%get(attributePointer, id_str)
        print *, "Imprimiendo el id en str: ", id_str   
        
        !parseando str a int
        read(id_str, *) id_int
        print *, "Imprimiendo el id_str en formato int: ", id_int
        
    end do

    call json%destroy()

end program main