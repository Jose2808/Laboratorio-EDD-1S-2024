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
    integer :: id

    call json%initialize()
    call json%load(filename='data.json')
    call json%info('', n_children=size)

    call json%get_core(jsonc)
    call json%get('', listPointer, found)

    do i = 1, size
        call jsonc%get_child(listPointer, i, animalPointer, found)
        call jsonc%get_child(animalPointer, 'nombre', attributePointer, found)
        call jsonc%get(attributePointer, nombre)
        print *, nombre
        call jsonc%get_child(animalPointer, 'id', attributePointer, found)
        call jsonc%get(attributePointer, id)
        print *, id
    end do

    call json%destroy()

end program main