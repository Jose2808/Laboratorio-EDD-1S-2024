program flow_control
    implicit none
    integer:: i
    integer:: j

    if(.true.) then
        !código
    else if(.false.) then
        !código
    else
        !código
    end if

    do i = 1, 10, 2
        print *, i
    end do

    i = 0
    do while (i < 11)
        print *, i
        i = i + 1
    end do

    afuera: do i = 1, 10
        dentro: do j = 1, 10
            if((j + i) > 10) then
                cycle afuera
            end if
            print *, 'I=', I, ' j=', j, ' Sum=', j + i
        end do dentro 
    end do afuera 
    
end program flow_control

