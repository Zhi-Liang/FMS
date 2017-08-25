subroutine destroy_atttype(att)
    implicit none

    !Inputs/outputs
    type(atttype),intent(inout) :: att

    if (allocated(att%fatt)) then
        deallocate(att%fatt)
    endif

end subroutine destroy_atttype

subroutine destroy_axistype(axis)
    implicit none

    !Inputs/outputs
    type(axistype),intent(inout) :: axis

    !Local variables
    integer(kind=c_int) :: i

    if (associated(axis%domain)) then
        axis%domain => null()
    endif

    if (allocated(axis%data)) then
        deallocate(axis%data)
    endif

    if (allocated(axis%data_bounds)) then
        deallocate(axis%data_bounds)
    endif

    if (allocated(axis%idata)) then
        deallocate(axis%idata)
    endif

    if (allocated(axis%Att)) then
        do i = 1,size(axis%Att)
            call destroy_atttype(axis%Att(i))
        enddo
        deallocate(axis%Att)
    endif

end subroutine destroy_axistype

subroutine destroy_fieldtype(var)
    implicit none

    !Inputs/outputs
    type(fieldtype),intent(inout) :: var

    !Local variables
    integer(kind=c_int) :: i

    if (allocated(var%axes)) then
        do i = 1,size(var%axes)
            call destroy_axistype(var%axes(i))
        enddo
        deallocate(var%axes)
    endif

    if (allocated(var%size)) then
        deallocate(var%size)
    endif

    if (allocated(var%Att)) then
        do i = 1,size(var%Att)
            call destroy_atttype(var%Att(i))
        enddo
        deallocate(var%Att)
    endif

end subroutine destroy_fieldtype
