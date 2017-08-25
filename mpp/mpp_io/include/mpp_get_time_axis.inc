subroutine mpp_get_time_axis(unit, &
                             time_axis)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(axistype),intent(inout) :: time_axis

    call openmp_thread_trap()
    call mpp_get_time_axis_(context, &
                            unit, &
                            time_axis)

end subroutine mpp_get_time_axis

subroutine mpp_get_time_axis_(context, &
                              file_index, &
                              unlim_dim)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(axistype),intent(inout) :: unlim_dim

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: num_dims
    integer(kind=c_int) :: i
    integer(kind=c_int) :: dim_index

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (allocated(context%files(file_index)%dim_indices)) then

        !Get the number of dimensions in the file.
        num_dims = size(context%files(file_index)%dim_indices)

        !Look for an unlimited dimension.
        do i = 1,num_dims
            dim_index = context%files(file_index)%dim_indices(i)

            if (mpp_io_c_get_netcdf_dimension_is_unlimited(f,dim_index)) then

                !Get the dimension values.
                call fill_axistype_from_index(context, &
                                              file_index, &
                                              dim_index, &
                                              unlim_dim)
                exit
            endif
        enddo
    else
        call mpp_error(FATAL, &
                       "mpp_get_time_axis_: the file associated with the" &
                           //" inputted index does not contain any" &
                           //" dimensions.")
    endif

end subroutine mpp_get_time_axis_
