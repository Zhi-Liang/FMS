subroutine mpp_get_axes(unit, &
                        axes, &
                        time_axis)

    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(axistype),dimension(:),intent(inout) :: axes
    type(axistype),intent(inout),optional :: time_axis

    call openmp_thread_trap()
    call mpp_get_axes_(context, &
                       unit, &
                       axes, &
                       time_axis)

end subroutine mpp_get_axes

subroutine mpp_get_axes_(context, &
                         file_index, &
                         axes, &
                         unlim_axis)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(axistype),dimension(:),intent(inout) :: axes
    type(axistype),intent(inout),optional :: unlim_axis

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: num_dims
    logical(kind=c_bool) :: found_unlim_axis
    integer(kind=c_int) :: i
    integer(kind=c_int) :: dim_index

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the input file is a netcdf file.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Set default values.
    axes = default_axis
    if (present(unlim_axis)) then
        unlim_axis = default_axis
    endif

    if (allocated(context%files(file_index)%dim_indices)) then

        !Make sure that the input array is the correct size.
        num_dims = size(context%files(file_index)%dim_indices)
        if (size(axes) .lt. num_dims) then
            call mpp_error(FATAL, &
                           "mpp_get_axes_: the size of the input" &
                               //" axes array must be at least as large" &
                               //" as the number of dimensions in the" &
                               //" netcdf file.")
        endif

        !Loop through the dimensions and fill in the axes array.
        found_unlim_axis = .false._c_bool
        do i = 1,num_dims
            dim_index = context%files(file_index)%dim_indices(i)
            call fill_axistype_from_index(context, &
                                          file_index, &
                                          dim_index, &
                                          axes(i))

            if (present(unlim_axis) .and. .not. found_unlim_axis) then
                if (mpp_io_c_get_netcdf_dimension_is_unlimited(f, &
                                                               dim_index)) then
                    unlim_axis = axes(i)
                    found_unlim_axis = .true._c_bool
                endif
            endif
        enddo
    endif

end subroutine mpp_get_axes_
