!Write out axis data.
subroutine mpp_write_axis(unit, &
                          axis)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(axistype),intent(in) :: axis

    call openmp_thread_trap()
    call mpp_write_axis_(context, &
                         unit, &
                         axis)

end subroutine mpp_write_axis

subroutine mpp_write_axis_(context, &
                           file_index, &
                           axis)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(axistype),intent(in) :: axis

    !Local variables
    integer(kind=c_int) :: f

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Write out buffered axis data.  If the data was not previously buffered,
    !this error should be caught in the mpp_io_c code.
    if (context%files(file_index)%is_writer) then
        call mpp_io_c_write_buffered_netcdf_variable_data(f, &
                                                          int(axis%id,kind=c_int), &
                                                          .false._c_bool)
    endif

end subroutine mpp_write_axis_
