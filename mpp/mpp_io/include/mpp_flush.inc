subroutine mpp_flush(unit)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit

    call openmp_thread_trap()
    call mpp_flush_(context, &
                    unit)

end subroutine mpp_flush

subroutine mpp_flush_(context, &
                      file_index)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index

    !Local variables
    integer(kind=c_int) :: f

    !Make sure that the module has been initialized and that a valid
    !file index was passed in.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)

    if (context%files(file_index)%is_writer) then
        if (context%files(file_index)%file_type .eq. MPP_NETCDF) then
            call mpp_io_c_flush_netcdf_file(f)
        else
            call mpp_io_c_flush_regular_file(f)
        endif
    endif

end subroutine mpp_flush_
