subroutine mpp_get_global_atts(unit, &
                               global_atts)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(atttype),dimension(:),intent(inout) :: global_atts

    call openmp_thread_trap()
    call mpp_get_global_atts_(context, &
                              unit, &
                              global_atts)

end subroutine mpp_get_global_atts

subroutine mpp_get_global_atts_(context, &
                                file_index, &
                                global_atts)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(atttype),dimension(:),intent(inout) :: global_atts

    !Local variables
    integer(kind=c_int) :: num_global_atts
    integer(kind=c_int) :: i

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    call check_file_index(context, &
                          file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Set default values.
    global_atts = default_att

    if (allocated(context%files(file_index)%global_att_indices)) then

        !Make sure that the inputted array is the correct size.
        num_global_atts = size(context%files(file_index)%global_att_indices)
        if (size(global_atts) .lt. num_global_atts) then
            call mpp_error(FATAL, &
                           "mpp_get_global_atts_: the size of the inputted" &
                               //" attributes array must be at least as large" &
                               //" as the number of global attributes in the" &
                               //" netcdf file.")
        endif

        !Loop through the attributes and fill in the global atts array.
        do i = 1,num_global_atts
            call fill_atttype_from_index(context, &
                                         file_index, &
                                         MPP_IO_C_GLOBAL_ATT, &
                                         context%files(file_index)%global_att_indices(i), &
                                         global_atts(i))
        enddo
    endif

end subroutine mpp_get_global_atts_
