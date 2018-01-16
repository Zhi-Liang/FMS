subroutine mpp_get_fields(unit, &
                          variables)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),dimension(:),intent(inout) :: variables

    call openmp_thread_trap()
    call mpp_get_fields_(context, &
                         unit, &
                         variables)

end subroutine mpp_get_fields

subroutine mpp_get_fields_(context, &
                           file_index, &
                           fields)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),dimension(:),intent(inout) :: fields

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: tot_num_vars
    integer(kind=c_int) :: input_array_size
    integer(kind=c_int) :: var_counter
    integer(kind=c_int) :: var_index
    type(c_ptr) :: ptr
    integer(kind=c_int) :: i
    integer(kind=c_int) :: dim_index

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Set default values.
    fields = default_field

    if (allocated(context%files(file_index)%vars)) then

        !Find the number of variables that are not associated with any
        !dimensions.
        tot_num_vars = size(context%files(file_index)%vars)
        input_array_size = size(fields)
        var_counter = 0
        ptr = c_null_ptr
        do i = 1,tot_num_vars
            var_index = context%files(file_index)%vars(i)%var_index
            ptr = mpp_io_c_get_netcdf_variable_name(f, &
                                                    var_index)
            dim_index = mpp_io_c_get_netcdf_dimension_index(f, &
                                                            ptr)
            ptr = c_null_ptr
            if (dim_index .eq. MPP_IO_C_INDEX_NOT_FOUND) then
                var_counter = var_counter + 1
                if (var_counter .gt. input_array_size) then
                    call mpp_error(FATAL, &
                                   "mpp_get_fields: input fields array" &
                                       //" is too small.")
                endif
                call fill_fieldtype_from_index(context, &
                                               file_index, &
                                               var_index, &
                                               fields(var_counter))
            endif
        enddo
    endif

end subroutine mpp_get_fields_
