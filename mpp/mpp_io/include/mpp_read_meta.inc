!Read in all metadata from a netcdf file.
subroutine mpp_read_meta(unit, &
                         read_time)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    logical,intent(in),optional :: read_time

    call openmp_thread_trap()
    call mpp_read_meta_(context, &
                        unit)

end subroutine mpp_read_meta

subroutine mpp_read_meta_(context, &
                          file_index)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: num_global_atts
    integer(kind=c_int) :: num_dims
    integer(kind=c_int) :: num_vars
    integer(kind=c_int) :: i
    integer(kind=c_int) :: var_index
    integer(kind=c_int) :: num_var_atts
    integer(kind=c_int) :: j

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_reader) then

        !Get the total number of global attributes, dimensions, and
        !variables in the netcdf file.
        call mpp_io_c_nc_inq(f, &
                             num_global_atts, &
                             num_dims, &
                             num_vars)

        !Read in the global attributes from the file.
        if (num_global_atts .gt. 0) then
            allocate(context%files(file_index)%global_att_indices(num_global_atts))
            do i = 1,num_global_atts
                context%files(file_index)%global_att_indices(i) =  &
                    mpp_io_c_read_netcdf_attribute(f, &
                                                   MPP_IO_C_GLOBAL_ATT, &
                                                   i-1)
            enddo
        endif

        !Read in the dimensions from the file.
        if (num_dims .gt. 0) then
            allocate(context%files(file_index)%dim_indices(num_dims))
            do i = 1,num_dims
                context%files(file_index)%dim_indices(i) = &
                    mpp_io_c_read_netcdf_dimension(f, &
                                                   i-1)
            enddo
        endif

        !Read in the variables from the file.
        if (num_vars .gt. 0) then
            allocate(context%files(file_index)%vars(num_vars))
            do i = 1,num_vars
                var_index = mpp_io_c_read_netcdf_variable_metadata(f, &
                                                                   i-1)
                context%files(file_index)%vars(i)%var_index = var_index

                !Read in the attributes for each variable.
                num_var_atts = mpp_io_c_get_netcdf_variable_max_num_attributes(f, &
                                                                               var_index)
                if (num_var_atts .gt. 0) then
                    allocate(context%files(file_index)%vars(i)%att_indices(num_var_atts))
                    do j = 1,num_var_atts
                        context%files(file_index)%vars(i)%att_indices(j) = &
                            mpp_io_c_read_netcdf_attribute(f, &
                                                           var_index, &
                                                           j-1)
                    enddo
                endif
            enddo
        endif
    endif

end subroutine mpp_read_meta_
