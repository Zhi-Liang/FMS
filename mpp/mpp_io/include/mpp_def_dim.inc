subroutine mpp_def_dim_nodata(unit, &
                              name, &
                              size)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(in) :: name
    integer(kind=c_int),intent(in) :: size

    call openmp_thread_trap()
    call mpp_def_dim_nodata_(context, &
                             unit, &
                             name, &
                             size)

end subroutine mpp_def_dim_nodata

subroutine mpp_def_dim_int(unit, &
                           name, &
                           dsize, &
                           longname, &
                           data)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(in) :: name
    integer(kind=c_int),intent(in) :: dsize
    character(len=*),intent(in) :: longname
    integer(kind=c_int),dimension(:),intent(in) :: data

    call openmp_thread_trap()
    call mpp_def_dim_int_(context, &
                          unit, &
                          name, &
                          dsize, &
                          longname, &
                          data, &
                          size(data,kind=c_size_t))

end subroutine mpp_def_dim_int

subroutine mpp_def_dim_real(unit, &
                            name, &
                            dsize, &
                            longname, &
                            data)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(in) :: name
    integer(kind=c_int),intent(in) :: dsize
    character(len=*),intent(in) :: longname
    real(kind=c_double),dimension(:),intent(in) :: data

    call openmp_thread_trap()
    call mpp_def_dim_real_(context, &
                           unit, &
                           name, &
                           dsize, &
                           longname, &
                           data, &
                           size(data,kind=c_size_t))

end subroutine mpp_def_dim_real

subroutine mpp_def_dim_nodata_(context, &
                               file_index, &
                               dim_name, &
                               dim_len)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    character(len=*),intent(in) :: dim_name
    integer(kind=c_int),intent(in) :: dim_len

    !Local variables
    integer(kind=c_int) :: f
    character(kind=c_char,len=1),dimension(:),allocatable :: name_buf
    integer(kind=c_int) :: dimension_index

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the input file is a netcdf file.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (.not. context%files(file_index)%is_writer) then
        return
    endif

    !Place the name in an interoperable buffer.
    call f_string_to_c(dim_name, &
                       name_buf)

    !Write the dimension to the netcdf file.  Do not buffer the values.
    dimension_index = mpp_io_c_write_netcdf_dimension(f, &
                                                      name_buf, &
                                                      int(dim_len,kind=c_size_t), &
                                                      .false._c_bool)
    deallocate(name_buf)

end subroutine mpp_def_dim_nodata_

subroutine mpp_def_dim_int_(context, &
                            file_index, &
                            dim_name, &
                            dim_len, &
                            longname, &
                            dim_data, &
                            data_size)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    character(len=*),intent(in) :: dim_name
    integer(kind=c_int),intent(in) :: dim_len
    character(len=*),intent(in) :: longname
    integer(kind=c_int),dimension(*),target,intent(in) :: dim_data
    integer(kind=c_size_t),intent(in) :: data_size

    !Local variables
    integer(kind=c_int) :: f
    character(kind=c_char,len=1),dimension(:),allocatable :: name_buf
    integer(kind=c_int),dimension(1) :: dimension_index
    integer(kind=c_int) :: variable_index

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the input file is a netcdf file.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (.not. context%files(file_index)%is_writer) then
        return
    endif

    !Place the name in an interoperable buffer.
    call f_string_to_c(dim_name, &
                       name_buf)

    !Write the dimension to the netcdf file.  Buffer the values.
    dimension_index(1) = mpp_io_c_write_netcdf_dimension(f, &
                                                         name_buf, &
                                                         int(dim_len,kind=c_size_t), &
                                                         .true._c_bool)

    !Write variable metadata to the netcdf file.  Buffer the values.
    variable_index = mpp_io_c_write_netcdf_variable_metadata(f, &
                                                             MPP_IO_C_NC_INT, &
                                                             name_buf, &
                                                             dimension_index, &
                                                             1, &
                                                             MPP_IO_C_MAX_ATTS_PER_VAR, &
                                                             .true._c_bool)
    deallocate(name_buf)

    !Add the longname attribute to the variable.
    call mpp_write_attribute_metadata(context, &
                                      file_index, &
                                      variable_index, &
                                      "long_name", &
                                      len_trim(longname,kind=c_size_t), &
                                      cval=longname)

    !Write out the data.
    call mpp_io_c_write_netcdf_variable_data(f, &
                                             variable_index, &
                                             (/int(0,kind=c_size_t)/), &
                                             (/data_size/), &
                                             c_loc(dim_data), &
                                             MPP_IO_C_NC_INT)

    !Free the buffered variable metadata.
    call mpp_io_c_free_netcdf_variable(f, &
                                       variable_index)

    !Free the buffered dimension values.
    call mpp_io_c_free_netcdf_dimension(f, &
                                        dimension_index(1))

end subroutine mpp_def_dim_int_

subroutine mpp_def_dim_real_(context, &
                             file_index, &
                             dim_name, &
                             dim_len, &
                             longname, &
                             dim_data, &
                             data_size)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    character(len=*),intent(in) :: dim_name
    integer(kind=c_int),intent(in) :: dim_len
    character(len=*),intent(in) :: longname
    real(kind=c_double),dimension(*),target,intent(in) :: dim_data
    integer(kind=c_size_t),intent(in) :: data_size

    !Local variables
    integer(kind=c_int) :: f
    character(kind=c_char,len=1),dimension(:),allocatable :: name_buf
    integer(kind=c_int),dimension(1) :: dimension_index
    integer(kind=c_int) :: variable_index

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the input file is a netcdf file.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_writer) then
        return
    endif

    !Place the name in an interoperable buffer.
    call f_string_to_c(dim_name, &
                       name_buf)

    !Write the dimension to the netcdf file.  Buffer the values.
    dimension_index(1) = mpp_io_c_write_netcdf_dimension(f, &
                                                         name_buf, &
                                                         int(dim_len,kind=c_size_t), &
                                                         .true._c_bool)

    !Write variable metadata to the netcdf file.  Buffer the values.
    variable_index = mpp_io_c_write_netcdf_variable_metadata(f, &
                                                             MPP_IO_C_NC_DOUBLE, &
                                                             name_buf, &
                                                             dimension_index, &
                                                             1, &
                                                             MPP_IO_C_MAX_ATTS_PER_VAR, &
                                                             .true._c_bool)
    deallocate(name_buf)

    !Add the longname attribute to the variable.
    call mpp_write_attribute_metadata(context, &
                                      file_index, &
                                      variable_index, &
                                      "long_name", &
                                      len_trim(longname,kind=c_size_t), &
                                      cval=longname)

    !Write out the data.
    call mpp_io_c_write_netcdf_variable_data(f, &
                                             variable_index, &
                                             (/int(0,kind=c_size_t)/), &
                                             (/data_size/), &
                                             c_loc(dim_data), &
                                             MPP_IO_C_NC_DOUBLE)

    !Free the buffered variable metadata.
    call mpp_io_c_free_netcdf_variable(f, &
                                       variable_index)

    !Free the buffered dimension values.
    call mpp_io_c_free_netcdf_dimension(f, &
                                        dimension_index(1))

end subroutine mpp_def_dim_real_
