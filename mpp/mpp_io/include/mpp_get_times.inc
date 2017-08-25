subroutine mpp_get_times(unit, &
                         time_values)

    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    real(kind=c_double),dimension(:),intent(inout) :: time_values

    call openmp_thread_trap()
    call mpp_get_times_(context, &
                        unit, &
                        time_values)

end subroutine mpp_get_times

subroutine mpp_get_times_(context, &
                          file_index, &
                          unlim_dim_vals)

    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    real(kind=c_double),dimension(:),intent(inout) :: unlim_dim_vals

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: num_dims
    logical(kind=c_bool) :: found_unlim_dim
    integer(kind=c_int) :: i
    integer(kind=c_int) :: dim_index
    integer(kind=c_int) :: unlim_dim_length
    type(c_ptr) :: ptr
    integer(kind=c_int) :: var_index
    integer(kind=c_int) :: var_type
    real(kind=c_float),dimension(:),allocatable,target :: float_buf
    integer(kind=c_int) :: j
    real(kind=c_double),dimension(:),allocatable,target :: double_buf

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Set initial values.
    unlim_dim_vals = 0.0

    if (allocated(context%files(file_index)%dim_indices)) then

        !Get the number of dimensions in the file.
        num_dims = size(context%files(file_index)%dim_indices)

        !Look for an unlimited dimension.
        found_unlim_dim = .false._c_bool
        do i = 1,num_dims
            dim_index = context%files(file_index)%dim_indices(i)

            if (mpp_io_c_get_netcdf_dimension_is_unlimited(f,dim_index)) then

                !Unlimited dimension does exist in the file.
                found_unlim_dim = .true._c_bool

                !Get the current unlimited dimension length.
                unlim_dim_length = int(mpp_io_c_get_netcdf_dimension_length(f, &
                                                                            dim_index),kind=c_int)

                !Get a pointer to the name of the unlimited dimension.
                ptr = c_null_ptr
                ptr = mpp_io_c_get_netcdf_dimension_name(f, &
                                                         dim_index)

                !Make sure that the inputted array is the correct size.
                if (size(unlim_dim_vals) .ne. unlim_dim_length) then
                    call mpp_error(FATAL, &
                                   "mpp_get_times_: the inputted array is not" &
                                       //" the correct size.")
                endif

                !Get the index of the variable that corresponds to the unlimited
                !dimension.
                var_index = mpp_io_c_get_netcdf_variable_index(f, &
                                                               ptr)
                ptr = c_null_ptr

                !Get the type for the unlimited dimension.
                var_type = mpp_io_c_get_netcdf_variable_type_in_file(f, &
                                                                     var_index)

                !Read in the unlimited dimension values.
                if (unlim_dim_length .gt. 0) then

                    select case (var_type)
                        case (MPP_IO_C_NC_FLOAT)

                            allocate(float_buf(unlim_dim_length))
                            ptr = c_loc(float_buf)
                            call mpp_io_c_read_netcdf_variable_data(f, &
                                                                    var_index, &
                                                                    (/int(0,kind=c_size_t)/), &
                                                                    (/int(unlim_dim_length,kind=c_size_t)/), &
                                                                    ptr, &
                                                                    var_type)
                            ptr = c_null_ptr
                            do j = 1,unlim_dim_length
                                unlim_dim_vals(j) = real(float_buf(j),kind=c_double)
                            enddo
                            deallocate(float_buf)

                        case (MPP_IO_C_NC_DOUBLE)

                            allocate(double_buf(unlim_dim_length))
                            ptr = c_loc(double_buf)
                            call mpp_io_c_read_netcdf_variable_data(f, &
                                                                    var_index, &
                                                                    (/int(0,kind=c_size_t)/), &
                                                                    (/int(unlim_dim_length,kind=c_size_t)/), &
                                                                    ptr, &
                                                                    var_type)
                            ptr = c_null_ptr
                            do j = 1,unlim_dim_length
                                unlim_dim_vals(j) = double_buf(j)
                            enddo
                            deallocate(double_buf)

                        case default
                            call mpp_error(FATAL, &
                                           "mpp_get_times_: unlimited dimension" &
                                               //" must be either a float or a" &
                                               //" double.")
                    end select
                endif

                exit
            endif
        enddo
    endif

end subroutine mpp_get_times_
