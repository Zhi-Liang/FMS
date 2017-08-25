!Write out data.
subroutine mpp_write_r0d(unit, &
                         field, &
                         data, &
                         tstamp)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),intent(in) :: data
    real(kind=c_double),intent(in),optional :: tstamp

    !Local variables
    integer(kind=c_size_t),dimension(1) :: data_sizes

    call openmp_thread_trap()
    data_sizes(1) = 1
    call mpp_write_r_array(context, &
                           unit, &
                           field, &
                           (/data/), &
                           data_sizes, &
                           .true._c_bool, &
                           tstamp)

end subroutine mpp_write_r0d

subroutine mpp_write_r1d(unit, &
                         field, &
                         data, &
                         tstamp)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(:),intent(in) :: data
    real(kind=c_double),intent(in),optional :: tstamp

    !Local variables
    integer(kind=c_size_t),dimension(1) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_write_r_array(context, &
                           unit, &
                           field, &
                           data, &
                           data_sizes, &
                           .false._c_bool, &
                           tstamp)

end subroutine mpp_write_r1d

subroutine mpp_write_r2d(unit, &
                         field, &
                         data, &
                         tstamp)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(:,:),intent(in) :: data
    real(kind=c_double),intent(in),optional :: tstamp

    !Local variables
    integer(kind=c_size_t),dimension(2) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_write_r_array(context, &
                           unit, &
                           field, &
                           data, &
                           data_sizes, &
                           .false._c_bool, &
                           tstamp)

end subroutine mpp_write_r2d

subroutine mpp_write_r3d(unit, &
                         field, &
                         data, &
                         tstamp)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(:,:,:),intent(in) :: data
    real(kind=c_double),intent(in),optional :: tstamp

    !Local variables
    integer(kind=c_size_t),dimension(3) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_write_r_array(context, &
                           unit, &
                           field, &
                           data, &
                           data_sizes, &
                           .false._c_bool, &
                           tstamp)

end subroutine mpp_write_r3d

subroutine mpp_write_r4d(unit, &
                         field, &
                         data, &
                         tstamp)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(:,:,:,:),intent(in) :: data
    real(kind=c_double),intent(in),optional :: tstamp

    !Local variables
    integer(kind=c_size_t),dimension(4) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_write_r_array(context, &
                           unit, &
                           field, &
                           data, &
                           data_sizes, &
                           .false._c_bool, &
                           tstamp)

end subroutine mpp_write_r4d

subroutine mpp_write_r_array(context, &
                             file_index, &
                             field, &
                             var_data, &
                             var_data_sizes, &
                             is_scalar, &
                             unlim_dim_val)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(*),target,intent(in) :: var_data
    integer(kind=c_size_t),dimension(:),intent(in) :: var_data_sizes
    logical(kind=c_bool),intent(in) :: is_scalar
    real(kind=c_double),intent(in),optional :: unlim_dim_val

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: type_in_mem
    integer(kind=c_int) :: unlim_dim_level
    integer(kind=c_int) :: unlim_dim_index
    type(c_ptr) :: data_ptr
    integer(kind=c_size_t),dimension(size(field%axes)) :: corner_indices
    integer(kind=c_size_t),dimension(size(field%axes)) :: edge_lengths
    integer(kind=c_size_t),dimension(size(field%axes)) :: corner_indices_tp
    integer(kind=c_size_t),dimension(size(field%axes)) :: edge_lengths_tp
    logical(kind=c_bool) :: found_unlim
    integer(kind=c_int) :: i
    integer(kind=c_int) :: num_axes

    f = check_file_index(context, &
                         file_index)

    !Get necessary quantities.
    call prepare_write_r8(context, &
                          file_index, &
                          field, &
                          type_in_mem, &
                          unlim_dim_level, &
                          unlim_dim_val)

    !Throw an error if the current rank is not allowed to write to the
    !file.
    if (context%files(file_index)%is_writer) then

        !Make sure that the number of data dimensions is compatible with the
        !inputted netcdf variable.
!       if (size(field%axes) .lt. size(var_data_sizes) .or. size(field%axes) &
!               .gt. size(var_data_sizes) + 1) then
!           call mpp_error(FATAL, &
!                          "mpp_write_r_array: the inputted data does not match" &
!                              //" the shape of the inputted netcdf variable.")
!       endif

        !Point at the data.
        data_ptr = c_null_ptr
        data_ptr = c_loc(var_data)

        !Set corner indices and edge lengths.
        num_axes = size(corner_indices)
        unlim_dim_index = field%time_axis_index
        if (unlim_dim_index .eq. -1) then
            corner_indices = 0
            edge_lengths = var_data_sizes
        else
            if (is_scalar) then
                corner_indices = unlim_dim_level
                edge_lengths = 1
            else
                found_unlim = .false._c_bool
                do i = 1,num_axes
                    if (unlim_dim_index .eq. i) then
                        corner_indices(i) = unlim_dim_level
                        edge_lengths(i) = 1
                        found_unlim = .true._c_bool
                    else
                        corner_indices(i) = 0
                        if (found_unlim) then
                            edge_lengths(i) = var_data_sizes(i-1)
                        else
                            edge_lengths(i) = var_data_sizes(i)
                        endif
                    endif
                enddo
            endif
        endif

        !Since c reads the data in "row-based" order, flip the dimension
        !order.
        do i = 1,num_axes
            corner_indices_tp(num_axes+1-i) = corner_indices(i)
            edge_lengths_tp(num_axes+1-i) = edge_lengths(i)
        enddo

        !Write out the data.
        call mpp_io_c_write_netcdf_variable_data(f, &
                                                 int(field%id,kind=c_int), &
                                                 corner_indices_tp, &
                                                 edge_lengths_tp, &
                                                 data_ptr, &
                                                 type_in_mem)

        !Clean up.
        data_ptr = c_null_ptr
    endif

end subroutine mpp_write_r_array
