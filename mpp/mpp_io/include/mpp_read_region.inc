subroutine mpp_read_region_r2d(unit, &
                               field, &
                               data, &
                               start, &
                               nread)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(:,:),intent(inout) :: data
    integer(kind=c_int),dimension(:),intent(in) :: start
    integer(kind=c_int),dimension(:),intent(in) :: nread

    !Local variables
    integer(kind=c_size_t),dimension(2) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_read_region_array(context, &
                               unit, &
                               field, &
                               data, &
                               data_sizes, &
                               start, &
                               nread)

end subroutine mpp_read_region_r2d

subroutine mpp_read_region_r3d(unit, &
                               field, &
                               data, &
                               start, &
                               nread)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(:,:,:),intent(inout) :: data
    integer(kind=c_int),dimension(:),intent(in) :: start
    integer(kind=c_int),dimension(:),intent(in) :: nread

    !Local variables
    integer(kind=c_size_t),dimension(3) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_read_region_array(context, &
                               unit, &
                               field, &
                               data, &
                               data_sizes, &
                               start, &
                               nread)

end subroutine mpp_read_region_r3d

subroutine mpp_read_region_array(context, &
                                 file_index, &
                                 field, &
                                 var_data, &
                                 data_sizes, &
                                 corner_indices, &
                                 edge_lengths)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(*),intent(inout) :: var_data
    integer(kind=c_size_t),dimension(:),intent(in) :: data_sizes
    integer(kind=c_int),dimension(:),intent(in) :: corner_indices
    integer(kind=c_int),dimension(:),intent(in) :: edge_lengths

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: i

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_reader) then
        if (size(corner_indices) .ne. 4 .or. size(edge_lengths) .ne. 4) then
            call mpp_error(FATAL, &
                           "mpp_read_region_array: size of corner indices and" &
                               //" edge lengths arrays must be 4.")
        endif

        do i = 1,size(data_sizes)
            if (data_sizes(i) .ne. edge_lengths(i)) then
                call mpp_error(FATAL, &
                               "mpp_read_region_array: size mismatch between" &
                                   //" data and edge lengths.")
            endif
        enddo

        do i = size(data_sizes)+1,size(edge_lengths)
            if (edge_lengths(i) .ne. 1) then
                call mpp_error(FATAL, &
                               "mpp_read_region_array: edge_lengths must be" &
                                   //" 1.")
            endif
        enddo

        call mpp_read_r_array(context, &
                              file_index, &
                              field, &
                              var_data, &
                              data_sizes, &
                              .false._c_bool, &
                              corner_indices_in=corner_indices, &
                              edge_lengths_in=edge_lengths)
    endif

end subroutine mpp_read_region_array
