!Read in variable data from a netcdf file.
subroutine mpp_read_r0d(unit, &
                        field, &
                        data, &
                        tindex)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex

    !Local variables
    integer(kind=c_size_t),dimension(1) :: data_sizes
    real(kind=c_double),dimension(1) :: buf

    call openmp_thread_trap()
    data_sizes(1) = 1
    buf(1) = data
    call mpp_read_r_array(context, &
                          unit, &
                          field, &
                          buf, &
                          data_sizes,&
                          .false._c_bool, &
                          tindex)
    data = buf(1)

end subroutine mpp_read_r0d

subroutine mpp_read_r1d(unit, &
                        field, &
                        data, &
                        tindex)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex

    !Local variables
    integer(kind=c_size_t),dimension(1) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_read_r_array(context, &
                          unit, &
                          field, &
                          data, &
                          data_sizes,&
                          .false._c_bool, &
                          tindex)

end subroutine mpp_read_r1d

subroutine mpp_read_r2d(unit, &
                        field, &
                        data, &
                        tindex)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(:,:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex

    !Local variables
    integer(kind=c_size_t),dimension(2) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_read_r_array(context, &
                          unit, &
                          field, &
                          data, &
                          data_sizes,&
                          .false._c_bool, &
                          tindex)

end subroutine mpp_read_r2d

subroutine mpp_read_r3d(unit, &
                        field, &
                        data, &
                        tindex)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(:,:,:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex

    !Local variables
    integer(kind=c_size_t),dimension(3) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_read_r_array(context, &
                          unit, &
                          field, &
                          data, &
                          data_sizes,&
                          .false._c_bool, &
                          tindex)

end subroutine mpp_read_r3d

subroutine mpp_read_r4d(unit, &
                        field, &
                        data, &
                        tindex)

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype), intent(in) :: field
    real(kind=c_double),dimension(:,:,:,:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex

    !Local variables
    integer(kind=c_size_t),dimension(4) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_read_r_array(context, &
                          unit, &
                          field, &
                          data, &
                          data_sizes,&
                          .false._c_bool, &
                          tindex)

end subroutine mpp_read_r4d

subroutine mpp_read_r_array(context, &
                            file_index, &
                            field, &
                            var_data, &
                            var_data_sizes, &
                            is_scalar, &
                            unlim_dim_level, &
                            corner_indices_in, &
                            edge_lengths_in)

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    real(kind=c_double),dimension(*),target,intent(inout) :: var_data
    integer(kind=c_size_t),dimension(:),intent(in) :: var_data_sizes
    logical(kind=c_bool),intent(in) :: is_scalar
    integer(kind=c_int),intent(in),optional :: unlim_dim_level
    integer(kind=c_int),dimension(:),intent(in),optional :: corner_indices_in
    integer(kind=c_int),dimension(:),intent(in),optional :: edge_lengths_in

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: dim_level
    integer(kind=c_size_t),dimension(size(field%axes)) :: corner_indices
    integer(kind=c_size_t),dimension(size(field%axes)) :: edge_lengths
    integer(kind=c_size_t),dimension(size(field%axes)) :: corner_indices_tp
    integer(kind=c_size_t),dimension(size(field%axes)) :: edge_lengths_tp
    integer(kind=c_int) :: i
    integer(kind=c_int) :: unlim_dim_index
    logical(kind=c_bool) :: found_unlim
    type(c_ptr) :: data_ptr
    integer(kind=c_int) :: num_axes

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_reader) then

        !Make sure that the number of data dimensions is compatible with the
        !inputted netcdf variable.
!       if (size(field%axes) .lt. size(var_data_sizes) .or. size(field%axes) &
!               .gt. size(var_data_sizes) + 1) then
!           call mpp_error(FATAL, &
!                          "mpp_read_r_array: the inputted data does not match" &
!                              //" the shape of the inputted netcdf variable.")
!       endif

        !Get the corner indices and edge lengths for the variable.
        num_axes = size(corner_indices)
        if (present(corner_indices_in) .and. present(edge_lengths_in)) then

            !Make sure that the size of the corner indices array matches the
            !number of dimensions that the variable depends on.
!           if (size(corner_indices_in) .ne. size(corner_indices)) then
!               call mpp_error(FATAL, &
!                              "mpp_read_r_array: the size of the inputted" &
!                                  //" corner_indices_in array must be equal" &
!                                  //" to the number of dimensions that the" &
!                                  //" inputted variable depends on.")
!           endif

            !Make sure that the size of the edge lengths array matches the
            !number of dimensions that the variable depends on.
!           if (size(edge_lengths_in) .ne. size(edge_lengths)) then
!               call mpp_error(FATAL, &
!                              "mpp_read_r_array: the size of the inputted" &
!                                  //" edge_lengths_in array must be equal" &
!                                  //" to the number of dimensions that the" &
!                                  //" inputted variable depends on.")
!           endif

            !Use the inputted values.  Error checking is done on the c side.
            do i = 1,num_axes
                corner_indices(i) = int(corner_indices_in(i)-1,kind=c_size_t)
                edge_lengths(i) = int(edge_lengths_in(i),kind=c_size_t)
            enddo
        else

            !Set the unlimited dimension level that will be read.  If not passed
            !in, then default to level zero.
            if (present(unlim_dim_level)) then
                dim_level = unlim_dim_level - 1
            else
                dim_level = 0
            endif

            !Set the corner indices and edge lengths.
            unlim_dim_index = field%time_axis_index
            if (unlim_dim_index .eq. -1) then
                corner_indices = 0
                edge_lengths = var_data_sizes
            else
                if (is_scalar) then
                    corner_indices = dim_level
                    edge_lengths = 1
                else
                    found_unlim = .false._c_bool
                    do i = 1,num_axes
                        if (unlim_dim_index .eq. i) then
                            corner_indices(i) = dim_level
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
        endif

        !Point at the space that will hold the data.
        data_ptr = c_null_ptr
        data_ptr = c_loc(var_data)

        !Since c reads the data in "row-based" order, flip the dimension
        !order.
        do i = 1,num_axes
            corner_indices_tp(num_axes+1-i) = corner_indices(i)
            edge_lengths_tp(num_axes+1-i) = edge_lengths(i)
        enddo

        !Read in the data.
        call mpp_io_c_read_netcdf_variable_data(f, &
                                                int(field%id,kind=c_int), &
                                                corner_indices_tp, &
                                                edge_lengths_tp, &
                                                data_ptr,&
                                                MPP_IO_C_NC_DOUBLE)

        !Clean up.
        data_ptr = c_null_ptr
    endif

end subroutine mpp_read_r_array

subroutine mpp_read_text(unit, &
                         field, &
                         data, &
                         level)

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    character(len=*),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: level

    call openmp_thread_trap()
    call mpp_read_text_(context, &
                        unit, &
                        field, &
                        data, &
                        level)

end subroutine mpp_read_text

subroutine mpp_read_text_(context, &
                          file_index, &
                          field, &
                          var_data, &
                          unlim_dim_level)

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    character(len=*),intent(inout) :: var_data
    integer(kind=c_int),intent(in),optional :: unlim_dim_level

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: num_dims
    integer(kind=c_size_t),dimension(size(field%axes)) :: corner_indices
    integer(kind=c_size_t),dimension(size(field%axes)) :: edge_lengths
    integer(kind=c_int) :: dim_level
    character(kind=c_char,len=1),dimension(:),allocatable,target :: buf
    integer(kind=c_int) :: i
    type(c_ptr) :: data_ptr
    integer(kind=c_size_t),dimension(size(field%axes)) :: corner_indices_tp
    integer(kind=c_size_t),dimension(size(field%axes)) :: edge_lengths_tp

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_reader) then

        !Get the number of dimensions that the variable depends on.
        num_dims = size(field%axes)
        if (num_dims .lt. 1 .or. num_dims .gt. 2) then
            call mpp_error(FATAL, &
                           "mpp_read_text_: the inputted variable must" &
                               //" be either one- or two-dimensional.")
        endif

        !This routine assumes that the variable's first dimension is
        !the length of the string and the variable's second dimension (if
        !it has one) is an unlimited dimension.  If an interface change is
        !made, this assumption can be relaxed.
        if (len(var_data) .lt. field%size(1)) then
            call mpp_error(FATAL, &
                           "mpp_read_text_: the inputted string is not" &
                               //" large enough to hold the data that will" &
                               //" be read in.")
        endif

        !Set corner indices and edge lengths.
        corner_indices(1) = 0
        edge_lengths(1) = field%size(1)

        if (num_dims .eq. 2) then

            !Get the unlimited dimension level.
            if (present(unlim_dim_level)) then
                dim_level = unlim_dim_level - 1
            else
                dim_level = 0
            endif

            corner_indices(2) = dim_level
            edge_lengths(2) = 1
        endif

        !Set up a buffer that is interoperable with c and point at it.
        allocate(buf(field%size(1)+1))
        do i = 1,field%size(1)
            buf(i) = ''
        enddo
        buf(field%size(1)+1) = c_null_char
        data_ptr = c_null_ptr
        data_ptr = c_loc(buf)

        !Since c reads the data in "row-based" order, flip the dimension
        !order.
        do i = 1,num_dims
            corner_indices_tp(num_dims+1-i) = corner_indices(i)
            edge_lengths_tp(num_dims+1-i) = edge_lengths(i)
        enddo

        !Read in the data.
        call mpp_io_c_read_netcdf_variable_data(f, &
                                                int(field%id,kind=c_int), &
                                                corner_indices_tp, &
                                                edge_lengths_tp, &
                                                data_ptr,&
                                                MPP_IO_C_NC_CHAR)

        !Get the read in data.
        var_data = ""
        do i = 1,field%size(1)
            if (buf(i) .eq. c_null_char) then
                exit
            else
                var_data(i:i) = buf(i)
            endif
        enddo

        !Clean up.
        data_ptr = c_null_ptr
        deallocate(buf)

    endif

end subroutine mpp_read_text_
