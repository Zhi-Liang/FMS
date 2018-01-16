!Add a dimension and field (and attributes to the field) associated with a
!model "axis" to a netcdf file.  Buffer the axis data, to be written out
!later.
subroutine mpp_write_meta_axis_i1d(unit, &
                                   axis, &
                                   name, &
                                   units, &
                                   longname, &
                                   data, &
                                   min, &
                                   compressed)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(axistype),intent(inout) :: axis
    character(len=*),intent(in) :: name
    character(len=*),intent(in) :: units
    character(len=*),intent(in) :: longname
    integer(kind=c_int),dimension(:),intent(in) :: data
    integer(kind=c_int),intent(in),optional :: min
    character(len=*),intent(in),optional :: compressed

    call openmp_thread_trap()
    call mpp_write_meta_axis_i1d_(context, &
                                  unit, &
                                  axis, &
                                  name, &
                                  units, &
                                  longname, &
                                  data, &
                                  size(data,kind=c_size_t), &
                                  min, &
                                  compressed)

end subroutine mpp_write_meta_axis_i1d

subroutine mpp_write_meta_axis_r1d(unit, &
                                   axis, &
                                   name, &
                                   units, &
                                   longname, &
                                   cartesian, &
                                   sense, &
                                   domain, &
                                   data, &
                                   min, &
                                   calendar)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(axistype),intent(inout) :: axis
    character(len=*),intent(in) :: name
    character(len=*),intent(in) :: units
    character(len=*),intent(in) :: longname
    character(len=*),intent(in),optional :: cartesian
    integer(kind=c_int),intent(in),optional :: sense
    type(domain1D),intent(in),optional :: domain
    real(kind=c_double),dimension(:),intent(in),optional :: data
    real(kind=c_double),intent(in),optional :: min
    character(len=*),intent(in),optional :: calendar

    !Local variables
    integer(kind=c_size_t) :: data_size
    integer(kind=c_int),dimension(2) :: data_bounds

    call openmp_thread_trap()
    if (present(data)) then
        data_size = size(data,kind=c_size_t)
        data_bounds(1) = lbound(data,1)
        data_bounds(2) = ubound(data,1)
    else
        data_size = 0
        data_bounds(1) = 0
        data_bounds(2) = 0
    endif

    call mpp_write_meta_axis_r1d_(context, &
                                  unit, &
                                  axis, &
                                  name, &
                                  units, &
                                  longname, &
                                  data_size, &
                                  data_bounds,&
                                  cartesian, &
                                  sense, &
                                  domain, &
                                  data, &
                                  min, &
                                  calendar)

end subroutine mpp_write_meta_axis_r1d

subroutine mpp_write_meta_axis_unlimited(unit, &
                                         axis, &
                                         name, &
                                         data, &
                                         unlimited, &
                                         units, &
                                         longname)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(axistype),intent(inout) :: axis
    character(len=*),intent(in) :: name
    integer(kind=c_int),intent(in) :: data
    logical,intent(in) :: unlimited
    character(len=*),intent(in),optional :: units
    character(len=*),intent(in),optional :: longname

    call openmp_thread_trap()
    call mpp_write_meta_axis_unlimited_(context, &
                                        unit, &
                                        axis, &
                                        name, &
                                        data, &
                                        units, &
                                        longname)

end subroutine mpp_write_meta_axis_unlimited

subroutine mpp_write_meta_axis_i1d_(context, &
                                    file_index, &
                                    axis, &
                                    dim_name, &
                                    units, &
                                    longname, &
                                    dim_data, &
                                    dim_length, &
                                    dim_min, &
                                    compressed)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(axistype),intent(inout) :: axis
    character(len=*),intent(in) :: dim_name
    character(len=*),intent(in) :: units
    character(len=*),intent(in) :: longname
    integer(kind=c_int),dimension(*),target,intent(in) :: dim_data
    integer(kind=c_size_t),intent(in) :: dim_length
    integer(kind=c_int),intent(in),optional :: dim_min
    character(len=*),intent(in),optional :: compressed

    !Local variables
    integer(kind=c_int) :: f
    character(kind=c_char,len=1),dimension(:),allocatable :: name_buf
    integer(kind=c_int),dimension(1) :: dimension_index
    integer(kind=c_int) :: variable_index
    type(c_ptr) :: data_ptr

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_writer) then

        !Place the name in an interoperable buffer.
        call f_string_to_c(dim_name, &
                           name_buf)

        !Write the dimension to the netcdf file.  Buffer the values.
        dimension_index(1) = mpp_io_c_write_netcdf_dimension(f, &
                                                             name_buf, &
                                                             dim_length, &
                                                             .true._c_bool)

        !Store the dimension index.
        axis%did = dimension_index(1)

        !Add a variable to the netcdf file that will hold the dimension data
        !values.
        if (kind(dim_data) .ne. c_int) then
            call mpp_error(FATAL, &
                           "mpp_write_meta_axis_i1d_: axis data must have" &
                               //" kind=c_int.")
        endif
        variable_index = mpp_io_c_write_netcdf_variable_metadata(f, &
                                                                 MPP_IO_C_NC_INT, &
                                                                 name_buf, &
                                                                 dimension_index, &
                                                                 1, &
                                                                 MPP_IO_C_MAX_ATTS_PER_VAR, &
                                                                 .true._c_bool)

        !Store the variable_index.
        axis%id = variable_index
        deallocate(name_buf)

        !Get a pointer to the interoperable buffer.
        data_ptr = c_null_ptr
        data_ptr = c_loc(dim_data)

        !Buffer the data.
        call mpp_io_c_buffer_netcdf_variable_data(f, &
                                                  variable_index, &
                                                  (/int(0,kind=c_size_t)/), &
                                                  (/dim_length/), &
                                                  data_ptr, &
                                                  MPP_IO_C_NC_INT, &
                                                  .true._c_bool)

        !Clean up.
        data_ptr = c_null_ptr

        !Add attributes to the variable.
        call mpp_write_attribute_metadata(context, &
                                          file_index, &
                                          variable_index, &
                                          "long_name", &
                                          len_trim(longname,kind=c_size_t), &
                                          cval=longname)

        if (lowercase(trim(units)) .ne. "none" .or. .not. cf_compliance) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "units", &
                                              len_trim(units,kind=c_size_t), &
                                              cval=units)
        endif

        if (present(compressed)) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "compress", &
                                              len_trim(compressed,kind=c_size_t), &
                                              cval=compressed)
        endif

        if (present(dim_min)) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "valid_min", &
                                              int(1,kind=c_size_t), &
                                              ival=(/dim_min/))
        endif
    endif

end subroutine mpp_write_meta_axis_i1d_

subroutine mpp_write_meta_axis_r1d_(context, &
                                    file_index, &
                                    axis, &
                                    dim_name, &
                                    units, &
                                    longname, &
                                    data_size, &
                                    data_bounds, &
                                    cartesian, &
                                    sense, &
                                    domain, &
                                    dim_data, &
                                    dim_min, &
                                    calendar)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(axistype),intent(inout) :: axis
    character(len=*),intent(in) :: dim_name
    character(len=*),intent(in) :: units
    character(len=*),intent(in) :: longname
    integer(kind=c_size_t),intent(in) :: data_size
    integer(kind=c_int),dimension(2),intent(in) :: data_bounds
    character(len=*),intent(in),optional  :: cartesian
    integer(kind=c_int),intent(in),optional :: sense
    type(domain1D),target,intent(in),optional :: domain
    real(kind=c_double),dimension(*),intent(in),optional :: dim_data
    real(kind=c_double),intent(in),optional :: dim_min
    character(len=*),intent(in),optional  :: calendar

    !Local variables
    integer(kind=c_int) :: f
    character(kind=c_char,len=1),dimension(:),allocatable :: name_buf
    integer(kind=c_int) :: i
    integer(kind=c_size_t) :: dimension_length
    integer(kind=c_int),dimension(1) :: dimension_index
    integer(kind=c_int) :: variable_index
    type(c_ptr) :: data_ptr
    real(kind=c_double),dimension(:),allocatable,target :: data_buf
    type(domain1D) :: xdomain
    type(domain1D) :: ydomain
    integer(kind=c_int) :: is
    integer(kind=c_int) :: ie
    integer(kind=c_int) :: isg
    integer(kind=c_int) :: ieg

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !If a 1D domain was passed in, then store the "cartesian" string (which
    !signifies which axis the domain corresponds to), and a pointer to the
    !domain (used to check that it is compatible with any future domain
    !that is passed in, such as in mpp_write_2ddecomp).
    axis%domain => null()
    axis%cartesian = ""
    axis%shift = 0
    if (present(domain)) then

        if (present(cartesian)) then
            axis%cartesian = cartesian
        endif

        if (axis%cartesian .eq. "X" .or. axis%cartesian .eq. "Y") then
            if (context%files(file_index)%domain .ne. NULL_DOMAIN2D) then

                !Check that the inputted domain matches one of the
                !components of the stored 2d structured domain.
                call mpp_get_domain_components(context%files(file_index)%domain, &
                                               xdomain, &
                                               ydomain)
                if (axis%cartesian .eq. "X" .and. xdomain .ne. domain) then
                    call mpp_error(FATAL, &
                                   "mpp_write_meta_axis_r1d_: the inputted" &
                                       //" one-dimensional domain does" &
                                       //" not match the x component of" &
                                       //" of the two-dimensional domain" &
                                       //" associated with the file.")
                elseif (axis%cartesian .eq. "Y" .and. ydomain .ne. domain) then
                    call mpp_error(FATAL, &
                                   "mpp_write_meta_axis_r1d_: the inputted" &
                                       //" one-dimensional domain does" &
                                       //" not match the y component of" &
                                       //" of the two-dimensional domain" &
                                       //" associated with the file.")
                endif
            else

                !Store a pointer to the inputted domain, so that
                !it may be checked against in the
                !mpp_write_2ddecomp and mpp_read_2ddecomp calls.
                axis%domain => domain
            endif

        else
            call mpp_error(FATAL, &
                           "mpp_write_meta_axis_r1d_: if a domain" &
                               //" is present, then the inputted" &
                               //" cartesian string must be" &
                               //" either 'X' or 'Y'.")
        endif

        if (present(dim_data)) then

            !Get the starting and ending indices of the "global" part
            !of the inputted domain.
            call mpp_get_global_domain(domain, &
                                       isg, &
                                       ieg)

            !Determine the "shift" for the axis.  This is needed to
            !determine the "domain position" for any fields that
            !depend on this axis.
            if (data_size .eq. ieg-isg+2) then
                axis%shift = 1
            endif
        endif
    endif

    if (context%files(file_index)%is_writer) then

        !Place the dimension name in an interoperable buffer.
        call f_string_to_c(dim_name, &
                           name_buf)

        !Determine the dimension length, and which data will be stored.
        if (present(dim_data)) then
            dimension_length = -1
            if (context%files(file_index)%data_in_multi_files) then
                if (present(domain)) then

                    if (context%files(file_index)%io_domain .ne. NULL_DOMAIN2D) then

                        !Get the starting and ending indices of the "global" part
                        !of the I/O domain associated with the inputted domain.
                        if (axis%cartesian .eq. "X") then
                            call mpp_get_global_domain(context%files(file_index)%io_domain, &
                                                       xbegin=is, &
                                                       xend=ie)
                        elseif (axis%cartesian .eq. "Y") then
                            call mpp_get_global_domain(context%files(file_index)%io_domain, &
                                                       ybegin=is, &
                                                       yend=ie)

                        endif
                    else

                        !Get the starting and ending indices of the "compute" part
                        !of the I/O domain assoicated with the inputted domain.
                        call mpp_get_compute_domain(domain, &
                                                    is, &
                                                    ie)
                    endif

                    !Sanity check the indices.
                    if (ieg .lt. isg .or. ie .lt. is .or. is .lt. isg .or. &
                            ie .gt. ieg) then
                        call mpp_error(FATAL, &
                                       "mpp_write_meta_axis_r1d_: domain" &
                                           //" indices do not pass the" &
                                           //" sanity check.")
                    endif

                    if (data_size .eq. ieg-isg+2) then

                        !Shift the indices.
                        ie = ie + 1
                        ieg = ieg + 1
                    endif

                    !Calculate the dimension length.
                    dimension_length = ie - is + 1
                endif
            endif

            if (dimension_length .ne. -1) then

                !Make sure that the input data array corresponds to the
                !input domain.
                if (is-isg+1 .lt. data_bounds(1)) then
                    call mpp_error(FATAL, &
                                   "mpp_write_meta_axis_r1d_: the lower" &
                                       //" bound of the input data array" &
                                       //" is too large.")
                endif
                if (ie-isg+1 .gt. data_bounds(2)) then
                    call mpp_error(FATAL, &
                                   "mpp_write_meta_axis_r1d_: the upper" &
                                       //" bound of the input data array" &
                                       //" is too small.")
                endif

                !Use only part of the input data.
                allocate(data_buf(dimension_length))
                do i = 1,int(dimension_length,kind=c_int)
                    data_buf(i) = dim_data(is-isg+i)
                enddo

            else

                !Assume that all inputted data will be used.
                dimension_length = data_size
                allocate(data_buf(dimension_length))
                do i = 1,int(dimension_length,kind=c_int)
                    data_buf(i) = dim_data(i)
                enddo
            endif
        else

            !If the data array is not present, then this dimension is
            !assumed to be an unlimited dimension.
            dimension_length = MPP_IO_C_NC_UNLIMITED
        endif

        !Write the dimension to the netcdf file.  Buffer the values.
        dimension_index(1) = mpp_io_c_write_netcdf_dimension(f, &
                                                             name_buf, &
                                                             dimension_length, &
                                                             .true._c_bool)

        !Store the dimension index.
        axis%did = dimension_index(1)

        !Add a variable to the netcdf file that will hold the dimension data
        !values.
        variable_index = mpp_io_c_write_netcdf_variable_metadata(f, &
                                                                 MPP_IO_C_NC_DOUBLE, &
                                                                 name_buf, &
                                                                 dimension_index, &
                                                                 1, &
                                                                 MPP_IO_C_MAX_ATTS_PER_VAR, &
                                                                 .true._c_bool)

        !Store the variable_index.
        axis%id = variable_index
        deallocate(name_buf)

        if (allocated(data_buf)) then

            !Get a pointer to the data.
            data_ptr = c_null_ptr
            data_ptr = c_loc(data_buf)

            !Buffer the data.
            call mpp_io_c_buffer_netcdf_variable_data(f, &
                                                      variable_index, &
                                                      (/int(0,kind=c_size_t)/), &
                                                      (/dimension_length/), &
                                                      data_ptr, &
                                                      MPP_IO_C_NC_DOUBLE, &
                                                      .true._c_bool)

            !Clean up.
            data_ptr = c_null_ptr
        endif

        !Add attributes to the variable.
        call mpp_write_attribute_metadata(context, &
                                          file_index, &
                                          variable_index, &
                                          "long_name", &
                                          len_trim(longname,kind=c_size_t), &
                                          cval=longname)

        if (lowercase(trim(units)) .ne. "none" .or. .not. cf_compliance) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "units", &
                                              len_trim(units,kind=c_size_t), &
                                              cval=units)
        endif

        if (present(calendar)) then
            if (.not. cf_compliance) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "calendar", &
                                                  len_trim(calendar,kind=c_size_t), &
                                                  cval=calendar)
            else
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "calendar", &
                                                  len_trim(calendar,kind=c_size_t), &
                                                  cval=lowercase(calendar))
            endif
        endif

        if (present(cartesian)) then
            if (.not. cf_compliance) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "cartesian_axis", &
                                                  len_trim(cartesian,kind=c_size_t), &
                                                  cval=cartesian)
            else
                if (trim(cartesian) .ne. 'N') then
                    call mpp_write_attribute_metadata(context, & 
                                                      file_index, &
                                                      variable_index, &
                                                      "axis", &
                                                      len_trim(cartesian,kind=c_size_t), &
                                                      cval=cartesian)
                endif
            endif
        endif

        if (present(sense)) then
            if (sense .eq. -1) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "positive", &
                                                  int(4,kind=c_size_t), &
                                                  cval="down")
            elseif (sense .eq. 1) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "positive", &
                                                  int(2,kind=c_size_t), &
                                                  cval="up")
            endif
        endif

        if (present(dim_min)) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "valid_min", &
                                              int(1,kind=c_size_t), &
                                              rval=(/dim_min/))
        endif

        if (context%files(file_index)%data_in_multi_files .and. &
                present(domain) .and. present(dim_data)) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "domain_decomposition", &
                                              int(4,kind=c_size_t), &
                                              ival=(/isg,ieg,is,ie/))
        endif
    endif

end subroutine mpp_write_meta_axis_r1d_

subroutine mpp_write_meta_axis_unlimited_(context, &
                                          file_index, &
                                          axis, &
                                          dim_name, &
                                          dim_data, &
                                          units, &
                                          longname)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(axistype),intent(inout) :: axis
    character(len=*),intent(in) :: dim_name
    integer(kind=c_int),intent(in),target :: dim_data
    character(len=*),intent(in),optional :: units
    character(len=*),intent(in),optional :: longname

    !Local variables
    integer(kind=c_int) :: f
    character(kind=c_char,len=1),dimension(:),allocatable :: name_buf
    integer(kind=c_int),dimension(1) :: dimension_index
    integer(c_int) :: variable_index
    type(c_ptr) :: data_ptr

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = get_c_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_writer) then

        !Place the name in an interoperable buffer.
        call f_string_to_c(dim_name, &
                           name_buf)

        !Write the dimension to the netcdf file.  Buffer the values.
        dimension_index(1) = mpp_io_c_write_netcdf_dimension(f, &
                                                             name_buf, &
                                                             int(1,kind=c_size_t), &
                                                             .true._c_bool)

        !Store the dimension index.
        axis%did = dimension_index(1)

        !Add a variable to the netcdf file that will hold the dimension data
        !values.
        variable_index = mpp_io_c_write_netcdf_variable_metadata(f, &
                                                                 MPP_IO_C_NC_INT, &
                                                                 name_buf, &
                                                                 dimension_index, &
                                                                 1, &
                                                                 MPP_IO_C_MAX_ATTS_PER_VAR, &
                                                                 .true._c_bool)

        !Store the variable_index.
        axis%id = variable_index
        deallocate(name_buf)

        !Get a pointer to the interoperable buffer.
        data_ptr = c_null_ptr
        data_ptr = c_loc(dim_data)

        !Buffer the dimension data.
        call mpp_io_c_buffer_netcdf_variable_data(f, &
                                                  variable_index, &
                                                  (/int(0,kind=c_size_t)/), &
                                                  (/int(1,kind=c_size_t)/), &
                                                  data_ptr, &
                                                  MPP_IO_C_NC_INT, &
                                                  .true._c_bool)

        !Clean up.
        data_ptr = c_null_ptr

        !Add attributes to the variable.
        if (present(longname)) then
            call mpp_write_attribute_metadata(context, &
                                              file_index, &
                                              variable_index, &
                                              "long_name", &
                                              len_trim(longname,kind=c_size_t), &
                                              cval=longname)
        endif

        if (present(units)) then
            if (lowercase(trim(units)) .ne. "none" .or. .not. cf_compliance) then
                call mpp_write_attribute_metadata(context, &
                                                  file_index, &
                                                  variable_index, &
                                                  "units", &
                                                  len_trim(units,kind=c_size_t), &
                                                  cval=units)
            endif
        endif
    endif

end subroutine mpp_write_meta_axis_unlimited_
