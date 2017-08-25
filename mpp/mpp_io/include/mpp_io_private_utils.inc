!------------------------------------------------------------------------------
!Throw a fatal error if this routine is called in an openmp threaded region.
subroutine openmp_thread_trap()
#ifdef _OPENMP
    use omp_lib, only: omp_get_level
    implicit none

    if (omp_get_level() .ne. 0) then
        call mpp_error(FATAL, &
                       "openmp_thread_trap: the calling routine is not" &
                           //" guaranteed to be OpenMP thread-safe.")
    endif

#endif
end subroutine openmp_thread_trap

!------------------------------------------------------------------------------
!Make sure that the mpp_io module has been initialized.  Need to avoid
!segmentation faults.
subroutine check_is_init()
    implicit none

    if (.not. module_is_initialized) then
        call mpp_error(FATAL, &
                       "check_is_init: you must first call mpp_io_init.")
    endif

end subroutine check_is_init

!------------------------------------------------------------------------------
!Make sure that an inputted file index is inside the files array of a context,
!in order to avoid segmentation faults.
function check_file_index(context, &
                          file_index) result(file_index_out)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(in) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int) :: file_index_out

    !Local variables
    integer(kind=c_int) :: lb1
    integer(kind=c_int) :: lb2
    integer(kind=c_int) :: lb3

    lb1 = context%num_netcdf_files
    lb2 = lb1 + context%num_regular_files
    lb3 = lb2 + context%num_hdf5_files

    if (file_index .ge. 1 .and. file_index .le. lb1) then
        file_index_out = file_index - 1
    elseif (file_index .gt. lb1 .and. file_index .le. lb2) then
        file_index_out = file_index - lb1 - 1
    elseif (file_index .gt. lb2 .and. file_index .le. lb3) then
        file_index_out = file_index - lb2 - 1
    else
        call mpp_error(FATAL, &
                       "check_file_index: the inputted file index must" &
                           //" be >= 0 and < the maximum number of files" &
                           //" allowed in the inputted mpp_io context.")
    endif

end function check_file_index

!------------------------------------------------------------------------------
!Convert from a file index returned from c to one needed by fortran.
function convert_c_file_index(context, &
                              file_type, &
                              file_index_in) result(file_index_out)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(in) :: context
    integer(kind=c_int),intent(in) :: file_type
    integer(kind=c_int),intent(in) :: file_index_in
    integer(kind=c_int) :: file_index_out

    if (file_type .eq. MPP_NETCDF) then
        file_index_out = file_index_in + 1
    elseif (file_type .eq. MPP_ASCII) then
        file_index_out = file_index_in + 1 + context%num_netcdf_files
    elseif (file_type .eq. MPP_HDF5) then
        file_index_out = file_index_in + 1 + context%num_netcdf_files + &
                             context%num_regular_files
    else
        call mpp_error(FATAL, &
                       "convert_c_file_index: the inputted file type must" &
                           //" be either MPP_NETCDF, MPP_ASCII, or" &
                           //" MPP_HDF5.")
    endif

end function convert_c_file_index

!------------------------------------------------------------------------------
!Make sure that the file associated with an inputted file index is a netcdf
!file.
subroutine check_is_netcdf_file(context, &
                                file_index)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(in) :: context
    integer(kind=c_int),intent(in) :: file_index

    if (context%files(file_index)%file_type .ne. MPP_NETCDF) then
        call mpp_error(FATAL, &
                       "check_is_netcdf_file: the inputted file index must" &
                           //" correspond to a netcdf file.")
    endif

end subroutine check_is_netcdf_file

!------------------------------------------------------------------------------
!Make sure that the 2d domain that is passed in matches the one stored in
!the inputted context.
subroutine domain2d_check(context, &
                          file_index, &
                          domain)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(in) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(domain2d),intent(in) :: domain

    if (.not. associated(context%files(file_index)%domain)) then
        call mpp_error(FATAL, &
                       "domain2d_check: the inputted context contains a" &
                           //" null 2d domain.")
    endif

    if (context%files(file_index)%domain .ne. domain) then
        call mpp_error(FATAL, &
                       "domain2d_check: the inputted 2d domain does not" &
                           //" match the domain stored in the inputted" &
                           //" mpp_io context at the inputted file index.")
    endif

end subroutine domain2d_check

!------------------------------------------------------------------------------
!Make sure that the unstructured domain that is passed in matches the one
!stored in the inputted context.
subroutine domainUG_check(context, &
                          file_index, &
                          domain)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(in) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(domainUG),intent(in) :: domain

    if (.not. associated(context%files(file_index)%domain_UG)) then
        call mpp_error(FATAL, &
                       "domainUG_check: the inputted context contains a" &
                           //" null unstructured domain.")
    endif

    if (context%files(file_index)%domain_UG .ne. domain) then
        call mpp_error(FATAL, &
                       "domainUG_check: the inputted unstructured domain" &
                           //" does not match the domain stored in the" &
                           //" inputted mpp_io context at the inputted" &
                           //" file index.")
    endif

end subroutine domainUG_check

!------------------------------------------------------------------------------
!Copy a c string to a fortran string.
subroutine c_string_to_f(ptr, &
                         fstring)
    implicit none

    !Inputs/outputs
    type(c_ptr),intent(in) :: ptr
    character(len=*),intent(inout) :: fstring

    !Local variables
    character(kind=c_char,len=1),dimension(:),pointer :: f_char_ptr
    integer(kind=c_int) :: name_len
    integer(kind=c_int) :: i

    name_len = int(mpp_io_c_strlen(ptr),kind=c_int)
    if (name_len .gt. len(fstring)-1) then
        call mpp_error(FATAL, &
                       "c_string_to_f: length of c string >" &
                           //" len(f string)-1.")
    endif
    call c_f_pointer(ptr, &
                     f_char_ptr, &
                     [name_len])
    fstring = ""
    do i = 1,name_len
        fstring(i:i) = f_char_ptr(i)
    enddo

end subroutine c_string_to_f

!------------------------------------------------------------------------------
!Advance the unlimited dimension level for the file (if necessary) and get
!type parameter for how the variable is stored in memory.
subroutine prepare_write_r8(context, &
                            file_index, &
                            field, &
                            type_in_mem, &
                            unlim_dim_level, &
                            tstamp)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    integer(kind=c_int),intent(out) :: type_in_mem
    integer(kind=c_int),intent(out) :: unlim_dim_level
    real(kind=c_double),intent(in),target,optional :: tstamp

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: dimension_index
    integer(kind=c_int) :: variable_index
    logical(kind=c_bool) :: advance_unlim_dim
    logical(kind=c_bool) :: write_new_unlim_dim_val
    type(c_ptr) :: unlim_dim_value_ptr
    integer(kind=c_int) :: unlim_dim_value_type_in_mem
    real(kind=c_double),dimension(:),pointer :: data_ptr

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_writer) then

        !Get the type that the variable data is stored in memory as.
        type_in_mem = MPP_IO_C_NC_DOUBLE

        !Get the current unlimited dimension level (if the variable is
        !associated with one, designated by a field%time_axis_index != -1).
        !If the variable does not depend on an unlimited dimension, then
        !return -1 for the current unlimited dimension level.
        if (field%time_axis_index .ne. -1) then
            dimension_index = field%axes(field%time_axis_index)%did
            unlim_dim_level = mpp_io_c_get_netcdf_dimension_current_level(f, &
                                                                          dimension_index)
        else

            !As a precaution, trap when tstamp is passed in, but the variable
            !does not depend on an unlimited dimension.
            if (present(tstamp)) then
                call mpp_error(FATAL, &
                               "a time stamp was passed in, but the inputted" &
                                   //" field does not have an unlimited axis.")
            endif

            unlim_dim_level = -1
            return
        endif

        !Advance the unlimited dimension in the file if tstamp is greater than
        !the data value stored in the unlimited dimension associated with this
        !variable.
        if (present(tstamp)) then

            !Get the index of the variable used to hold the unlimited dimension
            !data values.
            variable_index = field%axes(field%time_axis_index)%id

            write_new_unlim_dim_val = .false._c_bool
            advance_unlim_dim = .false._c_bool
            if (mpp_io_c_get_netcdf_variable_is_data_buffered(f, &
                                                              variable_index)) then

                !If data was previously buffered for the unlimited dimension,
                !get the data value and its type.
                unlim_dim_value_ptr = c_null_ptr
                unlim_dim_value_ptr = mpp_io_c_get_netcdf_variable_buffered_data(f, &
                                                                                 variable_index)
                if (c_associated(unlim_dim_value_ptr)) then
                    unlim_dim_value_type_in_mem = mpp_io_c_get_netcdf_variable_type_in_mem(f, &
                                                                                           variable_index)

                    !Throw an error if the type of the previously buffered data
                    !does not match the type of the inputted tstamp variable.
                    if (unlim_dim_value_type_in_mem .ne. MPP_IO_C_NC_DOUBLE) then
                        call mpp_error(FATAL, &
                                       "the kind of tstamp is not the same as" &
                                           //" the kind of the data currently" &
                                           //" stored in the unlimited" &
                                           //" dimension.")
                    endif

                    !Compare the previously buffered data value to the inputted
                    !tstamp value.
                    data_ptr => null()
                    call c_f_pointer(unlim_dim_value_ptr, &
                                     data_ptr, &
                                     [1])
                    if (tstamp > data_ptr(1)) then
                        write_new_unlim_dim_val = .true._c_bool
                        advance_unlim_dim = .true._c_bool
                    endif
                    data_ptr => null()
                    unlim_dim_value_ptr = c_null_ptr
                endif
            else
                write_new_unlim_dim_val = .true._c_bool
            endif

            if (write_new_unlim_dim_val) then

                if (advance_unlim_dim) then

                    !Advance the unlimited dimension.
                    call mpp_io_c_advance_netcdf_dimension_level(f, &
                                                                 dimension_index)

                    !Get the unlimited dimension level.
                    unlim_dim_level = mpp_io_c_get_netcdf_dimension_current_level(f, &
                                                                                  dimension_index)
                endif

                !Buffer the tstamp value.
                call mpp_io_c_buffer_netcdf_variable_data(f, &
                                                          variable_index, &
                                                          (/int(unlim_dim_level,kind=c_size_t)/), &
                                                          (/int(1,kind=c_size_t)/), &
                                                          c_loc(tstamp), &
                                                          MPP_IO_C_NC_DOUBLE, &
                                                          .true._c_bool)

                !Write out the tstamp value.
                call mpp_io_c_write_buffered_netcdf_variable_data(f, &
                                                                  variable_index, &
                                                                  .false._c_bool)
            endif
        endif
    endif

end subroutine prepare_write_r8

!------------------------------------------------------------------------------
!Fill in an atttype from data stored on the c side.
subroutine fill_atttype_from_index(context, &
                                   file_index, &
                                   var_index, &
                                   att_index, &
                                   att)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: var_index
    integer(kind=c_int),intent(in) :: att_index
    type(atttype),intent(out) :: att

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_size_t) :: num_vals
    type(c_ptr) :: ptr
    character(kind=c_char,len=1),dimension(:),pointer :: f_char_ptr
    integer(kind=c_int) :: i
    integer(kind=c_short),dimension(:),pointer :: f_short_ptr
    integer(kind=c_int),dimension(:),pointer :: f_int_ptr
    real(kind=c_float),dimension(:),pointer :: f_float_ptr
    real(kind=c_double),dimension(:),pointer :: f_double_ptr

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Set default values.
    att = default_att

    if (context%files(file_index)%is_reader .or. &
            context%files(file_index)%is_writer) then

        !Store the attribute's type.
        att%type = mpp_io_c_get_netcdf_attribute_type_in_file(f, &
                                                              var_index, &
                                                              att_index)

        !Store the number of values contained in the attribute.
        num_vals = mpp_io_c_get_netcdf_attribute_num_values(f, &
                                                            var_index, &
                                                            att_index)
        att%len = int(num_vals,kind=c_int)

        !Store the name of the attribute.
        ptr = c_null_ptr
        ptr = mpp_io_c_get_netcdf_attribute_name(f, &
                                                 var_index, &
                                                 att_index)
        call c_string_to_f(ptr, &
                           att%name)
        ptr = c_null_ptr

        !Store the attribute values.
        ptr = mpp_io_c_get_netcdf_attribute_values(f, &
                                                   var_index, &
                                                   att_index)
        if (att%type .eq. MPP_IO_C_NC_CHAR) then

            !Do not store the c null byte in memory in fortran.
            att%len = att%len - 1

            if (att%len .gt. len(att%catt)) then
                call mpp_error(FATAL, &
                               "fill_atttype_from_index: length of attribute" &
                                   //" > len(att%catt).")
            endif
            f_char_ptr => null()
            call c_f_pointer(ptr, &
                             f_char_ptr, &
                             [att%len])
            do i = 1,att%len
                att%catt(i:i) = f_char_ptr(i)
            enddo
            f_char_ptr => null()
        else
            allocate(att%fatt(att%len))
            if (att%type .eq. MPP_IO_C_NC_SHORT) then
                f_short_ptr => null()
                call c_f_pointer(ptr, &
                                 f_short_ptr, &
                                 [att%len])
                do i = 1,att%len
                    att%fatt(i) = real(f_short_ptr(i),kind=c_double)
                enddo
                f_short_ptr => null()
            elseif (att%type .eq. MPP_IO_C_NC_INT) then
                f_int_ptr => null()
                call c_f_pointer(ptr, &
                                 f_int_ptr, &
                                 [att%len])
                do i = 1,att%len
                    att%fatt(i) = real(f_int_ptr(i),kind=c_double)
                enddo
                f_int_ptr => null()
            elseif (att%type .eq. MPP_IO_C_NC_FLOAT) then
                f_float_ptr => null()
                call c_f_pointer(ptr, &
                                 f_float_ptr, &
                                 [att%len])
                do i = 1,att%len
                    att%fatt(i) = real(f_float_ptr(i),kind=c_double)
                enddo
                f_float_ptr => null()
            elseif (att%type .eq. MPP_IO_C_NC_DOUBLE) then
                f_double_ptr => null()
                call c_f_pointer(ptr, &
                                 f_double_ptr, &
                                 [att%len])
                do i = 1,att%len
                    att%fatt(i) = f_double_ptr(i)
                enddo
                f_double_ptr => null()
            else
                call mpp_error(FATAL, &
                               "fill_atttype_from_index: currently supported" &
                                   //" attribute types are: MPP_IO_C_NC_CHAR," &
                                   //" MPP_IO_NC_SHORT," &
                                   //" MPP_IO_NC_INT, MPP_IO_NC_FLOAT, and" &
                                   //" MPP_IO_NC_DOUBLE.")

            endif
        endif
        ptr = c_null_ptr
    endif

end subroutine fill_atttype_from_index

!------------------------------------------------------------------------------
!Fill in an axistype from data stored on the c side.
subroutine fill_axistype_from_index(context, &
                                    file_index, &
                                    dim_index, &
                                    axis)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: dim_index
    type(axistype),intent(out) :: axis

    !Local variables
    integer(kind=c_int) :: f
    type(c_ptr) :: ptr
    integer(kind=c_int) :: var_index
    integer(kind=c_int),dimension(:),allocatable,target :: int_buf
    real(kind=c_float),dimension(:),allocatable,target :: float_buf
    real(kind=c_double),dimension(:),allocatable,target :: double_buf
    integer(kind=c_int) :: i
    character(len=128) :: bounds_name
    logical(kind=c_bool) :: found_bounds
    integer(kind=c_int) :: bounds_var_index
    integer(kind=c_int) :: bounds_dim_index
    integer(kind=c_int) :: bounds_var_type
    integer(kind=c_int) :: bounds_name_len
    character(kind=c_char,len=1),dimension(:),allocatable,target :: bounds_name_buf
    integer(kind=c_int) :: data_size
    integer(kind=c_int) :: data_bounds_size
    integer(kind=c_size_t),dimension(2) :: corner_indices
    integer(kind=c_size_t),dimension(2) :: edge_lengths

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Set default values.
    axis = default_axis

    !Store the dimension index.
    axis%did = dim_index

    if (context%files(file_index)%is_reader .or. &
            context%files(file_index)%is_writer) then

        !Get dimension length.
        axis%len = int(mpp_io_c_get_netcdf_dimension_length(f, &
                                                            dim_index),kind=c_int)

        !Store the name of the dimension.
        ptr = c_null_ptr
        ptr = mpp_io_c_get_netcdf_dimension_name(f, &
                                                 dim_index)
        call c_string_to_f(ptr, &
                           axis%name)

        !Get the index of the variable that corresponds to the inputted
        !dimension.
        var_index = mpp_io_c_get_netcdf_variable_index(f, &
                                                       ptr)
        ptr = c_null_ptr

        !Normally I think we want to error out if there is no variable
        !that has the same name as the dimension with something like:
        !
        !if (var_index .eq. MPP_IO_C_INDEX_NOT_FOUND) then
        !    call mpp_error(FATAL, &
        !                   "fill_axis_type_from_index: no corresponding" &
        !                       //" variable in netcdf file for dimension " &
        !                       //trim(axis%name)//".")
        !endif
        !
        !However we can't error in all cases, because files like grid_spec.nc
        !do not have variables defined for their dimensions.
        if (var_index .ne. MPP_IO_C_INDEX_NOT_FOUND) then

            !Store the dimension variable's type.
            axis%type = mpp_io_c_get_netcdf_variable_type_in_file(f, &
                                                              var_index)

            !Store the necessary indices.
            axis%id = var_index

            if (axis%len .gt. 0) then

                !Read in the dimension's variable data.
                select case (axis%type)
                    case (MPP_IO_C_NC_INT)

                        !Make sure unlimited dimensions are only floats or
                        !doubles.
                        if (mpp_io_c_get_netcdf_dimension_is_unlimited(f,dim_index)) then
                            call mpp_error(FATAL, &
                                           "fill_axis_type_from_index: netcdf unlimited" &
                                               //" dimension variables must be either" &
                                               //" floats or doubles.")
                        endif

                        allocate(int_buf(axis%len))
                        ptr = c_loc(int_buf)
                        call mpp_io_c_read_netcdf_variable_data(f, &
                                                                var_index, &
                                                                (/int(0,kind=c_size_t)/), &
                                                                (/int(axis%len,kind=c_size_t)/), &
                                                                ptr, &
                                                                axis%type)
                        ptr = c_null_ptr
                        allocate(axis%idata(axis%len))
                        do i = 1,axis%len
                            axis%idata(i) = int_buf(i)
                        enddo
                        deallocate(int_buf)

                    case (MPP_IO_C_NC_FLOAT)
                        allocate(float_buf(axis%len))
                        ptr = c_loc(float_buf)
                        call mpp_io_c_read_netcdf_variable_data(f, &
                                                                var_index, &
                                                                (/int(0,kind=c_size_t)/), &
                                                                (/int(axis%len,kind=c_size_t)/), &
                                                                ptr, &
                                                                axis%type)
                        ptr = c_null_ptr
                        allocate(axis%data(axis%len))
                        do i = 1,axis%len
                            axis%data(i) = real(float_buf(i),kind=c_double)
                        enddo
                        deallocate(float_buf)

                    case (MPP_IO_C_NC_DOUBLE)
                        allocate(double_buf(axis%len))
                        ptr = c_loc(double_buf)
                        call mpp_io_c_read_netcdf_variable_data(f, &
                                                                var_index, &
                                                                (/int(0,kind=c_size_t)/), &
                                                                (/int(axis%len,kind=c_size_t)/), &
                                                                ptr, &
                                                                axis%type)
                        ptr = c_null_ptr
                        allocate(axis%data(axis%len))
                        do i = 1,axis%len
                            axis%data(i) = double_buf(i)
                        enddo
                        deallocate(double_buf)
                    case default
                        call mpp_error(FATAL, &
                                       "fill_axis_type_from_index: netcdf dimension" &
                                           //" variables must be either integers," &
                                           //" floats, or doubles.")
                end select
            endif

            !Store the number of attributes associated with the dimension variable.
            axis%natt = mpp_io_c_get_netcdf_variable_num_attributes(f, &
                                                                    var_index)

            found_bounds = .false._c_bool
            if (axis%natt .gt. 0) then

                !Store dimension variable attributes.
                allocate(axis%Att(axis%natt))
                do i = 1,axis%natt
                    call fill_atttype_from_index(context, &
                                                 file_index, &
                                                 var_index, &
                                                 i-1, &
                                                 axis%Att(i))

                    !Fill in "pre-defined" attributes.
                    select case (trim(axis%Att(i)%name))
                        case ("long_name")
                            axis%longname = trim(axis%Att(i)%catt)
                        case ("units")
                            axis%units = trim(axis%Att(i)%catt)
                        case ("cartesian_axis")
                            axis%cartesian = trim(axis%Att(i)%catt)
                        case ("calendar","calendar_type")
                            axis%calendar = trim(lowercase(axis%Att(i)%catt))
                            if (axis%calendar .eq. "none") then
                                axis%calendar = "no_calendar"
                            elseif (axis%calendar .eq. "no_leap") then
                                axis%calendar = "noleap"
                            elseif (axis%calendar .eq. "365_days") then
                                axis%calendar = "365_day"
                            elseif (axis%calendar .eq. "360_days") then
                                axis%calendar = "360_day"
                            endif
                        case ("compress")
                            axis%compressed = trim(axis%Att(i)%catt)
                        case ("positive")
                            if (trim(axis%Att(i)%catt) .eq. "down") then
                                axis%sense = -1
                            elseif (trim(axis%Att(i)%catt) .eq. "up") then
                                axis%sense = 1
                            endif
                        case ("bounds","edges")
                            bounds_name = trim(axis%Att(i)%catt)
                            found_bounds = .true._c_bool
                    end select

                enddo
            endif

            !Assign "axis bounds".
            if (allocated(axis%data) .and. .not. &
                    mpp_io_c_get_netcdf_dimension_is_unlimited(f,dim_index)) then

                axis%name_bounds = "none"
                if (found_bounds) then

                    !Get the index of the variable that corresponds to the
                    !"data bounds".
                    bounds_name_len = len(bounds_name)
                    allocate(bounds_name_buf(bounds_name_len+1))
                    do i = 1,bounds_name_len
                        bounds_name_buf(i) = bounds_name(i:i)
                    enddo
                    bounds_name_buf(bounds_name_len+1) = c_null_char
                    bounds_var_index = mpp_io_c_get_netcdf_variable_index(f, &
                                                                          c_loc(bounds_name_buf))
                    deallocate(bounds_name_buf)

                    if (bounds_var_index .ne. MPP_IO_C_INDEX_NOT_FOUND) then

                        !Make sure the "data bounds" variable is two dimensional.
                        if (mpp_io_c_get_netcdf_variable_num_dimensions(f,bounds_var_index) .ne. 2) then
                            call mpp_error(FATAL, &
                                           "fill_axis_type_from_index: data bounds" &
                                               //" variables must be two dimensional.")
                        endif

                        !Make sure that the dimensions of the "data bounds"
                        !variable have the correct lengths.
                        bounds_dim_index = mpp_io_c_get_netcdf_dimension_index(f, &
                                                                               bounds_var_index, &
                                                                           0)
                        if (mpp_io_c_get_netcdf_dimension_length(f,bounds_dim_index) .ne. size(axis%data)) then
                            call mpp_error(FATAL, &
                                           "fill_axis_type_from_index: incorrect size" &
                                               //" for data bounds variable.")
                        endif
                        bounds_dim_index = mpp_io_c_get_netcdf_dimension_index(f, &
                                                                               bounds_var_index, &
                                                                           1)
                        if (mpp_io_c_get_netcdf_dimension_length(f,bounds_dim_index) .ne. 2) then
                            call mpp_error(FATAL, &
                                           "fill_axis_type_from_index: incorrect size" &
                                               //" for data bounds variable.")
                        endif

                        !Get the type of the data bounds variable.
                        bounds_var_type = mpp_io_c_get_netcdf_variable_type_in_file(f, &
                                                                                    bounds_var_index)

                        !Read in the "data bounds" data.
                        axis%name_bounds = trim(bounds_name)
                        data_size = size(axis%data)
                        data_bounds_size = data_size + 1
                        allocate(axis%data_bounds(data_bounds_size))
                        corner_indices(1) = int(0,kind=c_size_t)
                        corner_indices(2) = int(0,kind=c_size_t)
                        edge_lengths(1) = int(data_size,kind=c_size_t)
                        edge_lengths(2) = int(2,kind=c_size_t)
                        select case (bounds_var_type)
                            case (MPP_IO_C_NC_INT)
                                allocate(int_buf(2*data_size))
                                ptr = c_loc(int_buf)
                                call mpp_io_c_read_netcdf_variable_data(f, &
                                                                        bounds_var_index, &
                                                                        corner_indices, &
                                                                        edge_lengths, &
                                                                        ptr, &
                                                                        bounds_var_type)
                                ptr = c_null_ptr
                                do i = 1,data_bounds_size-1
                                    axis%data_bounds(i) = real(int_buf(2*i-1),kind=c_double)
                                enddo
                                axis%data_bounds(data_bounds_size) = int_buf(2*data_size)
                                deallocate(int_buf)

                            case (MPP_IO_C_NC_FLOAT)
                                allocate(float_buf(2*data_size))
                                ptr = c_loc(float_buf)
                                call mpp_io_c_read_netcdf_variable_data(f, &
                                                                        bounds_var_index, &
                                                                        corner_indices, &
                                                                        edge_lengths, &
                                                                        ptr, &
                                                                        bounds_var_type)
                                ptr = c_null_ptr
                                do i = 1,data_bounds_size-1
                                    axis%data_bounds(i) = real(float_buf(2*i-1),kind=c_double)
                                enddo
                                axis%data_bounds(data_bounds_size) = float_buf(2*data_size)
                                deallocate(float_buf)

                            case (MPP_IO_C_NC_DOUBLE)
                                allocate(double_buf(2*data_size))
                                ptr = c_loc(double_buf)
                                call mpp_io_c_read_netcdf_variable_data(f, &
                                                                        bounds_var_index, &
                                                                        corner_indices, &
                                                                        edge_lengths, &
                                                                        ptr, &
                                                                        bounds_var_type)
                                ptr = c_null_ptr
                                do i = 1,data_bounds_size-1
                                    axis%data_bounds(i) = double_buf(2*i-1)
                                enddo
                                axis%data_bounds(data_bounds_size) = double_buf(2*data_size)
                                deallocate(double_buf)

                            case default
                                call mpp_error(FATAL, &
                                               "fill_axis_type_from_index: invalid" &
                                                   //"type for bounds variable.")
                        end select
                    endif
                endif
            endif
        else

            !Put in bad values.
            axis%type = -1
            axis%id = -1
            axis%natt = -1
        endif
    endif

end subroutine fill_axistype_from_index

!------------------------------------------------------------------------------
!Fill in a fieldtype from data stored on the c side.
subroutine fill_fieldtype_from_index(context, &
                                     file_index, &
                                     var_index, &
                                     field)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: var_index
    type(fieldtype),intent(out) :: field

    !Local variables
    integer(kind=c_int) :: f
    type(c_ptr) :: ptr
    integer(kind=c_int) :: dim_index
    integer(kind=c_int) :: i
    character(len=64) :: checksum_char
    integer(kind=int64) :: checksumf
    integer(kind=c_int) :: num_checksumf
    integer(kind=c_int) :: last
    integer(kind=c_int) :: is
    integer(kind=c_int) :: k

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Set default values.
    field = default_field

    if (context%files(file_index)%is_reader .or. &
            context%files(file_index)%is_writer) then

        !Get the name of the variable.
        ptr = c_null_ptr
        ptr = mpp_io_c_get_netcdf_variable_name(f, &
                                                var_index)

        !Check if this variable is associated with a dimension.
        dim_index = mpp_io_c_get_netcdf_dimension_index(f, &
                                                        ptr)

        if (dim_index .eq. MPP_IO_C_INDEX_NOT_FOUND) then

            !Store the name of the variable.
            call c_string_to_f(ptr, &
                               field%name)

            !Store the variable's index.
            field%id = var_index

            !Store the variable's type.
            field%type = mpp_io_c_get_netcdf_variable_type_in_file(f, &
                                                                   var_index)

            !Determine the variable's "pack" value.
            select case (field%type)
                case (MPP_IO_C_NC_SHORT)
                    field%pack = 4
                case (MPP_IO_C_NC_FLOAT,MPP_IO_C_NC_INT)
                    field%pack = 2
                case (MPP_IO_C_NC_DOUBLE,MPP_IO_C_NC_CHAR)
                    field%pack = 1
                case default
                    call mpp_error(FATAL, &
                                   "fill_fieldtype_from_index: invalid" &
                                       //"variable type.  Must be one of:" &
                                       //" short, int, float, double, or" &
                                       //" char.")
            end select

            !Store the number of dimensions that the variable depends on.
            field%ndim = mpp_io_c_get_netcdf_variable_num_dimensions(f, &
                                                                     var_index)

            !Store the dimension data.  Flip the dimension order, as is
            !necessary due to the different orders in which Fortran and
            !c walk through the data.
            allocate(field%axes(field%ndim))
            allocate(field%size(field%ndim))
            do i = 1,field%ndim
                dim_index = mpp_io_c_get_netcdf_dimension_index(f, &
                                                                var_index, &
                                                                field%ndim-i)
                call fill_axistype_from_index(context, &
                                              file_index, &
                                              dim_index, &
                                              field%axes(i))

                if (mpp_io_c_get_netcdf_dimension_is_unlimited(f,dim_index)) then
                    field%time_axis_index = i
                    field%size(i) = 1
                else
                    field%size(i) = field%axes(i)%len
                endif
            enddo


            !Store the number of attributes associated with the variable.
            field%natt = mpp_io_c_get_netcdf_variable_num_attributes(f, &
                                                                     var_index)

            if (field%natt .gt. 0) then

                !Store variable attributes.
                allocate(field%Att(field%natt))
                do i = 1,field%natt
                    call fill_atttype_from_index(context, &
                                                 file_index, &
                                                 var_index, &
                                                 i-1, &
                                                 field%Att(i))

                    !Fill in "pre-defined" attributes.
                    select case (trim(field%Att(i)%name))
                        case ("long_name")
                            field%longname = trim(field%Att(i)%catt)
                        case ("units")
                            field%units = trim(field%Att(i)%catt)
                        case ("scale_factor")
                            field%scale = field%Att(i)%fatt(1)
                        case ("missing","missing_value")
                            field%missing = field%Att(i)%fatt(1)
                        case ("_FillValue")
                            field%fill = field%Att(i)%fatt(1)
                        case ("add_offset")
                            field%add = field%Att(i)%fatt(1)
                        case ("packing")
                            field%pack = int(field%Att(i)%fatt(1),kind=c_int)
                        case ("valid_range")
                            field%min = field%Att(i)%fatt(1)
                            field%max = field%Att(i)%fatt(2)
                        case ("checksum")
                            checksum_char = trim(field%Att(i)%catt)
                            checksumf = 0
                            num_checksumf = 1
                            last = len_trim(checksum_char)
                            is = index(trim(checksum_char),",")
                            do while ((is .gt. 0) .and. (is .lt. (last-15)))
                                is = is + scan(checksum_char(is:last),",")
                                num_checksumf = num_checksumf + 1
                            enddo
                            is = 1
                            do k = 1,num_checksumf
                                read(checksum_char(is:is+15),'(Z16)') checksumf
                                field%checksum(k) = checksumf
                                is = is + 17
                            enddo
                    end select
                enddo
            endif
        endif
        ptr = c_null_ptr
    endif

end subroutine fill_fieldtype_from_index

!------------------------------------------------------------------------------
