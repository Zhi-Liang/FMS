!Add an attribute to a netCDF file.
subroutine mpp_write_meta_global(unit, &
                                 name, &
                                 rval, &
                                 ival, &
                                 cval, &
                                 pack)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(in) :: name
    real(kind=c_double),dimension(:),intent(in),optional :: rval
    integer(kind=c_int),dimension(:),intent(in),optional :: ival
    character(len=*),intent(in),optional :: cval
    integer(kind=c_int),intent(in),optional :: pack

    !Local variables
    integer(kind=c_size_t) :: num_vals

    call openmp_thread_trap()

    if (present(rval)) then
        num_vals = size(rval,kind=c_size_t)
    elseif (present(ival)) then
        num_vals = size(ival,kind=c_size_t)
    elseif (present(cval)) then
        num_vals = len_trim(cval,kind=c_size_t)
    endif

    call mpp_write_attribute_metadata(context, &
                                      unit, &
                                      MPP_IO_C_GLOBAL_ATT, &
                                      name, &
                                      num_vals, &
                                      rval, &
                                      ival, &
                                      cval, &
                                      pack)

end subroutine mpp_write_meta_global

subroutine mpp_write_meta_global_scalar_r(unit, &
                                          name, &
                                          rval, &
                                          pack)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(in) :: name
    real(kind=c_double),intent(in) :: rval
    integer(kind=c_int),intent(in),optional :: pack

    call openmp_thread_trap()
    call mpp_write_attribute_metadata(context, &
                                      unit, &
                                      MPP_IO_C_GLOBAL_ATT, &
                                      name, &
                                      int(1,kind=c_size_t), &
                                      rval=(/rval/), &
                                      pack_param=pack)

end subroutine mpp_write_meta_global_scalar_r

subroutine mpp_write_meta_global_scalar_i(unit, &
                                          name, &
                                          ival, &
                                          pack)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(in) :: name
    integer(kind=c_int),intent(in) :: ival
    integer(kind=c_int),intent(in),optional :: pack

    call openmp_thread_trap()
    call mpp_write_attribute_metadata(context, &
                                      unit, &
                                      MPP_IO_C_GLOBAL_ATT, &
                                      name, &
                                      int(1,kind=c_size_t), &
                                      ival=(/ival/), &
                                      pack_param=pack)

end subroutine mpp_write_meta_global_scalar_i

subroutine mpp_write_meta_var(unit, &
                              id, &
                              name, &
                              rval, &
                              ival, &
                              cval, &
                              pack)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    integer(kind=c_int),intent(in) :: id
    character(len=*),intent(in) :: name
    real(kind=c_double),dimension(:),intent(in),optional :: rval
    integer(kind=c_int),dimension(:),intent(in),optional :: ival
    character(len=*),intent(in),optional :: cval
    integer(kind=c_int),intent(in),optional :: pack

    !Local variables
    integer(kind=c_size_t) :: num_vals

    call openmp_thread_trap()

    if (present(rval)) then
        num_vals = size(rval,kind=c_size_t)
    elseif (present(ival)) then
        num_vals = size(ival,kind=c_size_t)
    elseif (present(cval)) then
        num_vals = len_trim(cval,kind=c_size_t)
    endif

    call mpp_write_attribute_metadata(context, &
                                      unit, &
                                      id, &
                                      name, &
                                      num_vals, &
                                      rval, &
                                      ival, &
                                      cval, &
                                      pack)

end subroutine mpp_write_meta_var

subroutine mpp_write_meta_scalar_r(unit, &
                                   id, &
                                   name, &
                                   rval, &
                                   pack)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    integer(kind=c_int),intent(in) :: id
    character(len=*),intent(in) :: name
    real(kind=c_double),intent(in) :: rval
    integer(kind=c_int),intent(in),optional :: pack

    call openmp_thread_trap()
    call mpp_write_attribute_metadata(context, &
                                      unit, &
                                      id, &
                                      name, &
                                      int(1,kind=c_size_t), &
                                      rval=(/rval/), &
                                      pack_param=pack)

end subroutine mpp_write_meta_scalar_r

subroutine mpp_write_meta_scalar_i(unit, &
                                   id, &
                                   name, &
                                   ival, &
                                   pack)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    integer(kind=c_int),intent(in) :: id
    character(len=*),intent(in) :: name
    integer(kind=c_int),intent(in) :: ival
    integer(kind=c_int),intent(in),optional :: pack

    call openmp_thread_trap()
    call mpp_write_attribute_metadata(context, &
                                      unit, &
                                      id, &
                                      name, &
                                      int(1,kind=c_size_t), &
                                      ival=(/ival/), &
                                      pack_param=pack)

end subroutine mpp_write_meta_scalar_i

subroutine mpp_write_attribute_metadata(context, &
                                        file_index, &
                                        variable_index, &
                                        attribute_name, &
                                        num_values, &
                                        rval, &
                                        ival, &
                                        cval, &
                                        pack_param)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in) :: variable_index
    character(len=*),intent(in) :: attribute_name
    integer(kind=c_size_t),intent(in) :: num_values
    real(kind=c_double),dimension(*),target,intent(in),optional :: rval
    integer(kind=c_int),dimension(*),target,intent(in),optional :: ival
    character(len=*),intent(in),optional :: cval
    integer(kind=c_int),intent(in),optional :: pack_param

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: opt_args_count
    integer(kind=c_int) :: type_in_mem
    integer(kind=c_int) :: type_in_file
    integer(kind=c_int) :: buf_len
    character(kind=c_char,len=1),dimension(:),allocatable :: name_buf
    integer(kind=c_size_t) :: i
    type(c_ptr) :: val_ptr
    character(kind=c_char,len=1),dimension(:),allocatable,target :: text_buf
    integer(kind=c_size_t) :: num_vals
    integer(kind=c_int) :: attribute_index

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_writer) then

        !Initialize the variable that counts the number of optional input val
        !variables.
        opt_args_count = 0

        !Handle real input.
        if (present(rval)) then

            !Iterate the optional input val counter.
            opt_args_count = opt_args_count + 1

            !Get the type of the inputted variable.
            type_in_mem = MPP_IO_C_NC_DOUBLE

            !Get the type that the variable will be written to the netCDF file as.
            if (present(pack_param)) then
                if (pack_param .eq. 1) then
                    type_in_file = MPP_IO_C_NC_DOUBLE
                elseif (pack_param .eq. 2) then
                    type_in_file = MPP_IO_C_NC_FLOAT
                elseif (pack_param .eq. 4) then
                    type_in_file = MPP_IO_C_NC_SHORT
                elseif (pack_param .eq. 8) then
                    type_in_file = MPP_IO_C_NC_BYTE
                else
                    call mpp_error(FATAL, &
                                   "mpp_write_attribute_metadata: the only" &
                                       //" supported pack values for real" &
                                       //" variables are 1, 2, 4, and 8.")
                endif
            else
                type_in_file = MPP_IO_C_NC_FLOAT
            endif
        endif

        !Handle integer input.
        if (present(ival)) then

            !Iterate the optional input val counter.
            opt_args_count = opt_args_count + 1

            !Get the type of the inputted variable.
            type_in_mem = MPP_IO_C_NC_INT

            !Get the type that the variable will be written to the netCDF file as.
            type_in_file = MPP_IO_C_NC_INT
            if (present(pack_param)) then
                if (pack_param .ne. 0) then
                    call mpp_error(FATAL, &
                                   "mpp_write_attribute_metadata: the only" &
                                       //" supported pack value for integer" &
                                       //" variables is 0.")
                endif
            endif
        endif

        !Handle character input.
        if (present(cval)) then

            !Iterate the optional input val counter.
            opt_args_count = opt_args_count + 1

            !Get the type of the inputted variable.
            type_in_mem = MPP_IO_C_NC_CHAR

            !Get the type that the variable will be written to the netCDF file as.
            type_in_file = MPP_IO_C_NC_CHAR
        endif

        !Make sure that exactly one val input was passed in.
        if (opt_args_count .ne. 1) then
            call mpp_error(FATAL, &
                           "mpp_write_attribute_metadata: you must pass in" &
                               //" one of: rval, ival, or cval.")
        endif

        !Copy the name into an interoperable buffer and null terminate it.
        buf_len = len_trim(attribute_name)
        allocate(name_buf(buf_len+1))
        do i = 1,buf_len
            name_buf(i) = attribute_name(i:i)
        enddo
        name_buf(buf_len+1) = c_null_char

        !Get a pointer to the attribute value.  If the attribute is a string,
        !make an interoperable local copy and null terminate it.
        val_ptr = c_null_ptr
        if (present(rval)) then
            val_ptr = c_loc(rval)
            num_vals = num_values
        elseif (present(ival)) then
            val_ptr = c_loc(ival)
            num_vals = num_values
        elseif (present(cval)) then
            allocate(text_buf(num_values+1))
            do i = 1,num_values
                text_buf(i) = cval(i:i)
            enddo
            num_vals = num_values + 1
            text_buf(num_vals) = c_null_char
            val_ptr = c_loc(text_buf)
        endif

        !Write out the attribute.
        attribute_index = mpp_io_c_write_netcdf_attribute(f, &
                                                          variable_index, &
                                                          type_in_file, &
                                                          num_vals, &
                                                          name_buf, &
                                                          val_ptr, &
                                                          type_in_mem, &
                                                          .false._c_bool)

        !Deallocate local allocatables and nullify pointers.
        deallocate(name_buf)
        if (allocated(text_buf)) then
            deallocate(text_buf)
        endif
        val_ptr = c_null_ptr
    endif

end subroutine mpp_write_attribute_metadata
