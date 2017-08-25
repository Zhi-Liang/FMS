subroutine mpp_get_field_att_text(unit, &
                                  fieldname, &
                                  attname, &
                                  attvalue)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(in) :: fieldname
    character(len=*),intent(in) :: attname
    character(len=*),intent(out) :: attvalue

    call openmp_thread_trap()
    call mpp_get_field_att_text_(context, &
                                 unit, &
                                 fieldname, &
                                 attname, &
                                 attvalue)

end subroutine mpp_get_field_att_text

subroutine mpp_get_field_att_text_(context, &
                                   file_index, &
                                   fieldname, &
                                   attname, &
                                   attvalue)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    character(len=*),intent(in) :: fieldname
    character(len=*),intent(in) :: attname
    character(len=*),intent(out) :: attvalue

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: name_len
    character(kind=c_char,len=1),dimension(:),allocatable,target :: name_buf
    integer(kind=c_int) :: i
    type(c_ptr) :: ptr
    integer(kind=c_int) :: var_index
    integer(kind=c_int) :: att_index
    type(atttype) :: att

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Place the inputted netcdf variable name in a buffer that is
    !interoperable with c.
    name_len = len(fieldname)
    allocate(name_buf(name_len+1))
    do i = 1,name_len
        name_buf(i) = fieldname(i:i)
    enddo
    name_buf(name_len+1) = c_null_char
    ptr = c_null_ptr
    ptr = c_loc(name_buf)

    !Get the index of the netcdf variable.
    var_index = mpp_io_c_get_netcdf_variable_index(f, &
                                                   ptr)
    ptr = c_null_ptr
    deallocate(name_buf)

    !Place the inputted netcdf attribute name in a buffer that is
    !interoperable with c.
    name_len = len(attname)
    allocate(name_buf(name_len+1))
    do i = 1,name_len
        name_buf(i) = attname(i:i)
    enddo
    name_buf(name_len+1) = c_null_char
    ptr = c_null_ptr
    ptr = c_loc(name_buf)

    !Get the index of the attribute.
    att_index = mpp_io_c_get_netcdf_attribute_index(f, &
                                                    var_index, &
                                                    ptr)
    ptr = c_null_ptr
    deallocate(name_buf)

    !Get the attribute values.
    call fill_atttype_from_index(context, &
                                 file_index, &
                                 var_index, &
                                 att_index, &
                                 att)

    !Make sure that the attribute is a character attribute.
    if (att%type .ne. MPP_IO_C_NC_CHAR) then
        call mpp_error(FATAL, &
                       "mpp_get_field_att_text_: the attribute is not" &
                          //" a character attribute.")
    endif

    !Make sure the inputted string is long enough.
    if (len(attvalue) .lt. att%len) then
        call mpp_error(FATAL, &
                       "mpp_get_field_att_text_: inputted string is too" &
                          //" short.")
    endif

    !Pass the string value.
    attvalue = trim(att%catt)

    !Clean up.
    call destroy_atttype(att)

end subroutine mpp_get_field_att_text_
