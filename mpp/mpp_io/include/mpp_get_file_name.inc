function mpp_get_file_name(unit)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=256) :: mpp_get_file_name

    call openmp_thread_trap()
    mpp_get_file_name = mpp_get_file_name_(context, &
                                           unit)

end function mpp_get_file_name

function mpp_get_file_name_(context, &
                            file_index)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    character(len=256) :: mpp_get_file_name_

    !Local variables
    integer(kind=c_int) :: f
    type(c_ptr) :: name_ptr
    integer(kind=c_size_t) :: name_len
    character(kind=c_char,len=1),dimension(:),pointer :: f_char_ptr
    integer(kind=c_size_t) :: i

    !Make sure that the module has been initialized and that a valid
    !file index was passed in.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)

    if (context%files(file_index)%is_reader .or. &
            context%files(file_index)%is_writer) then

        !Get a pointer to the name.
        name_ptr = c_null_ptr
        if (context%files(file_index)%file_type .eq. MPP_NETCDF) then
            name_ptr = mpp_io_c_get_netcdf_file_name(f)
        else
            name_ptr = mpp_io_c_get_regular_file_name(f)
        endif
        name_len = mpp_io_c_strlen(name_ptr)

        !Make sure that the file name is not too long.
        if (name_len .gt. len(mpp_get_file_name_,kind=c_size_t)) then
            call mpp_error(FATAL, &
                           "mpp_get_file_name_: file name is longer than" &
                               //" inputted buffer.")
        endif

        !Convert to a fortran pointer.
        f_char_ptr => null()
        call c_f_pointer(name_ptr, &
                         f_char_ptr, &
                         [name_len])
        name_ptr = c_null_ptr

        !Copy the file name.
        do i = 1,name_len
            mpp_get_file_name_(i:i) = f_char_ptr(i)
        enddo
        f_char_ptr => null()
    else
        mpp_get_file_name_ = ""
    endif

end function mpp_get_file_name_

function mpp_get_file_unit(file_name, &
                           file_type)
    implicit none

    !Inputs/outputs
    character(len=*),intent(in) :: file_name
    integer(kind=c_int),intent(in) :: file_type
    integer(kind=c_int) :: mpp_get_file_unit

    call openmp_thread_trap()
    mpp_get_file_unit = mpp_get_file_unit_(context, &
                                           file_name, &
                                           file_type)

end function mpp_get_file_unit

function mpp_get_file_unit_(context, &
                            file_name, &
                            file_type)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    character(len=*),intent(in) :: file_name
    integer(kind=c_int),intent(in) :: file_type
    integer(kind=c_int) :: mpp_get_file_unit_

    !Local variables
    integer(kind=c_int) :: fname_length
    integer(kind=c_int) :: i
    character(kind=c_char,len=1),dimension(:),allocatable :: c_fname
    integer(kind=c_int) :: c_file_index

    call check_is_init()

    !Copy the file name into a buffer that is interoperable with c.
    fname_length = len_trim(file_name)
    allocate(c_fname(fname_length+1))
    do i = 1,fname_length
        c_fname(i) = file_name(i:i)
    enddo
    c_fname(fname_length+1) = c_null_char

    c_file_index = mpp_io_c_get_file_index(c_fname, &
                                           file_type)

    mpp_get_file_unit_ = convert_c_file_index(context, &
                                              file_type, &
                                              c_file_index)
    deallocate(c_fname)

end function mpp_get_file_unit_
