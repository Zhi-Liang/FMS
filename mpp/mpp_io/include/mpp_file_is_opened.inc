function mpp_file_is_opened_i(unit)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    logical :: mpp_file_is_opened_i

    call openmp_thread_trap()
    mpp_file_is_opened_i = mpp_file_is_opened_i_(context, &
                                                 unit)

end function mpp_file_is_opened_i

function mpp_file_is_opened_i_(context, &
                               file_index)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    logical :: mpp_file_is_opened_i_

    !Local variables
    integer(kind=c_int) :: f

    !Make sure that the module has been initialized and that a valid
    !file index was passed in.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)

    if (context%files(file_index)%is_reader .or. &
            context%files(file_index)%is_writer) then

        if (context%files(file_index)%file_type .eq. MPP_NETCDF) then
            mpp_file_is_opened_i_ = mpp_io_c_get_netcdf_file_is_open(f)
        else
            mpp_file_is_opened_i_ = mpp_io_c_get_regular_file_is_open(f)
        endif

    else
        mpp_file_is_opened_i_ = .false.
    endif

end function mpp_file_is_opened_i_

function mpp_file_is_opened_a(file_name, &
                              file_type)
    implicit none

    !Inputs/outputs
    character(len=*),intent(in) :: file_name
    integer(kind=c_int),intent(in) :: file_type
    logical :: mpp_file_is_opened_a

    !Local variables
    character(kind=c_char,len=1),dimension(:),allocatable :: c_fname

    !Make sure that the module has been initialized and that a valid
    !file index was passed in.
    call check_is_init()

    !Copy the file name into a buffer that is interoperable with c.
    call f_string_to_c(file_name, &
                       c_fname)

    if (file_type .eq. MPP_NETCDF) then
        mpp_file_is_opened_a = mpp_io_c_get_netcdf_file_is_open(c_fname)
    elseif (file_type .eq. MPP_ASCII) then
        mpp_file_is_opened_a = mpp_io_c_get_regular_file_is_open(c_fname)
    else
        call mpp_error(FATAL, &
                       "mpp_file_is_opened_a: invalid file_type.  Must be" &
                           //" either MPP_NETCDF or MPP_ASCII.")
    endif
    deallocate(c_fname)

end function mpp_file_is_opened_a
