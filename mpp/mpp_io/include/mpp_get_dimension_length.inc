function mpp_get_dimension_length(unit, &
                                  dimname, &
                                  found)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(in) :: dimname
    logical,intent(out),optional :: found
    integer(kind=c_int) :: mpp_get_dimension_length

    call openmp_thread_trap()
    mpp_get_dimension_length = mpp_get_dimension_length_(context, &
                                                         unit, &
                                                         dimname, &
                                                         found)

end function mpp_get_dimension_length

function mpp_get_dimension_length_(context, &
                                   file_index, &
                                   dim_name, &
                                   dim_found)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    character(len=*),intent(in) :: dim_name
    logical,intent(out),optional :: dim_found
    integer(kind=c_int) :: mpp_get_dimension_length_

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: name_len
    character(kind=c_char,len=1),dimension(:),allocatable,target :: name_buf
    integer(kind=c_int) :: i
    integer(kind=c_int) :: dim_index

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    if (context%files(file_index)%is_reader .or. &
            context%files(file_index)%is_writer) then

        !Place the name in an interoperable buffer.
        name_len = len(dim_name)
        allocate(name_buf(name_len+1))
        do i = 1,name_len
            name_buf(i) = dim_name(i:i)
        enddo
        name_buf(name_len+1) = c_null_char

        !Look for the dimension index.
        dim_index = mpp_io_c_get_netcdf_dimension_index(f, &
                                                        c_loc(name_buf))
        deallocate(name_buf)

        if (dim_index .ne. MPP_IO_C_INDEX_NOT_FOUND) then
            mpp_get_dimension_length_ = int(mpp_io_c_get_netcdf_dimension_length(f, &
                                                                                 dim_index),kind=c_int)
            if (present(dim_found)) then
                dim_found = .true.
            endif
        else
            mpp_get_dimension_length_ = -1
            if (present(dim_found)) then
                dim_found = .false.
            endif
        endif
    else
        mpp_get_dimension_length_ = -1
        if (present(dim_found)) then
            dim_found = .false.
        endif
    endif

end function mpp_get_dimension_length_
