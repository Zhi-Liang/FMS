function mpp_get_axis_by_name(unit, &
                              axisname) result(axis)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(in) :: axisname
    type(axistype) :: axis

    call openmp_thread_trap()
    axis = mpp_get_axis_by_name_(context, &
                                 unit, &
                                 axisname)

end function mpp_get_axis_by_name

function mpp_get_axis_by_name_(context, &
                               file_index, &
                               axisname) result(out_axis)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    character(len=*),intent(in) :: axisname
    type(axistype) :: out_axis

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: name_len
    character(kind=c_char,len=1),dimension(:),allocatable,target :: name_buf
    integer(kind=c_int) :: i
    type(c_ptr) :: ptr
    integer(kind=c_int) :: dim_index

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)

    !Set default values.
    out_axis = default_axis

    !Copy the dimension name into a buffer that is interoperable with c.
    name_len = len_trim(axisname)
    allocate(name_buf(name_len+1))
    do i = 1,name_len
        name_buf(i) = axisname(i:i)
    enddo
    name_buf(name_len+1) = c_null_char
    ptr = c_null_ptr
    ptr = c_loc(name_buf)

    !Get the index of the dimension.
    dim_index = mpp_io_c_get_netcdf_dimension_index(f, &
                                                    ptr)
    ptr = c_null_ptr
    deallocate(name_buf)

    !Fill in the axis values.
    call fill_axistype_from_index(context, &
                                  file_index, &
                                  dim_index, &
                                  out_axis)

end function mpp_get_axis_by_name_
