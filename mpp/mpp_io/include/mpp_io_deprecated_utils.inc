subroutine mpp_get_iospec(unit, &
                          iospec)

    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    character(len=*),intent(inout) :: iospec

    call mpp_error(FATAL, &
                   "mpp_get_iospec: this routine is no longer supported.")

end subroutine mpp_get_iospec

function mpp_get_ncid(unit) result(ncid)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    integer(kind=c_int) :: ncid

    call mpp_error(FATAL, &
                   "mpp_get_ncid: this routine is no longer supported.")

end function mpp_get_ncid

subroutine mpp_io_set_stack_size(n)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: n

    call mpp_error(FATAL, &
                   "mpp_io_set_stack_size: this routine is no longer" &
                       //" supported.")

end subroutine mpp_io_set_stack_size

subroutine netcdf_err(err, &
                      file, &
                      axis, &
                      field, &
                      attr, &
                      string)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: err
    type(filetype),optional :: file
    type(axistype),optional :: axis
    type(fieldtype),optional :: field
    type(atttype),optional :: attr
    character(len=*),optional :: string

    call mpp_error(FATAL, &
                   "netcdf_err: this routine is no longer supported.")

end subroutine netcdf_err

subroutine mpp_get_valid(f, &
                         v)
    implicit none

    !Inputs/outputs
    type(fieldtype),intent(in) :: f
    type(validtype),intent(out) :: v

    call mpp_error(FATAL, &
                   "mpp_get_valid: this routine is no longer supported.")

end subroutine mpp_get_valid

subroutine mpp_copy_meta_axis(unit, &
                              axis, &
                              domain)

    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(axistype),intent(inout) :: axis
    type(domain1D),intent(in),optional :: domain

    call mpp_error(FATAL, &
                   "mpp_copy_meta_axis: this routine is no longer supported.")

end subroutine mpp_copy_meta_axis

subroutine mpp_copy_meta_field(unit, &
                               field, &
                               axes)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(inout) :: field
    type(axistype),dimension(:),intent(in),optional :: axes

    call mpp_error(FATAL, &
                   "mpp_copy_meta_field: this routine is no longer supported.")

end subroutine mpp_copy_meta_field

subroutine mpp_copy_meta_global(unit, &
                                gatt)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(atttype),intent(in) :: gatt

    call mpp_error(FATAL, &
                   "mpp_copy_meta_global: this routine is no longer supported.")

end subroutine mpp_copy_meta_global

subroutine mpp_set_unit_range(unit_begin_in, &
                              unit_end_in)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit_begin_in
    integer(kind=c_int),intent(in) :: unit_end_in

    call mpp_error(FATAL, &
                   "mpp_set_unit_range: this routine is no longer supported.")

end subroutine mpp_set_unit_range

subroutine mpp_get_unit_range(unit_begin_out, &
                              unit_end_out)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(out) :: unit_begin_out
    integer(kind=c_int),intent(out) :: unit_end_out

    call mpp_error(FATAL, &
                   "mpp_get_unit_range: this routine is no longer supported.")

end subroutine mpp_get_unit_range

function mpp_get_axis_index(axes, &
                            axisname)
    implicit none

    !Inputs/outputs
    type(axistype),dimension(:),intent(in) :: axes
    character(len=*),intent(in) :: axisname
    integer(kind=c_int) :: mpp_get_axis_index

    call mpp_error(FATAL, &
                   "mpp_get_axis_index: this routine is no longer supported.")

end function  mpp_get_axis_index

function mpp_get_axis_length(axis)
    implicit none

    !Inputs/outputs
    type(axistype),intent(in) :: axis
    integer(kind=c_int) :: mpp_get_axis_length

    call mpp_error(FATAL, &
                   "mpp_get_axis_length: this routine is no longer supported.")

end function mpp_get_axis_length

function mpp_get_recdimid(unit)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    integer(kind=c_int) :: mpp_get_recdimid

    call mpp_error(FATAL, &
                   "mpp_get_recdimid: this routine is no longer supported.")

end function mpp_get_recdimid

subroutine mpp_write_axis_data(unit, &
                               axes)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(axistype),dimension(:),intent(in) :: axes

    call mpp_error(FATAL, &
                   "mpp_write_axis_data: this routine is no longer supported.")

end subroutine mpp_write_axis_data
