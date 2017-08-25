!Seems to be just a different interface to mpp_write_compressed.
subroutine mpp_write_unlimited_axis_r1d(unit, &
                                        field, &
                                        domain, &
                                        data, &
                                        nelems_io)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(inout) :: field
    type(domain2D),intent(inout) :: domain
    real(kind=c_double),dimension(:),intent(inout) :: data
    integer(kind=c_int),dimension(:),intent(in) :: nelems_io

    !Local variables
    integer(kind=c_size_t),dimension(1) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_write_compressed_array(context, &
                                    unit, &
                                    field, &
                                    domain, &
                                    data, &
                                    data_sizes, &
                                    nelems_io)

end subroutine mpp_write_unlimited_axis_r1d
