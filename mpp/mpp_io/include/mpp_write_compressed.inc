!Write out "compressed" (in the land sense) data.
subroutine mpp_write_compressed_r1d(unit, &
                                    field, &
                                    domain, &
                                    data, &
                                    nelems_io, &
                                    tstamp, &
                                    default_data)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(inout) :: field
    type(domain2D),intent(inout) :: domain
    real(kind=c_double),dimension(:),intent(inout) :: data
    integer(kind=c_int),dimension(:),intent(in) :: nelems_io
    real(kind=c_double),intent(in),optional :: tstamp
    real(kind=c_double),intent(in),optional :: default_data

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
                                    nelems_io, &
                                    tstamp, &
                                    default_data)

end subroutine mpp_write_compressed_r1d

subroutine mpp_write_compressed_r2d(unit, &
                                    field, &
                                    domain, &
                                    data, &
                                    nelems_io, &
                                    tstamp, &
                                    default_data)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(inout) :: field
    type(domain2D),intent(inout) :: domain
    real(kind=c_double),dimension(:,:),intent(inout) :: data
    integer(kind=c_int),dimension(:),intent(in) :: nelems_io
    real(kind=c_double),intent(in),optional :: tstamp
    real(kind=c_double),intent(in),optional :: default_data

    !Local variables
    integer(kind=c_size_t),dimension(2) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_write_compressed_array(context, &
                                    unit, &
                                    field, &
                                    domain, &
                                    data, &
                                    data_sizes, &
                                    nelems_io, &
                                    tstamp, &
                                    default_data)

end subroutine mpp_write_compressed_r2d

subroutine mpp_write_compressed_r3d(unit, &
                                    field, &
                                    domain, &
                                    data, &
                                    nelems_io, &
                                    tstamp, &
                                    default_data)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(inout) :: field
    type(domain2D),intent(inout) :: domain
    real(kind=c_double),dimension(:,:,:),intent(inout) :: data
    integer(kind=c_int),dimension(:),intent(in) :: nelems_io
    real(kind=c_double),intent(in),optional :: tstamp
    real(kind=c_double),intent(in),optional :: default_data

    !Local variables
    integer(kind=c_size_t),dimension(3) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_write_compressed_array(context, &
                                    unit, &
                                    field, &
                                    domain, &
                                    data, &
                                    data_sizes, &
                                    nelems_io, &
                                    tstamp, &
                                    default_data)

end subroutine mpp_write_compressed_r3d

subroutine mpp_write_compressed_array(context, &
                                      file_index, &
                                      field, &
                                      domain, &
                                      var_data, &
                                      data_sizes, &
                                      nelems_io, &
                                      unlim_dim_val, &
                                      default_data)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(inout) :: field
    type(domain2D),intent(inout) :: domain
    real(kind=c_double),dimension(*),intent(inout) :: var_data
    integer(kind=c_size_t),dimension(:),intent(inout) :: data_sizes
    integer(kind=c_int),dimension(:),intent(in) :: nelems_io
    real(kind=c_double),intent(in),optional :: unlim_dim_val
    real(kind=c_double),intent(in),optional :: default_data

    !Local variables
    integer(kind=c_int) :: f
    type(domain2d),pointer :: io_domain_ptr
    real(kind=c_double) :: fill
    integer(kind=c_int) :: num_ranks
    integer(kind=c_int),dimension(:),allocatable :: pelist
    integer(kind=c_int) :: num_vals
    integer(kind=c_int) :: i
    integer(kind=c_int),dimension(:),allocatable :: buf
    integer(kind=c_int) :: nelems
    real(kind=c_double),dimension(:),allocatable :: rbuf
    integer(kind=c_int) :: var_data_size
    real(kind=c_double),dimension(:),allocatable :: sbuf
    real(kind=c_double),dimension(:),allocatable :: rbuf2
    integer(kind=c_int) :: j
    integer(kind=c_int) :: k
    integer(kind=c_int) :: offset_out
    integer(kind=c_int) :: offset_in

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the inputted file is a netcdf file.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)
    call check_is_netcdf_file(context, &
                              file_index)
    call domain2d_check(context, &
                        file_index, &
                        domain)

    !If the previous subroutine call doesn't error, then the inputted domain
    !should be the same as the one stored in the context.  From now on, we
    !will use the domain stored in the context instead of the one that was
    !passed in.  This is done so that we can remove the need to pass in a
    !domain in the future.
    io_domain_ptr => null()
    io_domain_ptr => context%files(file_index)%io_domain

    !Set the fill value.
    fill = 0
    if (present(default_data)) then
        fill = default_data
    endif

    !Make sure that the I/O domain for this file is not null.
    if (.not. associated(io_domain_ptr)) then
        call mpp_error(FATAL, &
                       "mpp_write_compressed: the io_domain corresponding" &
                           //" to this file must be defined.")
    endif

    !Get the pelist associated with the I/O domain.
    num_ranks = mpp_get_domain_npes(io_domain_ptr)
    allocate(pelist(num_ranks))
    call mpp_get_pelist(io_domain_ptr, &
                        pelist)

    !Make sure that only the root rank of the pelist will write to the file.
    !Currently data is only gathered on the pelist's first rank.
    if (mpp_pe() .eq. pelist(1) .and. &
            .not. context%files(file_index)%is_writer) then
        call mpp_error(FATAL, &
                       "mpp_write_compressed: the root rank of the pelist" &
                           //" must be allowed to write to the file.")
    endif
    if (mpp_pe() .ne. pelist(1) .and. context%files(file_index)%is_writer) then
        call mpp_error(FATAL, &
                       "mpp_write_compressed: a non-root rank of the pelist" &
                           //" is not allowed to write to the file.")
    endif

    !Check that the number of elements in every dimension except the first
    !dimension of the data is consistent across all rank participating in
    !the gather.
    num_vals = 1
    if (size(data_sizes) .gt. 1) then
        do i = 2,size(data_sizes)
            num_vals = num_vals*int(data_sizes(i),kind=c_int)
        enddo
        allocate(buf(num_ranks))
        call mpp_gather((/num_vals/), &
                        buf, &
                        pelist)
        if (context%files(file_index)%is_writer) then
            if (maxloc(buf,1) .ne. minloc(buf,1)) then
                call mpp_error(FATAL, &
                               "mpp_write_compressed: the number of elements" &
                                   //"along the non-first dimensions of the" &
                                   //" inputted data is not consistent" &
                                   //" across all ranks in the I/O domain.")
            endif
        endif
        deallocate(buf)
    endif

    !Allocate an array which will be used to gather the data on the root
    !rank of the pelist.
    if (context%files(file_index)%is_writer) then

        !Get the total number of points in the fastest-changing dimension.
        !For example, if rank 0 has 7 points and rank 1 has 15 points in
        !the fastest changing dimension, then nelems will be 22.
        nelems = sum(nelems_io)

        !Get space for the buffer.
        allocate(rbuf(nelems*num_vals))
    else
        allocate(rbuf(1))
    endif

    !Perform the gather of the data onto the root rank.
    var_data_size = int(sum(data_sizes),kind=c_int)
    allocate(sbuf(var_data_size))
    do i = 1,var_data_size
        sbuf(i) = var_data(i)
    enddo
    call mpp_gather(sbuf, &
                    var_data_size, &
                    rbuf, &
                    nelems_io*num_vals, &
                    pelist)
    deallocate(sbuf)

    !Reorder the data.  The data comes out of the gather with each
    !rank's data stored contiguously, which is not how the data will
    !be written.  Reorder so that the gathered data fits the same
    !form as the inputted data (i.e. if the inputted data is stored as
    !(dim1,dim2,dim3), reorder the gathered data so that it is also of
    !the form (dim1,dim2,dim3).
    if (context%files(file_index)%is_writer) then
        allocate(rbuf2(nelems*num_vals))
        rbuf2 = fill
        if (size(data_sizes) .eq. 1) then
            do i = 1,size(rbuf2)
                rbuf2(i) = rbuf(i)
            enddo
        else
            do j = 1,num_vals
                offset_out = 0
                do k = 1,num_ranks
                    if (k .gt. 1) then
                        offset_in = (j-1)*nelems_io(k) + num_vals*(sum(nelems_io(1:k-1)))
                    else
                        offset_in = (j-1)*nelems_io(k)
                    endif
                    do i = 1,nelems_io(k)
                        rbuf2((j-1)*nelems+offset_out+i) = rbuf(offset_in+i)
                    enddo
                    offset_out = offset_out + nelems_io(k)
                enddo
            enddo
        endif
        data_sizes(1) = nelems
        call mpp_write_r_array(context, &
                               file_index, &
                               field, &
                               rbuf2, &
                               data_sizes, &
                               .false._c_bool, &
                               unlim_dim_val)
        deallocate(rbuf2)
    endif
    deallocate(pelist)
    deallocate(rbuf)

end subroutine mpp_write_compressed_array
