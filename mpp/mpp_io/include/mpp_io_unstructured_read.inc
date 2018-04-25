!Read in "compressed" (in the land sense) data.
subroutine mpp_io_unstructured_read_r1d(unit, &
                                        field, &
                                        domain, &
                                        data, &
                                        tindex, &
                                        start, &
                                        nread, &
                                        threading)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(domainUG),intent(in) :: domain
    real(kind=c_double),dimension(:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex
    integer(kind=c_int),dimension(:),intent(in),optional :: start
    integer(kind=c_int),dimension(:),intent(in),optional :: nread
    integer(kind=c_int),intent(in),optional :: threading

    !Local variables
    integer(kind=c_size_t),dimension(1) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_io_unstructured_read_array(context, &
                                        unit, &
                                        field, &
                                        domain, &
                                        data, &
                                        data_sizes, &
                                        tindex, &
                                        start, &
                                        nread, &
                                        threading)

end subroutine mpp_io_unstructured_read_r1d

subroutine mpp_io_unstructured_read_r2d(unit, &
                                        field, &
                                        domain, &
                                        data, &
                                        tindex, &
                                        start, &
                                        nread, &
                                        threading)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(domainUG),intent(in) :: domain
    real(kind=c_double),dimension(:,:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex
    integer(kind=c_int),dimension(:),intent(in),optional :: start
    integer(kind=c_int),dimension(:),intent(in),optional :: nread
    integer(kind=c_int),intent(in),optional :: threading

    !Local variables
    integer(kind=c_size_t),dimension(2) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_io_unstructured_read_array(context, &
                                        unit, &
                                        field, &
                                        domain, &
                                        data, &
                                        data_sizes, &
                                        tindex, &
                                        start, &
                                        nread, &
                                        threading)

end subroutine mpp_io_unstructured_read_r2d

subroutine mpp_io_unstructured_read_r3d(unit, &
                                        field, &
                                        domain, &
                                        data, &
                                        tindex, &
                                        start, &
                                        nread, &
                                        threading)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    type(fieldtype),intent(in) :: field
    type(domainUG),intent(in) :: domain
    real(kind=c_double),dimension(:,:,:),intent(inout) :: data
    integer(kind=c_int),intent(in),optional :: tindex
    integer(kind=c_int),dimension(:),intent(in),optional :: start
    integer(kind=c_int),dimension(:),intent(in),optional :: nread
    integer(kind=c_int),intent(in),optional :: threading

    !Local variables
    integer(kind=c_size_t),dimension(3) :: data_sizes

    call openmp_thread_trap()
    data_sizes = shape(data,kind=c_size_t)
    call mpp_io_unstructured_read_array(context, &
                                        unit, &
                                        field, &
                                        domain, &
                                        data, &
                                        data_sizes, &
                                        tindex, &
                                        start, &
                                        nread, &
                                        threading)

end subroutine mpp_io_unstructured_read_r3d

subroutine mpp_io_unstructured_read_array(context, &
                                          file_index, &
                                          field, &
                                          domain, &
                                          var_data, &
                                          data_sizes, &
                                          unlim_dim_level, &
                                          corner_indices, &
                                          edge_lengths, &
                                          threading)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    type(fieldtype),intent(in) :: field
    type(domainUG),intent(in) :: domain
    real(kind=c_double),dimension(*),intent(inout) :: var_data
    integer(kind=c_size_t),dimension(:),intent(inout) :: data_sizes
    integer(kind=c_int),intent(in),optional :: unlim_dim_level
    integer(kind=c_int),dimension(:),intent(in),optional :: corner_indices
    integer(kind=c_int),dimension(:),intent(in),optional :: edge_lengths
    integer(kind=c_int),intent(in),optional :: threading

    !Local variables
    integer(kind=c_int) :: total_data_size
    integer(kind=c_int) :: i
    logical(kind=c_bool) :: all_read
    type(domainUG) :: io_domain
    integer(kind=c_int) :: io_domain_npes
    integer(kind=c_int),dimension(:),allocatable :: pelist
    real(kind=c_double),dimension(:),allocatable :: buf

    !Make sure that the module has been initialized, that a valid
    !file index was passed in, and that the input file is a netcdf file.
    call check_is_init()
    call check_file_index(context, &
                          file_index)
    call check_is_netcdf_file(context, &
                              file_index)
    call domainUG_check(context, &
                        file_index, &
                        domain)

    total_data_size = 1
    do i = 1,size(data_sizes)
        total_data_size = total_data_size*int(data_sizes(i),kind=c_int)
    enddo

    if (total_data_size .gt. 0) then

        !Zero out the data inorder to get accurate checksums.
        do i = 1,total_data_size
            var_data(i) = 0.0
        enddo

        !Determine if all ranks will read, or only the root ranks for
        !the I/O domain pelists.
        all_read = .false._c_bool
        if (present(threading)) then
            if (threading .eq. MPP_MULTI) then
                all_read = .true._c_bool
            elseif (threading .ne. MPP_SINGLE) then
                call mpp_error(FATAL, &
                               "mpp_io_unstructured_read_array: threading should" &
                                   //" be MPP_SINGLE or MPP_MULTI.")
            endif
        endif

        if (all_read) then

            !All ranks attempt to read from the file.  Make sure that have
            !opened it first.
            if (.not. context%files(file_index)%is_reader) then
                call mpp_error(FATAL, &
                               "mpp_io_unstructured_read_array: the current rank" &
                                   //" is not allowed to read because it" &
                                   //" never opened the file.")
            endif

            call mpp_read_r_array(context, &
                                  file_index, &
                                  field, &
                                  var_data, &
                                  data_sizes, &
                                  .false._c_bool, &
                                  unlim_dim_level, &
                                  corner_indices, &
                                  edge_lengths)
        else

            !Get the I/O domain pelist.
            io_domain = NULL_DOMAINUG
            io_domain = mpp_get_UG_io_domain(domain)
            if (.not. mpp_domainUG_is_defined(io_domain)) then
                call mpp_error(FATAL, &
                               "mpp_io_unstructured_read_array: io_domain must" &
                                   //" be defined.")
            endif
            io_domain_npes = mpp_get_UG_domain_npes(io_domain)
            allocate(pelist(io_domain_npes))
            call mpp_get_UG_domain_pelist(io_domain, &
                                          pelist)

            !Local copy required by send/receive.
            allocate(buf(total_data_size))
            do i = 1,total_data_size
                buf(i) = var_data(i)
            enddo

            if (mpp_pe() .eq. pelist(1)) then

                !Have the pelist root read in the data and sent it to the
                !the other ranks on the pelist.
                if (.not. context%files(file_index)%is_reader) then
                    call mpp_error(FATAL, &
                                   "mpp_io_unstructured_read_array: the current rank" &
                                       //" is not allowed to read because it" &
                                       //" never opened the file.")
                endif

                call mpp_read_r_array(context, &
                                      file_index, &
                                      field, &
                                      buf, &
                                      data_sizes, &
                                      .false._c_bool, &
                                      unlim_dim_level, &
                                      corner_indices, &
                                      edge_lengths)

                do i = 2,io_domain_npes
                    call mpp_send(buf, &
                                  total_data_size, &
                                  pelist(i), &
                                  tag=COMM_TAG_1)
                enddo
                call mpp_sync_self()
            else

                !Have the non-root ranks of the pelist receive the data from
                !the root rank.
!               if (context%files(file_index)%is_reader) then
!                   call mpp_error(FATAL, &
!                                  "mpp_io_unstructured_read_array: only the root" &
!                                      //" rank is allowed to read from" &
!                                      //" the file.")
!               endif

                call mpp_recv(buf, &
                              total_data_size, &
                              pelist(1), &
                              block=.false., &
                              tag=COMM_TAG_1)
                call mpp_sync_self(check=EVENT_RECV)
            endif

            !Copy values back.
            do i = 1,total_data_size
                var_data(i) = buf(i)
            enddo
            deallocate(buf)

            deallocate(pelist)
        endif
    endif

end subroutine mpp_io_unstructured_read_array
