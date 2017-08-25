!Initialize a mpp_io context.
subroutine mpp_io_context_init(context, &
                               num_netcdf_files, &
                               num_regular_files, &
                               num_hdf5_files)
    implicit none

    !Inputs/Outputs.
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: num_netcdf_files
    integer(kind=c_int),intent(in) :: num_regular_files
    integer(kind=c_int),intent(in) :: num_hdf5_files

    !Local variables
    integer(kind=c_int) :: i
    integer(kind=c_int) :: num_files

    if (allocated(context%files)) then
        call mpp_error(FATAL, &
                       "mpp_io_context_init:  the inputted context has" &
                           //" already been initialized.")
    endif

    num_files = num_netcdf_files + num_regular_files + num_hdf5_files
    if (num_files .le. 0 .or. num_files .gt. MPP_IO_MAX_NUM_FILES) then
        call mpp_error(FATAL, &
                       "mpp_io_context_init: the inputted number of files" &
                           //" must be > 0 and <= MPP_IO_MAX_NUM_FILES.")
    endif

    allocate(context%files(num_files))
    context%num_netcdf_files = num_netcdf_files
    context%num_regular_files = num_regular_files
    context%num_hdf5_files = num_hdf5_files
    do i = 1,num_files
        context%files(i)%file_type = -1
        context%files(i)%rank_list_root = -1
        context%files(i)%domain => null()
        context%files(i)%io_domain => null()
        context%files(i)%domain_UG => null()
        context%files(i)%io_domain_UG => null()
    enddo

end subroutine mpp_io_context_init

subroutine mpp_io_context_destroy(context)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context

    !Local variables
    integer(kind=c_int) :: array_size
    integer(kind=c_int) :: i
    integer(kind=c_int) :: j

    if (.not. allocated(context%files)) then
        call mpp_error(FATAL, &
                       "mpp_io_context_destroy: the inputted context must" &
                           //" be initialized by calling mpp_io_context_init")
    endif

    array_size = size(context%files)
    do i = 1,array_size
        if (allocated(context%files(i)%rank_list)) then
            deallocate(context%files(i)%rank_list)
        endif
        if (associated(context%files(i)%domain)) then
            context%files(i)%domain => null()
        endif
        if (associated(context%files(i)%io_domain)) then
            context%files(i)%io_domain => null()
        endif
        if (associated(context%files(i)%domain_UG)) then
            context%files(i)%domain_UG => null()
        endif
        if (associated(context%files(i)%io_domain_UG)) then
            context%files(i)%io_domain_UG => null()
        endif
        if (allocated(context%files(i)%global_att_indices)) then
            deallocate(context%files(i)%global_att_indices)
        endif
        if (allocated(context%files(i)%dim_indices)) then
            deallocate(context%files(i)%dim_indices)
        endif
        if (allocated(context%files(i)%vars)) then
            do j = 1,size(context%files(i)%vars)
                if (allocated(context%files(i)%vars(j)%att_indices)) then
                    deallocate(context%files(i)%vars(j)%att_indices)
                endif
            enddo
            deallocate(context%files(i)%vars)
        endif
    enddo
    deallocate(context%files)

end subroutine mpp_io_context_destroy
