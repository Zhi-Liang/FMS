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
    integer(kind=c_int) :: num_files
    integer(kind=c_int) :: i

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
    do i = 1,num_files
        call parallel_io_type_reset(context%files(i))
    enddo
    context%num_netcdf_files = num_netcdf_files
    context%num_regular_files = num_regular_files
    context%num_hdf5_files = num_hdf5_files

end subroutine mpp_io_context_init

!Destroy a mpp_io context.
subroutine mpp_io_context_destroy(context)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context

    !Local variables
    integer(kind=c_int) :: i

    if (.not. allocated(context%files)) then
        call mpp_error(FATAL, &
                       "mpp_io_context_destroy: the input context must" &
                           //" be initialized by calling mpp_io_context_init.")
    endif

    do i = 1,size(context%files)
        call parallel_io_type_reset(context%files(i))
    enddo
    deallocate(context%files)

end subroutine mpp_io_context_destroy

!Reset a parallel_io_type.
subroutine parallel_io_type_reset(var)
    implicit none

    !Inputs/outputs
    type(parallel_io_type),intent(inout) :: var

    !Local variables
    integer(kind=c_int) :: i

    !Set initial values.
    var%file_type = -1
    if (allocated(var%rank_list)) then
        deallocate(var%rank_list)
    endif
    var%rank_list_root = -1
    var%domain = NULL_DOMAIN2D
    var%io_domain = NULL_DOMAIN2D
    var%domain_UG = NULL_DOMAINUG
    var%io_domain_UG = NULL_DOMAINUG
    var%is_writer = .false._c_bool
    var%is_reader = .false._c_bool
    var%data_in_multi_files = .false._c_bool
    var%comm_required = .false._c_bool
    var%num_ranks_at_init = -1
    if (allocated(var%global_att_indices)) then
        deallocate(var%global_att_indices)
    endif
    if (allocated(var%dim_indices)) then
        deallocate(var%dim_indices)
    endif
    if (allocated(var%vars)) then
        do i = 1,size(var%vars)
            call netcdf_var_type_reset(var%vars(i))
        enddo
        deallocate(var%vars)
    endif

end subroutine parallel_io_type_reset

!Reset a netcdf_var_type.
subroutine netcdf_var_type_reset(var)
    implicit none

    !Inputs/outputs
    type(netcdf_var_type),intent(inout) :: var

    var%var_index = -1
    if (allocated(var%att_indices)) then
        deallocate(var%att_indices)
    endif

end subroutine netcdf_var_type_reset
