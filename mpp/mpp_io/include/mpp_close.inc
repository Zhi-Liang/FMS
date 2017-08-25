!Close a file.
subroutine mpp_close(unit, &
                     action)
    implicit none

    !Inputs/outputs
    integer(kind=c_int),intent(in) :: unit
    integer(kind=c_int),intent(in),optional :: action

    call openmp_thread_trap()
    call mpp_close_(context, &
                    unit, &
                    action)

end subroutine mpp_close

subroutine mpp_close_(context, &
                      file_index, &
                      file_action)
    implicit none

    !Inputs/outputs
    type(mpp_io_context_type),intent(inout) :: context
    integer(kind=c_int),intent(in) :: file_index
    integer(kind=c_int),intent(in),optional :: file_action

    !Local variables
    integer(kind=c_int) :: f
    integer(kind=c_int) :: action_flag
    integer(kind=c_int) :: j

    !Make sure that the module has been initialized and that a valid
    !file index was passed in.
    call check_is_init()
    f = check_file_index(context, &
                         file_index)

    !Deallocate context arrays and nullify context pointers.
    if (allocated(context%files(file_index)%rank_list)) then
        deallocate(context%files(file_index)%rank_list)
    endif
    if (associated(context%files(file_index)%domain)) then
        context%files(file_index)%domain => null()
    endif
    if (associated(context%files(file_index)%io_domain)) then
        context%files(file_index)%io_domain => null()
    endif
    if (associated(context%files(file_index)%domain_UG)) then
        context%files(file_index)%domain_UG => null()
    endif
    if (associated(context%files(file_index)%io_domain_UG)) then
        context%files(file_index)%io_domain_UG => null()
    endif
    if (allocated(context%files(file_index)%global_att_indices)) then
        deallocate(context%files(file_index)%global_att_indices)
    endif
    if (allocated(context%files(file_index)%dim_indices)) then
        deallocate(context%files(file_index)%dim_indices)
    endif
    if (allocated(context%files(file_index)%vars)) then
        do j = 1,size(context%files(file_index)%vars)
            if (allocated(context%files(file_index)%vars(j)%att_indices)) then
                deallocate(context%files(file_index)%vars(j)%att_indices)
            endif
        enddo
        deallocate(context%files(file_index)%vars)
    endif

    !Close the file.
    if (context%files(file_index)%is_reader .or. &
            context%files(file_index)%is_writer) then

        !Determine which action should be taken on the file.  The default
        !value is to keep the file.
        if (present(file_action)) then
            action_flag = file_action
        else
            action_flag = MPP_IO_C_KEEP_FILE
        endif

        if (context%files(file_index)%file_type .eq. MPP_NETCDF) then
            call mpp_io_c_close_netcdf_file(f, &
                                            action_flag, &
                                            .true._c_bool)
        elseif (context%files(file_index)%file_type .eq. MPP_ASCII) then
            call mpp_io_c_close_regular_file(f, &
                                             action_flag, &
                                             .true._c_bool)
        endif
    endif

end subroutine mpp_close_
