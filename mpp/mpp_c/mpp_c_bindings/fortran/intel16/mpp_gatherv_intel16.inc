 !*****************************************************************************
 !Public facing mpp_gatherv routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer one-dimensional data.
  subroutine mpp_gatherv_i32_1D(send_data,send_len,recv_data,recv_len_array, &
                                pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(:),intent(in)          :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    integer(c_int32_t),dimension(:),intent(inout)       :: recv_data
    integer(INT_KIND),dimension(:),intent(in)           :: recv_len_array
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1)            :: null_pelist
    integer(c_size_t),dimension(:),allocatable :: tmp_recv_len_array
    integer(INT_KIND)                          :: i

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_GATHERV_I32_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_GATHERV_I32_1D: data is not contiguous.')
    endif

   !<Convert the recv_len_array to the correct type.
    allocate(tmp_recv_len_array(size(recv_len_array)))
    do i = 1,size(recv_len_array)
        tmp_recv_len_array(i) = int(recv_len_array(i),kind=c_size_t)
    enddo

    if (present(pelist)) then
        call mpp_gatherv_c_style(send_data,send_len,recv_data, &
                                 size(recv_data),size(tmp_recv_len_array), &
                                 tmp_recv_len_array,MPP_INT32, &
                                 size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_gatherv_c_style(send_data,send_len,recv_data, &
                                 size(recv_data),size(tmp_recv_len_array), &
                                 tmp_recv_len_array,MPP_INT32, &
                                 1,null_pelist)
    endif

   !<Deallocate local allocatables.
    deallocate(tmp_recv_len_array)

  end subroutine mpp_gatherv_i32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real one-dimensional data.
  subroutine mpp_gatherv_r32_1D(send_data,send_len,recv_data,recv_len_array, &
                                pelist)
    use iso_c_binding, only: c_float,c_int32_t,c_size_t

   !<Inputs/Outputs
    real(c_float),dimension(:),intent(in)               :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    real(c_float),dimension(:),intent(inout)            :: recv_data
    integer(INT_KIND),dimension(:),intent(in)           :: recv_len_array
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1)            :: null_pelist
    integer(c_size_t),dimension(:),allocatable :: tmp_recv_len_array
    integer(INT_KIND)                          :: i

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_GATHERV_R64_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_GATHERV_R64_1D: data is not contiguous.')
    endif

   !<Convert the recv_len_array to the correct type.
    allocate(tmp_recv_len_array(size(recv_len_array)))
    do i = 1,size(recv_len_array)
        tmp_recv_len_array(i) = int(recv_len_array(i),kind=c_size_t)
    enddo

    if (present(pelist)) then
        call mpp_gatherv_c_style(send_data,send_len,recv_data, &
                                 size(recv_data),size(tmp_recv_len_array), &
                                 tmp_recv_len_array,MPP_REAL32, &
                                 size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_gatherv_c_style(send_data,send_len,recv_data, &
                                 size(recv_data),size(tmp_recv_len_array), &
                                 tmp_recv_len_array,MPP_REAL32, &
                                 1,null_pelist)
    endif

   !<Deallocate local allocatables.
    deallocate(tmp_recv_len_array)

  end subroutine mpp_gatherv_r32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real one-dimensional data.
  subroutine mpp_gatherv_r64_1D(send_data,send_len,recv_data,recv_len_array, &
                                pelist)
    use iso_c_binding, only: c_double,c_int32_t,c_size_t

   !<Inputs/Outputs
    real(c_double),dimension(:),intent(in)              :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    real(c_double),dimension(:),intent(inout)           :: recv_data
    integer(INT_KIND),dimension(:),intent(in)           :: recv_len_array
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1)            :: null_pelist
    integer(c_size_t),dimension(:),allocatable :: tmp_recv_len_array
    integer(INT_KIND)                          :: i

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_GATHERV_R64_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_GATHERV_R64_1D: data is not contiguous.')
    endif

   !<Convert the recv_len_array to the correct type.
    allocate(tmp_recv_len_array(size(recv_len_array)))
    do i = 1,size(recv_len_array)
        tmp_recv_len_array(i) = int(recv_len_array(i),kind=c_size_t)
    enddo

    if (present(pelist)) then
        call mpp_gatherv_c_style(send_data,send_len,recv_data, &
                                 size(recv_data),size(tmp_recv_len_array), &
                                 tmp_recv_len_array,MPP_REAL64, &
                                 size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_gatherv_c_style(send_data,send_len,recv_data, &
                                 size(recv_data),size(tmp_recv_len_array), &
                                 tmp_recv_len_array,MPP_REAL64, &
                                 1,null_pelist)
    endif

   !<Deallocate local allocatables.
    deallocate(tmp_recv_len_array)

  end subroutine mpp_gatherv_r64_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit logical one-dimensional data.
  subroutine mpp_gatherv_l32_1D(send_data,send_len,recv_data,recv_len_array, &
                                pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    logical(INT_KIND),dimension(:),intent(in)           :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    logical(INT_KIND),dimension(:),intent(inout)        :: recv_data
    integer(INT_KIND),dimension(:),intent(in)           :: recv_len_array
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1)            :: null_pelist
    integer(c_size_t),dimension(:),allocatable :: tmp_recv_len_array
    integer(INT_KIND)                          :: i

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_GATHERV_L32_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_GATHERV_L32_1D: data is not contiguous.')
    endif

   !<Convert the recv_len_array to the correct type.
    allocate(tmp_recv_len_array(size(recv_len_array)))
    do i = 1,size(recv_len_array)
        tmp_recv_len_array(i) = int(recv_len_array(i),kind=c_size_t)
    enddo

    if (present(pelist)) then
        call mpp_gatherv_c_style(send_data,send_len,recv_data, &
                                 size(recv_data),size(tmp_recv_len_array), &
                                 tmp_recv_len_array,MPP_INT32, &
                                 size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_gatherv_c_style(send_data,send_len,recv_data, &
                                 size(recv_data),size(tmp_recv_len_array), &
                                 tmp_recv_len_array,MPP_INT32, &
                                 1,null_pelist)
    endif

   !<Deallocate local allocatables.
    deallocate(tmp_recv_len_array)

  end subroutine mpp_gatherv_l32_1D

 !-----------------------------------------------------------------------------
 !>Gather data across all ranks on a pelist.
  subroutine mpp_gatherv_c_style(send_data,send_len,recv_data,recv_data_size, &
                                 recv_len_array_size,recv_len_array, &
                                 mpp_type,n,pelist)
    use iso_c_binding, only: c_size_t,c_int32_t

   !<Inputs/Outputs
    type(*),dimension(..),intent(in)                            :: send_data
    integer(INT_KIND),intent(in)                                :: send_len
    type(*),dimension(..),intent(inout)                         :: recv_data
    integer(INT_KIND),intent(in)                                :: recv_data_size
    integer(INT_KIND),intent(in)                                :: recv_len_array_size
    integer(c_size_t),dimension(recv_len_array_size),intent(in) :: recv_len_array
    integer(c_int32_t),intent(in)                               :: mpp_type
    integer(INT_KIND),intent(in)                                :: n
    integer(c_int32_t),dimension(n),intent(in)                  :: pelist

    call iso_c_mpp_gatherv(send_data,int(send_len,kind=c_size_t), &
                           recv_data,int(recv_data_size,kind=c_size_t), &
                           recv_len_array, &
                           int(recv_len_array_size,kind=c_size_t), &
                           mpp_type,pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_gatherv_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
