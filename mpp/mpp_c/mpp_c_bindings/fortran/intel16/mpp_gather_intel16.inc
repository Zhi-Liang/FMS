 !*****************************************************************************
 !Public facing mpp_gather routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer one-dimensional data.
  subroutine mpp_gather_i32_1D(send_data,recv_data,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(:),intent(in)          :: send_data
    integer(c_int32_t),dimension(:),intent(inout)       :: recv_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_GATHER_I32_ID: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_GATHER_I32_ID: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_gather_c_style(send_data,size(send_data),recv_data, &
                                size(recv_data),MPP_INT32,size(pelist), &
                                pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_gather_c_style(send_data,size(send_data),recv_data, &
                                size(recv_data),MPP_INT32,1, &
                                null_pelist)
    endif

  end subroutine mpp_gather_i32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real one-dimensional data.
  subroutine mpp_gather_r32_1D(send_data,recv_data,pelist)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),dimension(:),intent(in)               :: send_data
    real(c_float),dimension(:),intent(inout)            :: recv_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_GATHER_R32_ID: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_GATHER_R32_ID: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_gather_c_style(send_data,size(send_data),recv_data, &
                                size(recv_data),MPP_REAL32,size(pelist), &
                                pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_gather_c_style(send_data,size(send_data),recv_data, &
                                size(recv_data),MPP_REAL32,1, &
                                null_pelist)
    endif

  end subroutine mpp_gather_r32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real one-dimensional data.
  subroutine mpp_gather_r64_1D(send_data,recv_data,pelist)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),dimension(:),intent(in)              :: send_data
    real(c_double),dimension(:),intent(inout)           :: recv_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_GATHER_R64_ID: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_GATHER_R64_ID: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_gather_c_style(send_data,size(send_data),recv_data, &
                                size(recv_data),MPP_REAL64,size(pelist), &
                                pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_gather_c_style(send_data,size(send_data),recv_data, &
                                size(recv_data),MPP_REAL64,1, &
                                null_pelist)
    endif

  end subroutine mpp_gather_r64_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit logical one-dimensional data.
  subroutine mpp_gather_l32_1D(send_data,recv_data,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    logical(INT_KIND),dimension(:),intent(in)           :: send_data
    logical(INT_KIND),dimension(:),intent(inout)        :: recv_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_GATHER_L32_ID: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_GATHER_L32_ID: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_gather_c_style(send_data,size(send_data),recv_data, &
                                size(recv_data),MPP_INT32,size(pelist), &
                                pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_gather_c_style(send_data,size(send_data),recv_data, &
                                size(recv_data),MPP_INT32,1, &
                                null_pelist)
    endif

  end subroutine mpp_gather_l32_1D

 !-----------------------------------------------------------------------------
 !>Gather data across all ranks on a pelist.
  subroutine mpp_gather_c_style(send_data,send_len,recv_data,recv_len, &
                                mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(..),intent(in)           :: send_data
    integer(INT_KIND),intent(in)               :: send_len
    type(*),dimension(..),intent(inout)        :: recv_data
    integer(INT_KIND),intent(in)               :: recv_len
    integer(c_int32_t),intent(in)              :: mpp_type
    integer(INT_KIND),intent(in)               :: n
    integer(c_int32_t),dimension(n),intent(in) :: pelist

    call iso_c_mpp_gather(send_data,int(send_len,kind=c_size_t), &
                          recv_data,int(recv_len,kind=c_size_t), &
                          mpp_type,pelist,int(n,kind=c_size_t), &
                          mpp_context)

  end subroutine mpp_gather_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
