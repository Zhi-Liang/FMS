 !*****************************************************************************
 !Public facing mpp_alltoall routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer one-dimensional data.
  subroutine mpp_alltoall_i32_1D(send_data,send_len,recv_data,recv_len,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(:),intent(in)          :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    integer(c_int32_t),dimension(:),intent(inout)       :: recv_data
    integer(INT_KIND),intent(in)                        :: recv_len
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALL_I32_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALL_I32_1D: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_alltoall_c_style(send_data,size(send_data),recv_data, &
                                  size(recv_data),send_len,recv_len, &
                                  MPP_INT32,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoall_c_style(send_data,size(send_data),recv_data, &
                                  size(recv_data),send_len,recv_len, &
                                  MPP_INT32,1,null_pelist)
    endif

  end subroutine mpp_alltoall_i32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer one-dimensional data.
  subroutine mpp_alltoall_i64_1D(send_data,send_len,recv_data,recv_len,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(:),intent(in)          :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    integer(c_int64_t),dimension(:),intent(inout)       :: recv_data
    integer(INT_KIND),intent(in)                        :: recv_len
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALL_I64_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALL_I64_1D: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_alltoall_c_style(send_data,size(send_data),recv_data, &
                                  size(recv_data),send_len,recv_len, &
                                  MPP_INT64,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoall_c_style(send_data,size(send_data),recv_data, &
                                  size(recv_data),send_len,recv_len, &
                                  MPP_INT64,1,null_pelist)
    endif

  end subroutine mpp_alltoall_i64_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real one-dimensional data.
  subroutine mpp_alltoall_r32_1D(send_data,send_len,recv_data,recv_len,pelist)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),dimension(:),intent(in)               :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    real(c_float),dimension(:),intent(inout)            :: recv_data
    integer(INT_KIND),intent(in)                        :: recv_len
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALL_R32_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALL_R32_1D: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_alltoall_c_style(send_data,size(send_data),recv_data, &
                                  size(recv_data),send_len,recv_len, &
                                  MPP_REAL32,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoall_c_style(send_data,size(send_data),recv_data, &
                                  size(recv_data),send_len,recv_len, &
                                  MPP_REAL32,1,null_pelist)
    endif

  end subroutine mpp_alltoall_r32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real one-dimensional data.
  subroutine mpp_alltoall_r64_1D(send_data,send_len,recv_data,recv_len,pelist)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),dimension(:),intent(in)              :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    real(c_double),dimension(:),intent(inout)           :: recv_data
    integer(INT_KIND),intent(in)                        :: recv_len
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALL_R64_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALL_R64_1D: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_alltoall_c_style(send_data,size(send_data),recv_data, &
                                  size(recv_data),send_len,recv_len, &
                                  MPP_REAL64,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoall_c_style(send_data,size(send_data),recv_data, &
                                  size(recv_data),send_len,recv_len, &
                                  MPP_REAL64,1,null_pelist)
    endif

  end subroutine mpp_alltoall_r64_1D

 !-----------------------------------------------------------------------------
 !>Perform alltoall communication with all ranks on the pelist.
  subroutine mpp_alltoall_c_style(send_data,send_data_size,recv_data, &
                                  recv_data_size,send_len,recv_len, &
                                  mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(..),intent(in)           :: send_data
    integer(INT_KIND),intent(in)               :: send_data_size
    type(*),dimension(..),intent(inout)        :: recv_data
    integer(INT_KIND),intent(in)               :: recv_data_size
    integer(INT_KIND)                          :: send_len
    integer(INT_KIND)                          :: recv_len
    integer(c_int32_t),intent(in)              :: mpp_type
    integer(INT_KIND),intent(in)               :: n
    integer(c_int32_t),dimension(n),intent(in) :: pelist

    call iso_c_mpp_alltoall(send_data,int(send_data_size,kind=c_size_t), &
                            recv_data,int(recv_data_size,kind=c_size_t), &
                            int(send_len,kind=c_size_t), &
                            int(recv_len,kind=c_size_t),mpp_type, &
                            pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_alltoall_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
