 !*****************************************************************************
 !Public facing mpp_alltoallv routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer one-dimensional data.
  subroutine mpp_alltoallv_i32_1D(send_data,send_len,send_displace,recv_data, &
                                  recv_len,recv_displace,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(:),intent(in)          :: send_data
    integer(c_int32_t),dimension(:),intent(in)          :: send_len
    integer(c_int32_t),dimension(:),intent(in)          :: send_displace
    integer(c_int32_t),dimension(:),intent(inout)       :: recv_data
    integer(c_int32_t),dimension(:),intent(in)          :: recv_len
    integer(c_int32_t),dimension(:),intent(in)          :: recv_displace
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALLV_I32_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALLV_I32_1D: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_alltoallv_c_style(send_data,size(send_len),send_len, &
                                   size(send_displace),send_displace, &
                                   recv_data,size(recv_len),recv_len, &
                                   size(recv_displace),recv_displace, &
                                   MPP_INT32,size(pelist),pelist)

    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoallv_c_style(send_data,size(send_len),send_len, &
                                   size(send_displace),send_displace, &
                                   recv_data,size(recv_len),recv_len, &
                                   size(recv_displace),recv_displace, &
                                   MPP_INT32,1,null_pelist)
    endif

  end subroutine mpp_alltoallv_i32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer one-dimensional data.
  subroutine mpp_alltoallv_i64_1D(send_data,send_len,send_displace,recv_data, &
                                  recv_len,recv_displace,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(:),intent(in)          :: send_data
    integer(c_int32_t),dimension(:),intent(in)          :: send_len
    integer(c_int32_t),dimension(:),intent(in)          :: send_displace
    integer(c_int64_t),dimension(:),intent(inout)       :: recv_data
    integer(c_int32_t),dimension(:),intent(in)          :: recv_len
    integer(c_int32_t),dimension(:),intent(in)          :: recv_displace
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALLV_I64_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALLV_I64_1D: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_alltoallv_c_style(send_data,size(send_len),send_len, &
                                   size(send_displace),send_displace, &
                                   recv_data,size(recv_len),recv_len, &
                                   size(recv_displace),recv_displace, &
                                   MPP_INT64,size(pelist),pelist)

    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoallv_c_style(send_data,size(send_len),send_len, &
                                   size(send_displace),send_displace, &
                                   recv_data,size(recv_len),recv_len, &
                                   size(recv_displace),recv_displace, &
                                   MPP_INT64,1,null_pelist)
    endif

  end subroutine mpp_alltoallv_i64_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real one-dimensional data.
  subroutine mpp_alltoallv_r32_1D(send_data,send_len,send_displace,recv_data, &
                                  recv_len,recv_displace,pelist)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),dimension(:),intent(in)               :: send_data
    integer(c_int32_t),dimension(:),intent(in)          :: send_len
    integer(c_int32_t),dimension(:),intent(in)          :: send_displace
    real(c_float),dimension(:),intent(inout)            :: recv_data
    integer(c_int32_t),dimension(:),intent(in)          :: recv_len
    integer(c_int32_t),dimension(:),intent(in)          :: recv_displace
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALLV_R32_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALLV_R32_1D: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_alltoallv_c_style(send_data,size(send_len),send_len, &
                                   size(send_displace),send_displace, &
                                   recv_data,size(recv_len),recv_len, &
                                   size(recv_displace),recv_displace, &
                                   MPP_REAL32,size(pelist),pelist)

    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoallv_c_style(send_data,size(send_len),send_len, &
                                   size(send_displace),send_displace, &
                                   recv_data,size(recv_len),recv_len, &
                                   size(recv_displace),recv_displace, &
                                   MPP_REAL32,1,null_pelist)
    endif

  end subroutine mpp_alltoallv_r32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real one-dimensional data.
  subroutine mpp_alltoallv_r64_1D(send_data,send_len,send_displace,recv_data, &
                                  recv_len,recv_displace,pelist)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),dimension(:),intent(in)              :: send_data
    integer(c_int32_t),dimension(:),intent(in)          :: send_len
    integer(c_int32_t),dimension(:),intent(in)          :: send_displace
    real(c_double),dimension(:),intent(inout)           :: recv_data
    integer(c_int32_t),dimension(:),intent(in)          :: recv_len
    integer(c_int32_t),dimension(:),intent(in)          :: recv_displace
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(send_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALLV_R64_1D: data is not contiguous.')
    endif
    if (.not. is_contiguous(recv_data)) then
        call mpp_error(FATAL,'MPP_ALLTOALLV_R64_1D: data is not contiguous.')
    endif

    if (present(pelist)) then
        call mpp_alltoallv_c_style(send_data,size(send_len),send_len, &
                                   size(send_displace),send_displace, &
                                   recv_data,size(recv_len),recv_len, &
                                   size(recv_displace),recv_displace, &
                                   MPP_REAL64,size(pelist),pelist)

    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoallv_c_style(send_data,size(send_len),send_len, &
                                   size(send_displace),send_displace, &
                                   recv_data,size(recv_len),recv_len, &
                                   size(recv_displace),recv_displace, &
                                   MPP_REAL64,1,null_pelist)
    endif

  end subroutine mpp_alltoallv_r64_1D

 !-----------------------------------------------------------------------------
 !>Perform variable alltoall communication with all ranks on the pelist.
  subroutine mpp_alltoallv_c_style(send_data,sln,send_len,sdn,send_displace, &
                                   recv_data,rln,recv_len,rdn,recv_displace, &
                                   mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(..),intent(in)             :: send_data
    integer(INT_KIND),intent(in)                 :: sln
    integer(c_int32_t),dimension(sln),intent(in) :: send_len
    integer(INT_KIND),intent(in)                 :: sdn
    integer(c_int32_t),dimension(sdn),intent(in) :: send_displace
    type(*),dimension(..),intent(inout)          :: recv_data
    integer(INT_KIND),intent(in)                 :: rln
    integer(c_int32_t),dimension(rln),intent(in) :: recv_len
    integer(INT_KIND),intent(in)                 :: rdn
    integer(c_int32_t),dimension(rdn),intent(in) :: recv_displace
    integer(c_int32_t),intent(in)                :: mpp_type
    integer(INT_KIND),intent(in)                 :: n
    integer(c_int32_t),dimension(n),intent(in)   :: pelist

    call iso_c_mpp_alltoallv(send_data,send_len,send_displace,recv_data, &
                             recv_len,recv_displace,mpp_type,pelist, &
                             int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_alltoallv_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
