 !*****************************************************************************
 !Public facing mpp_alltoallv routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for one-dimensional data.
  subroutine mpp_alltoallv_1D(send_data,send_len,send_displace,recv_data, &
                              recv_len,recv_displace,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:),intent(in)                   :: send_data
    integer(INT_KIND),dimension(:),intent(in)          :: send_len
    integer(INT_KIND),dimension(:),intent(in)          :: send_displace
    class(*),dimension(:),intent(inout)                :: recv_data
    integer(INT_KIND),dimension(:),intent(in)          :: recv_len
    integer(INT_KIND),dimension(:),intent(in)          :: recv_displace
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_ALLTOALLV_1D: data is not contiguous.')
!   endif

   !<Set the mpp_type value.
    select type (send_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_ALLTOALLV_1D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_alltoallv_c_style_array(send_data,size(send_len),send_len, &
                                         size(send_displace),send_displace, &
                                         recv_data,size(recv_len),recv_len, &
                                         size(recv_displace),recv_displace, &
                                         mpp_type,size(pelist),pelist)

    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoallv_c_style_array(send_data,size(send_len),send_len, &
                                         size(send_displace),send_displace, &
                                         recv_data,size(recv_len),recv_len, &
                                         size(recv_displace),recv_displace, &
                                         mpp_type,1,null_pelist)
    endif

  end subroutine mpp_alltoallv_1D

 !-----------------------------------------------------------------------------
 !>Perform variable alltoall communication with all ranks on the pelist.
  subroutine mpp_alltoallv_c_style_array(send_data,sln,send_len,sdn,send_displace, &
                                         recv_data,rln,recv_len,rdn,recv_displace, &
                                         mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(*),intent(in)             :: send_data
    integer(INT_KIND),intent(in)                :: sln
    integer(INT_KIND),dimension(sln),intent(in) :: send_len
    integer(INT_KIND),intent(in)                :: sdn
    integer(INT_KIND),dimension(sdn),intent(in) :: send_displace
    type(*),dimension(*),intent(inout)          :: recv_data
    integer(INT_KIND),intent(in)                :: rln
    integer(INT_KIND),dimension(rln),intent(in) :: recv_len
    integer(INT_KIND),intent(in)                :: rdn
    integer(INT_KIND),dimension(rdn),intent(in) :: recv_displace
    integer(c_int32_t),intent(in)               :: mpp_type
    integer(INT_KIND),intent(in)                :: n
    integer(INT_KIND),dimension(n),intent(in)   :: pelist

    call iso_c_mpp_alltoallv_array(send_data,send_len,send_displace,recv_data, &
                                   recv_len,recv_displace,mpp_type,pelist, &
                                   int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_alltoallv_c_style_array

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
