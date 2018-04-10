 !*****************************************************************************
 !Public facing mpp_alltoall routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for one-dimensional data.
  subroutine mpp_alltoall_1D(send_data,send_len,recv_data,recv_len,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:),intent(in)                   :: send_data
    integer(INT_KIND),intent(in)                       :: send_len
    class(*),dimension(:),intent(inout)                :: recv_data
    integer(INT_KIND),intent(in)                       :: recv_len
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_ALLTOALL_1D: data is not contiguous.')
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
        call mpp_error(FATAL,'MPP_ALLTOALL_1D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_alltoall_c_style_array(send_data,size(send_data),recv_data, &
                                        size(recv_data),send_len,recv_len, &
                                        mpp_type,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_alltoall_c_style_array(send_data,size(send_data),recv_data, &
                                        size(recv_data),send_len,recv_len, &
                                        mpp_type,1,null_pelist)
    endif

  end subroutine mpp_alltoall_1D

 !-----------------------------------------------------------------------------
 !>Perform alltoall communication with all ranks on the pelist.
  subroutine mpp_alltoall_c_style_array(send_data,send_data_size,recv_data, &
                                        recv_data_size,send_len,recv_len, &
                                        mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(*),intent(in)           :: send_data
    integer(INT_KIND),intent(in)              :: send_data_size
    type(*),dimension(*),intent(inout)        :: recv_data
    integer(INT_KIND),intent(in)              :: recv_data_size
    integer(INT_KIND)                         :: send_len
    integer(INT_KIND)                         :: recv_len
    integer(c_int32_t),intent(in)             :: mpp_type
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist

    call iso_c_mpp_alltoall_array(send_data,int(send_data_size,kind=c_size_t), &
                                  recv_data,int(recv_data_size,kind=c_size_t), &
                                  int(send_len,kind=c_size_t), &
                                  int(recv_len,kind=c_size_t),mpp_type, &
                                  pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_alltoall_c_style_array

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
