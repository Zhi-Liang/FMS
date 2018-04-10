 !-----------------------------------------------------------------------------
 !>mpp_alltoall template for array data.
  subroutine _A_NAME(send_data,send_len,recv_data,recv_len,pelist)
    use iso_c_binding, only: c_int32_t _C_TYPE

   !<Inputs/Outputs
    _MPP_TYPE,_DIMENSION,intent(in)                     :: send_data
    integer(INT_KIND),intent(in)                        :: send_len
    _MPP_TYPE,_DIMENSION,intent(inout)                  :: recv_data
    integer(INT_KIND),intent(in)                        :: recv_len
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(send_data)) then
!       call mpp_error(FATAL,'_A_NAME: data is not contiguous.')
!   endif
!   if (.not. is_contiguous(recv_data)) then
!       call mpp_error(FATAL,'_A_NAME: data is not contiguous.')
!   endif

    if (present(pelist)) then
        call _A_SUB(send_data,size(send_data),recv_data, &
                    size(recv_data),send_len,recv_len, &
                    _MPP_PARAM,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call _A_SUB(send_data,size(send_data),recv_data, &
                    size(recv_data),send_len,recv_len, &
                    _MPP_PARAM,1,null_pelist)
    endif

  end subroutine _A_NAME

 !-----------------------------------------------------------------------------
