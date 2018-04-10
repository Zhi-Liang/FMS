 !*****************************************************************************
 !mpp_gather interface which binds to C.

#if !defined(_GNU_FBINDINGS) && !defined(_INTEL16_FBINDINGS)
 !-----------------------------------------------------------------------------
 !mpp_gather
  interface
      subroutine iso_c_mpp_gather(send_data,send_len,recv_data,recv_len, &
                                  mpp_type,pelist,pelist_size, &
                                  mpp_context) &
      bind(C,name='mpp_c_gather_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_ptr
        implicit none
        type(c_ptr),value,intent(in)               :: send_data
        integer(c_size_t),value,intent(in)         :: send_len
        type(c_ptr)                                :: recv_data
        integer(c_size_t),value,intent(in)         :: recv_len
        integer(c_int32_t),value,intent(in)        :: mpp_type
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        type(c_ptr)                                :: mpp_context
      end subroutine iso_c_mpp_gather
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
