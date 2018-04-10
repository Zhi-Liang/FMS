 !*****************************************************************************
 !mpp_alltoall interface which binds to C.

#if !defined(_GNU_FBINDINGS) && !defined(_INTEL16_FBINDINGS)
 !-----------------------------------------------------------------------------
 !mpp_alltoall
  interface
      subroutine iso_c_mpp_alltoall(send_data,send_data_size,recv_data, &
                                    recv_data_size,size_per_send, &
                                    size_per_recv,mpp_type,pelist, &
                                    pelist_size,mpp_context) &
      bind(C,name='mpp_c_alltoall_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_ptr
        implicit none
        type(c_ptr),value,intent(in)               :: send_data
        integer(c_size_t),value,intent(in)         :: send_data_size
        type(c_ptr)                                :: recv_data
        integer(c_size_t),value,intent(in)         :: recv_data_size
        integer(c_size_t),value,intent(in)         :: size_per_send
        integer(c_size_t),value,intent(in)         :: size_per_recv
        integer(c_int32_t),value,intent(in)        :: mpp_type
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        type(c_ptr)                                :: mpp_context
      end subroutine iso_c_mpp_alltoall
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
