 !*****************************************************************************
 !mpp_alltoallv interface which binds to C.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !mpp_alltoallv
  interface
      subroutine iso_c_mpp_alltoallv(send_data,send_len,send_displace, &
                                     recv_data,recv_len,recv_displace, &
                                     mpp_type,pelist,pelist_size,mpp_context) &
      bind(C,name='mpp_c_alltoallv_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_ptr
        implicit none
        type(*),dimension(..),intent(in)           :: send_data
        integer(c_int32_t),dimension(*),intent(in) :: send_len
        integer(c_int32_t),dimension(*),intent(in) :: send_displace
        type(*),dimension(..),intent(inout)        :: recv_data
        integer(c_int32_t),dimension(*),intent(in) :: recv_len
        integer(c_int32_t),dimension(*),intent(in) :: recv_displace
        integer(c_int32_t),value,intent(in)        :: mpp_type
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        type(c_ptr)                                :: mpp_context
      end subroutine iso_c_mpp_alltoallv
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
