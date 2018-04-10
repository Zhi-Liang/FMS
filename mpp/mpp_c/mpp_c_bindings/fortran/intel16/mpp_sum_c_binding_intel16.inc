 !*****************************************************************************
 !mpp_sum interface which binds to C.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !mpp_sum
  interface
      subroutine iso_c_mpp_sum(sum_data,sum_len,mpp_type,pelist,pelist_size, &
                               mpp_context) &
      bind(C,name='mpp_c_sum_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_ptr
        implicit none
        type(*),dimension(..),intent(inout)        :: sum_data
        integer(c_size_t),value,intent(in)         :: sum_len
        integer(c_int32_t),value,intent(in)        :: mpp_type
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        type(c_ptr)                                :: mpp_context
      end subroutine iso_c_mpp_sum
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
