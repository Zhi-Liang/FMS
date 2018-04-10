 !*****************************************************************************
 !mpp_max interface which binds to C.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !mpp_max
  interface
      subroutine iso_c_mpp_max_scalar(max_data,max_len,mpp_type,pelist, &
                                      pelist_size,mpp_context) &
      bind(C,name='mpp_c_max_scalar_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_ptr
        implicit none
        type(*),intent(inout)                      :: max_data
        integer(c_size_t),value,intent(in)         :: max_len
        integer(c_int32_t),value,intent(in)        :: mpp_type
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        type(c_ptr)                                :: mpp_context
      end subroutine iso_c_mpp_max_scalar
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
