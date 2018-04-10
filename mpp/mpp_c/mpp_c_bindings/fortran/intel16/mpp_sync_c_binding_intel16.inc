 !*****************************************************************************

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !mpp_sync
  interface
      subroutine iso_c_mpp_sync(pelist,pelist_size,mpp_context) &
      bind(C,name='mpp_c_sync_wrap')
        use iso_c_binding, only: c_int32_t,c_size_t,c_ptr
        implicit none
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        type(c_ptr)                                :: mpp_context
      end subroutine iso_c_mpp_sync
  end interface

 !-----------------------------------------------------------------------------
 !mpp_sync_self interface which binds to C.
  interface
      subroutine iso_c_mpp_sync_self(check,request,request_size,msg_size, &
                                    msg_type,mpp_context) &
      bind(C,name='mpp_c_sync_self_wrap')
        use iso_c_binding, only: c_int32_t,c_int,c_size_t,c_ptr
        implicit none
        integer(c_int32_t),value,intent(in)        :: check
        integer(c_int),dimension(*),intent(inout)  :: request
        integer(c_size_t),value,intent(in)         :: request_size
        integer(c_size_t),dimension(*),intent(in)  :: msg_size
        integer(c_int32_t),dimension(*),intent(in) :: msg_type
        type(c_ptr)                                :: mpp_context
      end subroutine iso_c_mpp_sync_self
  end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
