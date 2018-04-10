 !*****************************************************************************

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !mpp_recv interface which binds to C.
  interface
      subroutine iso_c_mpp_recv_scalar(get_data,get_len,from_pe,mpp_type, &
                                       tag,request,block,mpp_context) &
      bind(C,name='mpp_c_recv_scalar_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_int,c_ptr
        implicit none
        type(*),intent(inout)               :: get_data
        integer(c_size_t),value,intent(in)  :: get_len
        integer(c_int32_t),value,intent(in) :: from_pe
        integer(c_int32_t),value,intent(in) :: mpp_type
        integer(c_int32_t),value,intent(in) :: tag
        integer(c_int),intent(inout)        :: request
        integer(c_int32_t),value,intent(in) :: block
        type(c_ptr)                         :: mpp_context
      end subroutine iso_c_mpp_recv_scalar
   end interface

  interface
      subroutine iso_c_mpp_recv_array(get_data,get_len,from_pe,mpp_type, &
                                      tag,request,block,mpp_context) &
      bind(C,name='mpp_c_recv_array_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_int,c_ptr
        implicit none
        type(*),dimension(*),intent(inout)  :: get_data
        integer(c_size_t),value,intent(in)  :: get_len
        integer(c_int32_t),value,intent(in) :: from_pe
        integer(c_int32_t),value,intent(in) :: mpp_type
        integer(c_int32_t),value,intent(in) :: tag
        integer(c_int),intent(inout)        :: request
        integer(c_int32_t),value,intent(in) :: block
        type(c_ptr)                         :: mpp_context
      end subroutine iso_c_mpp_recv_array
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
