 !*****************************************************************************

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !mpp_send scalar interface which binds to C.
  interface
      subroutine iso_c_mpp_send_scalar(put_data,put_len,to_pe,mpp_type, &
                                       tag,request,mpp_context) &
      bind(C,name='mpp_c_send_scalar_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_int,c_ptr
        implicit none
        type(*),intent(inout)               :: put_data
        integer(c_size_t),value,intent(in)  :: put_len
        integer(c_int32_t),value,intent(in) :: to_pe
        integer(c_int32_t),value,intent(in) :: mpp_type
        integer(c_int32_t),value,intent(in) :: tag
        integer(c_int),intent(inout)        :: request
        type(c_ptr)                         :: mpp_context
      end subroutine iso_c_mpp_send_scalar
  end interface

  interface
      subroutine iso_c_mpp_send_array(put_data,put_len,to_pe,mpp_type, &
                                      tag,request,mpp_context) &
      bind(C,name='mpp_c_send_array_wrap')
        use iso_c_binding, only: c_size_t,c_int32_t,c_int,c_ptr
        implicit none
        type(*),dimension(*),intent(inout)  :: put_data
        integer(c_size_t),value,intent(in)  :: put_len
        integer(c_int32_t),value,intent(in) :: to_pe
        integer(c_int32_t),value,intent(in) :: mpp_type
        integer(c_int32_t),value,intent(in) :: tag
        integer(c_int),intent(inout)        :: request
        type(c_ptr)                         :: mpp_context
      end subroutine iso_c_mpp_send_array
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************