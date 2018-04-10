 !*****************************************************************************

#if !defined(_GNU_FBINDINGS) && !defined(_INTEL16_FBINDINGS)
 !-----------------------------------------------------------------------------
 !mpp_error interface which binds to C.
  interface
      subroutine iso_c_mpp_error(error_type,error_mesg,error_mesg_len, &
                                 mpp_context) &
      bind(C,name='mpp_c_error_wrap')
        use iso_c_binding, only: c_int32_t,c_char,c_size_t,c_ptr
        implicit none
        integer(c_int32_t),value,intent(in)            :: error_type
        character(kind=c_char),dimension(*),intent(in) :: error_mesg
        integer(c_size_t),value,intent(in)             :: error_mesg_len
        type(c_ptr),value,intent(in)                   :: mpp_context
      end subroutine iso_c_mpp_error
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
