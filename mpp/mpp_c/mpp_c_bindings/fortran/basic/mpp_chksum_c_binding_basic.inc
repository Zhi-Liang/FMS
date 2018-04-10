 !*****************************************************************************
 !mpp_chksum interface which binds to C.

#if !defined(_GNU_FBINDINGS) && !defined(_INTEL16_FBINDINGS)
 !-----------------------------------------------------------------------------
 !mpp_chksum
  interface
      function iso_c_mpp_chksum(chksum_data,chksum_len,num_bytes,mask_flag, &
                                mask_val,pelist,pelist_size,mpp_context) &
      result(chksum_val) &
      bind(C,name='mpp_c_chksum_wrap')
        use iso_c_binding, only: c_size_t,c_int8_t,c_int32_t,c_ptr,c_int64_t
        implicit none
        type(c_ptr),value,intent(in)               :: chksum_data
        integer(c_size_t),value,intent(in)         :: chksum_len
        integer(c_size_t),value,intent(in)         :: num_bytes
        integer(c_int8_t),value,intent(in)         :: mask_flag
        type(c_ptr),value,intent(in)               :: mask_val
        integer(c_int32_t),dimension(*),intent(in) :: pelist
        integer(c_size_t),value,intent(in)         :: pelist_size
        type(c_ptr)                                :: mpp_context
        integer(c_int64_t)                         :: chksum_val
      end function iso_c_mpp_chksum
   end interface

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
