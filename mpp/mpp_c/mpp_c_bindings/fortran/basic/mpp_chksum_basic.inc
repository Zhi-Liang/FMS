 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  function mpp_chksum_c_style_i32a(chksum_data,chksum_len,num_bytes,mask_flag, &
                                   mask_val,n,pelist) result(chksum_val)
    use iso_c_binding, only: c_int32_t,c_size_t,c_int8_t,c_loc

   !<Inputs/Outputs
    integer(c_int32_t),dimension(*),target,intent(in) :: chksum_data
    integer(INT_KIND),intent(in)                      :: chksum_len
    integer(c_size_t),intent(in)                      :: num_bytes
    integer(c_int8_t),intent(in)                      :: mask_flag
    integer(c_int32_t),target,intent(in)              :: mask_val
    integer(INT_KIND),intent(in)                      :: n
    integer(INT_KIND),dimension(n),intent(in)         :: pelist
    integer(LONG_KIND)                                :: chksum_val

    chksum_val = mpp_chksum_c_style(c_loc(chksum_data),chksum_len, &
                                    num_bytes,mask_flag, &
                                    c_loc(mask_val),n,pelist)

  end function mpp_chksum_c_style_i32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  function mpp_chksum_c_style_i64a(chksum_data,chksum_len,num_bytes,mask_flag, &
                                   mask_val,n,pelist) result(chksum_val)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_size_t,c_int8_t,c_loc

   !<Inputs/Outputs
    integer(c_int64_t),dimension(*),target,intent(in) :: chksum_data
    integer(INT_KIND),intent(in)                      :: chksum_len
    integer(c_size_t),intent(in)                      :: num_bytes
    integer(c_int8_t),intent(in)                      :: mask_flag
    integer(c_int64_t),target,intent(in)              :: mask_val
    integer(INT_KIND),intent(in)                      :: n
    integer(INT_KIND),dimension(n),intent(in)         :: pelist
    integer(LONG_KIND)                                :: chksum_val

    chksum_val = mpp_chksum_c_style(c_loc(chksum_data),chksum_len, &
                                    num_bytes,mask_flag, &
                                    c_loc(mask_val),n,pelist)

  end function mpp_chksum_c_style_i64a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  function mpp_chksum_c_style_r32a(chksum_data,chksum_len,num_bytes,mask_flag, &
                                   mask_val,n,pelist) result(chksum_val)
    use iso_c_binding, only: c_float,c_int32_t,c_size_t,c_int8_t,c_loc

   !<Inputs/Outputs
    real(c_float),dimension(*),target,intent(in) :: chksum_data
    integer(INT_KIND),intent(in)                 :: chksum_len
    integer(c_size_t),intent(in)                 :: num_bytes
    integer(c_int8_t),intent(in)                 :: mask_flag
    real(c_float),target,intent(in)              :: mask_val
    integer(INT_KIND),intent(in)                 :: n
    integer(INT_KIND),dimension(n),intent(in)    :: pelist
    integer(LONG_KIND)                           :: chksum_val

    chksum_val = mpp_chksum_c_style(c_loc(chksum_data),chksum_len, &
                                    num_bytes,mask_flag, &
                                    c_loc(mask_val),n,pelist)

  end function mpp_chksum_c_style_r32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  function mpp_chksum_c_style_r64a(chksum_data,chksum_len,num_bytes,mask_flag, &
                                   mask_val,n,pelist) result(chksum_val)
    use iso_c_binding, only: c_double,c_int32_t,c_size_t,c_int8_t,c_loc

   !<Inputs/Outputs
    real(c_double),dimension(*),target,intent(in) :: chksum_data
    integer(INT_KIND),intent(in)                  :: chksum_len
    integer(c_size_t),intent(in)                  :: num_bytes
    integer(c_int8_t),intent(in)                  :: mask_flag
    real(c_double),target,intent(in)              :: mask_val
    integer(INT_KIND),intent(in)                  :: n
    integer(INT_KIND),dimension(n),intent(in)     :: pelist
    integer(LONG_KIND)                            :: chksum_val

    chksum_val = mpp_chksum_c_style(c_loc(chksum_data),chksum_len, &
                                    num_bytes,mask_flag, &
                                    c_loc(mask_val),n,pelist)

  end function mpp_chksum_c_style_r64a

 !-----------------------------------------------------------------------------
 !>Perform a check-sum across all ranks on a pelist.
  function mpp_chksum_c_style(chksum_data,chksum_len,num_bytes,mask_flag, &
                              mask_val,n,pelist) result(chksum_val)
    use iso_c_binding, only: c_size_t,c_int8_t

   !<Inputs/Outputs
    type(c_ptr),intent(in)                    :: chksum_data
    integer(INT_KIND),intent(in)              :: chksum_len
    integer(c_size_t),intent(in)              :: num_bytes
    integer(c_int8_t),intent(in)              :: mask_flag
    type(c_ptr),intent(in)                    :: mask_val
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist
    integer(LONG_KIND)                        :: chksum_val

    chksum_val = int(iso_c_mpp_chksum(chksum_data, &
                                      int(chksum_len,kind=c_size_t), &
                                      num_bytes,mask_flag,mask_val,pelist, &
                                      int(n,kind=c_size_t),mpp_context), &
                                      kind=LONG_KIND)

  end function mpp_chksum_c_style

 !-----------------------------------------------------------------------------

