 !-----------------------------------------------------------------------------
 !>mpp_chksum template for scalar data.
  function _S_NAME(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int32_t,c_int8_t,c_size_t,c_loc, &
                             c_null_ptr _C_TYPE

   !<Inputs/Outputs
    _MPP_TYPE,target,intent(in)                         :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    _MPP_TYPE,target,intent(in),optional                :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(_NUM_BYTES,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(c_loc(chksum_data),1, &
                                            num_bytes,mask_flag,c_loc(mask_val), &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(c_loc(chksum_data),1, &
                                            num_bytes,mask_flag,c_null_ptr, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(c_loc(chksum_data),1, &
                                            num_bytes,mask_flag,c_loc(mask_val), &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(c_loc(chksum_data),1, &
                                            num_bytes,mask_flag,c_null_ptr, &
                                            1,null_pelist)
        endif
    endif

  end function _S_NAME

 !-----------------------------------------------------------------------------

