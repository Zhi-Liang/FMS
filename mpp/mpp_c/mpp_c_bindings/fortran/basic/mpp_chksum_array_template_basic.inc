 !-----------------------------------------------------------------------------
 !>mpp_chksum template for array data.
  function _A_NAME(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int32_t,c_int8_t,c_size_t _C_TYPE

   !<Inputs/Outputs
    _MPP_TYPE,_DIMENSION,intent(in)                     :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    _MPP_TYPE,intent(in),optional                       :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    _MPP_TYPE                       :: null_mask_val
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(chksum_data)) then
!       call mpp_error(NOTE,'_A_NAME: data is not contiguous')
!       chksum_val = 0
!       return
!   endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(_NUM_BYTES,kind=c_size_t)

    null_mask_val = 0
    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = _A_SUB(chksum_data,size(chksum_data), &
                                num_bytes,mask_flag,mask_val, &
                                size(pelist),pelist)
        else
            chksum_val = _A_SUB(chksum_data,size(chksum_data), &
                                num_bytes,mask_flag,null_mask_val, &
                                size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = _A_SUB(chksum_data,size(chksum_data), &
                                num_bytes,mask_flag,mask_val, &
                                1,null_pelist)
        else
            chksum_val = _A_SUB(chksum_data,size(chksum_data), &
                                num_bytes,mask_flag,null_mask_val, &
                                1,null_pelist)
        endif
    endif

  end function _A_NAME

 !-----------------------------------------------------------------------------

