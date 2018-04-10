 !*****************************************************************************
 !Public facing mpp_chksum routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer scalar data.
  function mpp_chksum_i32s(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int32_t),intent(in)                       :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int32_t),intent(in),optional              :: mask_val
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
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i32s

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer one-dimensional data.
  function mpp_chksum_i32_1D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(:),intent(in)          :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int32_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I32_1D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer two-dimensional data.
  function mpp_chksum_i32_2D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(:,:),intent(in)        :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int32_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I32_2D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i32_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer three-dimensional data.
  function mpp_chksum_i32_3D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(:,:,:),intent(in)      :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int32_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I32_3D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i32_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer four-dimensional data.
  function mpp_chksum_i32_4D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(:,:,:,:),intent(in)    :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int32_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I32_4D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i32_4D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer five-dimensional data.
  function mpp_chksum_i32_5D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(:,:,:,:,:),intent(in)  :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int32_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I32_5D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i32_5D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer scalar data.
  function mpp_chksum_i64s(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int64_t),intent(in)                       :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int64_t),intent(in),optional              :: mask_val
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
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i64s

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer one-dimensional data.
  function mpp_chksum_i64_1D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(:),intent(in)          :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int64_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I64_1D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i64_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer two-dimensional data.
  function mpp_chksum_i64_2D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(:,:),intent(in)        :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int64_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I64_2D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i64_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer three-dimensional data.
  function mpp_chksum_i64_3D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(:,:,:),intent(in)      :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int64_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I64_3D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i64_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer four-dimensional data.
  function mpp_chksum_i64_4D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(:,:,:,:),intent(in)    :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int64_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I64_4D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i64_4D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer five-dimensional data.
  function mpp_chksum_i64_5D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(:,:,:,:,:),intent(in)  :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    integer(c_int64_t),intent(in),optional              :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_I64_5D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_i64_5D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real scalar data.
  function mpp_chksum_r32s(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_float,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_float),intent(in)                            :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_float),intent(in),optional                   :: mask_val
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
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r32s

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real one-dimensional data.
  function mpp_chksum_r32_1D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_float,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_float),dimension(:),intent(in)               :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_float),intent(in),optional                   :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_R32_1D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r32_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real two-dimensional data.
  function mpp_chksum_r32_2D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_float,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_float),dimension(:,:),intent(in)             :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_float),intent(in),optional                   :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_R32_2D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r32_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real three-dimensional data.
  function mpp_chksum_r32_3D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_float,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_float),dimension(:,:,:),intent(in)           :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_float),intent(in),optional                   :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_R32_3D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r32_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real four-dimensional data.
  function mpp_chksum_r32_4D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_float,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_float),dimension(:,:,:,:),intent(in)         :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_float),intent(in),optional                   :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_R32_4D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r32_4D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real five-dimensional data.
  function mpp_chksum_r32_5D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_float,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_float),dimension(:,:,:,:,:),intent(in)       :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_float),intent(in),optional                   :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_R32_5D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(4,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r32_5D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real scalar data.
  function mpp_chksum_r64s(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_double,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_double),intent(in)                           :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_double),intent(in),optional                  :: mask_val
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
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data,1, &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r64s

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real one-dimensional data.
  function mpp_chksum_r64_1D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_double,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_double),dimension(:),intent(in)              :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_double),intent(in),optional                  :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_R64_1D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r64_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real two-dimensional data.
  function mpp_chksum_r64_2D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_double,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_double),dimension(:,:),intent(in)             :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional  :: pelist
    real(c_double),intent(in),optional                   :: mask_val
    integer(LONG_KIND)                                   :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(NOTE,'MPP_CHKSUM_R64_2D: data is not contiguous')
        chksum_val = 0
        return
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r64_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real three-dimensional data.
  function mpp_chksum_r64_3D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_double,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_double),dimension(:,:,:),intent(in)          :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_double),intent(in),optional                  :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(NOTE,'MPP_CHKSUM_R64_3D: data is not contiguous')
        chksum_val = 0
        return
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r64_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real four-dimensional data.
  function mpp_chksum_r64_4D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_double,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_double),dimension(:,:,:,:),intent(in)        :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_double),intent(in),optional                  :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(NOTE,'MPP_CHKSUM_R64_4D: data is not contiguous')
        chksum_val = 0
        return
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r64_4D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real five-dimensional data.
  function mpp_chksum_r64_5D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_double,c_int32_t,c_int8_t,c_size_t

   !<Inputs/Outputs
    real(c_double),dimension(:,:,:,:,:),intent(in)      :: chksum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist
    real(c_double),intent(in),optional                  :: mask_val
    integer(LONG_KIND)                                  :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(chksum_data)) then
        call mpp_error(FATAL,'MPP_CHKSUM_R64_5D: data is not contiguous')
    endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    num_bytes = int(8,kind=c_size_t)

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag, &
                                            mask_val, &
                                            1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style(chksum_data, &
                                            size(chksum_data), &
                                            num_bytes,mask_flag,0, &
                                            1,null_pelist)
        endif
    endif

  end function mpp_chksum_r64_5D

 !-----------------------------------------------------------------------------
 !>Perform a check-sum across all ranks on a pelist.
  function mpp_chksum_c_style(chksum_data,chksum_len,num_bytes,mask_flag, &
                              mask_val,n,pelist) result(chksum_val)
    use iso_c_binding, only: c_size_t,c_int8_t

   !<Inputs/Outputs
    type(*),dimension(..),intent(in)          :: chksum_data
    integer(INT_KIND),intent(in)              :: chksum_len
    integer(c_size_t),intent(in)              :: num_bytes
    integer(c_int8_t),intent(in)              :: mask_flag
    type(*),dimension(..),intent(in)          :: mask_val
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
#endif

 !*****************************************************************************
