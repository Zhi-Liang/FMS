 !*****************************************************************************
 !Public facing mpp_chksum routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for scalar data.
  function mpp_chksum_scalar(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int8_t,c_size_t,c_int32_t,c_int64_t

   !<Inputs/Outputs
    class(*),intent(in)                                :: chksum_data
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist
    class(*),intent(in),optional                       :: mask_val
    integer(LONG_KIND)                                 :: chksum_val

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
    select type (chksum_data)
    type is (integer(INT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (integer(LONG_KIND))
        num_bytes = int(8,kind=c_size_t)
    type is (real(FLOAT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (real(DOUBLE_KIND))
        num_bytes = int(8,kind=c_size_t)
    class default
        call mpp_error(FATAL,'MPP_CHKSUM_SCALAR: unsupported type.')
    end select

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_scalar(chksum_data,1, &
                                                   num_bytes,mask_flag,mask_val, &
                                                   size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style_scalar(chksum_data,1, &
                                                   num_bytes,mask_flag,0, &
                                                   size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_scalar(chksum_data,1, &
                                                   num_bytes,mask_flag,mask_val, &
                                                   1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style_scalar(chksum_data,1, &
                                                   num_bytes,mask_flag,0, &
                                                   1,null_pelist)
        endif
    endif

  end function mpp_chksum_scalar

 !-----------------------------------------------------------------------------
 !>Wrapper for one-dimensional data.
  function mpp_chksum_1D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int8_t,c_size_t,c_int32_t,c_int64_t

   !<Inputs/Outputs
    class(*),dimension(:),intent(in)                   :: chksum_data
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist
    class(*),intent(in),optional                       :: mask_val
    integer(LONG_KIND)                                 :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(chksum_data)) then
!       call mpp_error(FATAL,'MPP_CHKSUM_1D: data is not contiguous.')
!   endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    select type (chksum_data)
    type is (integer(INT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (integer(LONG_KIND))
        num_bytes = int(8,kind=c_size_t)
    type is (real(FLOAT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (real(DOUBLE_KIND))
        num_bytes = int(8,kind=c_size_t)
    class default
        call mpp_error(FATAL,'MPP_CHKSUM_1D: unsupported type.')
    end select

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  1,null_pelist)
        endif
    endif

  end function mpp_chksum_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for two-dimensional data.
  function mpp_chksum_2D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int8_t,c_size_t,c_int32_t,c_int64_t

   !<Inputs/Outputs
    class(*),dimension(:,:),intent(in)                 :: chksum_data
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist
    class(*),intent(in),optional                       :: mask_val
    integer(LONG_KIND)                                 :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(chksum_data)) then
!       call mpp_error(FATAL,'MPP_CHKSUM_2D: data is not contiguous.')
!   endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    select type (chksum_data)
    type is (integer(INT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (integer(LONG_KIND))
        num_bytes = int(8,kind=c_size_t)
    type is (real(FLOAT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (real(DOUBLE_KIND))
        num_bytes = int(8,kind=c_size_t)
    class default
        call mpp_error(FATAL,'MPP_CHKSUM_2D: unsupported type.')
    end select

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  1,null_pelist)
        endif
    endif

  end function mpp_chksum_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for three-dimensional data.
  function mpp_chksum_3D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int8_t,c_size_t,c_int32_t,c_int64_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:),intent(in)               :: chksum_data
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist
    class(*),intent(in),optional                       :: mask_val
    integer(LONG_KIND)                                 :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(chksum_data)) then
!       call mpp_error(FATAL,'MPP_CHKSUM_3D: data is not contiguous.')
!   endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    select type (chksum_data)
    type is (integer(INT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (integer(LONG_KIND))
        num_bytes = int(8,kind=c_size_t)
    type is (real(FLOAT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (real(DOUBLE_KIND))
        num_bytes = int(8,kind=c_size_t)
    class default
        call mpp_error(FATAL,'MPP_CHKSUM_3D: unsupported type.')
    end select

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  1,null_pelist)
        endif
    endif

  end function mpp_chksum_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for four-dimensional data.
  function mpp_chksum_4D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int8_t,c_size_t,c_int32_t,c_int64_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:),intent(in)             :: chksum_data
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist
    class(*),intent(in),optional                       :: mask_val
    integer(LONG_KIND)                                 :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(chksum_data)) then
!       call mpp_error(FATAL,'MPP_CHKSUM_4D: data is not contiguous.')
!   endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    select type (chksum_data)
    type is (integer(INT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (integer(LONG_KIND))
        num_bytes = int(8,kind=c_size_t)
    type is (real(FLOAT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (real(DOUBLE_KIND))
        num_bytes = int(8,kind=c_size_t)
    class default
        call mpp_error(FATAL,'MPP_CHKSUM_4D: unsupported type.')
    end select

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  1,null_pelist)
        endif
    endif

  end function mpp_chksum_4D

 !-----------------------------------------------------------------------------
 !>Wrapper for five-dimensional data.
  function mpp_chksum_5D(chksum_data,pelist,mask_val) result(chksum_val)
    use iso_c_binding, only: c_int8_t,c_size_t,c_int32_t,c_int64_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:,:),intent(in)           :: chksum_data
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist
    class(*),intent(in),optional                       :: mask_val
    integer(LONG_KIND)                                 :: chksum_val

   !<Local variables
    integer(c_int8_t)               :: mask_flag
    integer(c_size_t)               :: num_bytes
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(chksum_data)) then
!       call mpp_error(FATAL,'MPP_CHKSUM_5D: data is not contiguous.')
!   endif

   !<Set the mask flag.
    if (present(mask_val)) then
        mask_flag = MPP_C_TRUE
    else
        mask_flag = MPP_C_FALSE
    endif

   !<Set the number of bytes.
    select type (chksum_data)
    type is (integer(INT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (integer(LONG_KIND))
        num_bytes = int(8,kind=c_size_t)
    type is (real(FLOAT_KIND))
        num_bytes = int(4,kind=c_size_t)
    type is (real(DOUBLE_KIND))
        num_bytes = int(8,kind=c_size_t)
    class default
        call mpp_error(FATAL,'MPP_CHKSUM_5D: unsupported type.')
    end select

    if (present(pelist)) then
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  size(pelist),pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  size(pelist),pelist)
        endif
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        if (present(mask_val)) then
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,mask_val, &
                                                  1,null_pelist)
        else
            chksum_val = mpp_chksum_c_style_array(chksum_data,size(chksum_data), &
                                                  num_bytes,mask_flag,0, &
                                                  1,null_pelist)
        endif
    endif

  end function mpp_chksum_5D

 !-----------------------------------------------------------------------------
 !>Perform a check-sum across all ranks on a pelist.
  function mpp_chksum_c_style_scalar(chksum_data,chksum_len,num_bytes,mask_flag, &
                                     mask_val,n,pelist) result(chksum_val)
    use iso_c_binding, only: c_size_t,c_int8_t

   !<Inputs/Outputs
    type(*),intent(in)                        :: chksum_data
    integer(INT_KIND),intent(in)              :: chksum_len
    integer(c_size_t),intent(in)              :: num_bytes
    integer(c_int8_t),intent(in)              :: mask_flag
    type(*),intent(in)                        :: mask_val
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist
    integer(LONG_KIND)                        :: chksum_val

    chksum_val = int(iso_c_mpp_chksum_scalar(chksum_data, &
                                             int(chksum_len,kind=c_size_t), &
                                             num_bytes,mask_flag,mask_val,pelist, &
                                             int(n,kind=c_size_t),mpp_context), &
                                             kind=LONG_KIND)

  end function mpp_chksum_c_style_scalar

 !-----------------------------------------------------------------------------
 !>Perform a check-sum across all ranks on a pelist.
  function mpp_chksum_c_style_array(chksum_data,chksum_len,num_bytes,mask_flag, &
                                    mask_val,n,pelist) result(chksum_val)
    use iso_c_binding, only: c_size_t,c_int8_t

   !<Inputs/Outputs
    type(*),dimension(*),intent(in)           :: chksum_data
    integer(INT_KIND),intent(in)              :: chksum_len
    integer(c_size_t),intent(in)              :: num_bytes
    integer(c_int8_t),intent(in)              :: mask_flag
    type(*),intent(in)                        :: mask_val
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist
    integer(LONG_KIND)                        :: chksum_val

    chksum_val = int(iso_c_mpp_chksum_array(chksum_data, &
                                            int(chksum_len,kind=c_size_t), &
                                            num_bytes,mask_flag,mask_val,pelist, &
                                            int(n,kind=c_size_t),mpp_context), &
                                            kind=LONG_KIND)

  end function mpp_chksum_c_style_array

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
