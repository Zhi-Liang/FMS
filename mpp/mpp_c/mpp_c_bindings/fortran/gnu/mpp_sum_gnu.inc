 !*****************************************************************************
 !Public facing mpp_sum routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for scalar data.
  subroutine mpp_sum_scalar(sum_data,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),intent(inout)                             :: sum_data
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Set the mpp_type value.
    select type (sum_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_SUM_SCALAR: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_sum_c_style_scalar(sum_data,1,mpp_type,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style_scalar(sum_data,1,mpp_type,1,null_pelist)
    endif

  end subroutine mpp_sum_scalar

 !-----------------------------------------------------------------------------
 !>Wrapper for one-dimensional data.
  subroutine mpp_sum_1D(sum_data,sum_len,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:),intent(inout)                :: sum_data
    integer(INT_KIND),intent(in)                       :: sum_len
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(sum_data)) then
!       call mpp_error(FATAL,'MPP_SUM_1D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (sum_len .ne. size(sum_data)) then
        call mpp_error(FATAL,'MPP_SUM_1D: inputted length does' &
                             //' not match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (sum_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_SUM_1D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,1,null_pelist)
    endif

  end subroutine mpp_sum_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for two-dimensional data.
  subroutine mpp_sum_2D(sum_data,sum_len,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:,:),intent(inout)              :: sum_data
    integer(INT_KIND),intent(in)                       :: sum_len
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(sum_data)) then
!       call mpp_error(FATAL,'MPP_SUM_2D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (sum_len .ne. size(sum_data)) then
        call mpp_error(FATAL,'MPP_SUM_2D: inputted length does' &
                             //' not match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (sum_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_SUM_2D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,1,null_pelist)
    endif

  end subroutine mpp_sum_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for three-dimensional data.
  subroutine mpp_sum_3D(sum_data,sum_len,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:),intent(inout)            :: sum_data
    integer(INT_KIND),intent(in)                       :: sum_len
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(sum_data)) then
!       call mpp_error(FATAL,'MPP_SUM_3D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (sum_len .ne. size(sum_data)) then
        call mpp_error(FATAL,'MPP_SUM_3D: inputted length does' &
                             //' not match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (sum_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_SUM_3D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,1,null_pelist)
    endif

  end subroutine mpp_sum_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for four-dimensional data.
  subroutine mpp_sum_4D(sum_data,sum_len,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:),intent(inout)          :: sum_data
    integer(INT_KIND),intent(in)                       :: sum_len
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(sum_data)) then
!       call mpp_error(FATAL,'MPP_SUM_4D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (sum_len .ne. size(sum_data)) then
        call mpp_error(FATAL,'MPP_SUM_4D: inputted length does' &
                             //' not match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (sum_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_SUM_4D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,1,null_pelist)
    endif

  end subroutine mpp_sum_4D

 !-----------------------------------------------------------------------------
 !>Wrapper for five-dimensional data.
  subroutine mpp_sum_5D(sum_data,sum_len,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:,:),intent(inout)        :: sum_data
    integer(INT_KIND),intent(in)                       :: sum_len
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(sum_data)) then
!       call mpp_error(FATAL,'MPP_SUM_5D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (sum_len .ne. size(sum_data)) then
        call mpp_error(FATAL,'MPP_SUM_5D: inputted length does' &
                             //' not match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (sum_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_SUM_5D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style_array(sum_data,sum_len,mpp_type,1,null_pelist)
    endif

  end subroutine mpp_sum_5D

 !-----------------------------------------------------------------------------
 !>Sum the data across ranks on a pelist.
  subroutine mpp_sum_c_style_scalar(sum_data,sum_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),intent(inout)                     :: sum_data
    integer(INT_KIND),intent(in)              :: sum_len
    integer(c_int32_t),intent(in)             :: mpp_type
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist

    call iso_c_mpp_sum_scalar(sum_data,int(sum_len,kind=c_size_t),mpp_type, &
                              pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_sum_c_style_scalar

 !-----------------------------------------------------------------------------
 !>Sum the data across ranks on a pelist.
  subroutine mpp_sum_c_style_array(sum_data,sum_len,mpp_type,n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(*),intent(inout)        :: sum_data
    integer(INT_KIND),intent(in)              :: sum_len
    integer(c_int32_t),intent(in)             :: mpp_type
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist

    call iso_c_mpp_sum_array(sum_data,int(sum_len,kind=c_size_t),mpp_type, &
                             pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_sum_c_style_array

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
