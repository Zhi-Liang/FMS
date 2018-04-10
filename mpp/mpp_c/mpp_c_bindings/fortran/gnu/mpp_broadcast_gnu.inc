 !*****************************************************************************
 !Public facing mpp_broadcast routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for character data


 !-----------------------------------------------------------------------------
 !>Wrapper for scalar data.
  subroutine mpp_broadcast_scalar(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),intent(inout)                             :: bcast_data
    integer(INT_KIND),intent(in)                       :: bcast_len
    integer(INT_KIND),intent(in)                       :: from_pe
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Set the mpp_type value.
    select type (bcast_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    type is (logical(INT_KIND))
        mpp_type = MPP_INT32
    type is (logical(LONG_KIND))
        mpp_type = MPP_INT64
    class default
        call mpp_error(FATAL,'MPP_BROADCAST_SCALAR: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_broadcast_c_style_scalar(bcast_data,bcast_len,from_pe,mpp_type, &
                                          size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style_scalar(bcast_data,bcast_len,from_pe,mpp_type, &
                                          1,null_pelist)
    endif

  end subroutine mpp_broadcast_scalar

 !-----------------------------------------------------------------------------
 !>Wrapper for one-dimensional data.
  subroutine mpp_broadcast_1D(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:),intent(inout)                :: bcast_data
    integer(INT_KIND),intent(in)                       :: bcast_len
    integer(INT_KIND),intent(in)                       :: from_pe
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_BROADCAST_1D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (bcast_len .ne. size(bcast_data)) then
        call mpp_error(FATAL,'MPP_BROADCAST_1D: inputted length does not' &
                             //' match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (bcast_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    type is (logical(INT_KIND))
        mpp_type = MPP_INT32
    type is (logical(LONG_KIND))
        mpp_type = MPP_INT64
    class default
        call mpp_error(FATAL,'MPP_BROADCAST_1D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         1,null_pelist)
    endif

  end subroutine mpp_broadcast_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for two-dimensional data.
  subroutine mpp_broadcast_2D(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:,:),intent(inout)              :: bcast_data
    integer(INT_KIND),intent(in)                       :: bcast_len
    integer(INT_KIND),intent(in)                       :: from_pe
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_BROADCAST_2D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (bcast_len .ne. size(bcast_data)) then
        call mpp_error(FATAL,'MPP_BROADCAST_2D: inputted length does not' &
                             //' match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (bcast_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    type is (logical(INT_KIND))
        mpp_type = MPP_INT32
    type is (logical(LONG_KIND))
        mpp_type = MPP_INT64
    class default
        call mpp_error(FATAL,'MPP_BROADCAST_2D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         1,null_pelist)
    endif

  end subroutine mpp_broadcast_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for three-dimensional data.
  subroutine mpp_broadcast_3D(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:),intent(inout)            :: bcast_data
    integer(INT_KIND),intent(in)                       :: bcast_len
    integer(INT_KIND),intent(in)                       :: from_pe
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_BROADCAST_3D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (bcast_len .ne. size(bcast_data)) then
        call mpp_error(FATAL,'MPP_BROADCAST_3D: inputted length does not' &
                             //' match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (bcast_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    type is (logical(INT_KIND))
        mpp_type = MPP_INT32
    type is (logical(LONG_KIND))
        mpp_type = MPP_INT64
    class default
        call mpp_error(FATAL,'MPP_BROADCAST_3D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         1,null_pelist)
    endif

  end subroutine mpp_broadcast_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for four-dimensional data.
  subroutine mpp_broadcast_4D(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:),intent(inout)          :: bcast_data
    integer(INT_KIND),intent(in)                       :: bcast_len
    integer(INT_KIND),intent(in)                       :: from_pe
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_BROADCAST_4D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (bcast_len .ne. size(bcast_data)) then
        call mpp_error(FATAL,'MPP_BROADCAST_4D: inputted length does not' &
                             //' match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (bcast_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    type is (logical(INT_KIND))
        mpp_type = MPP_INT32
    type is (logical(LONG_KIND))
        mpp_type = MPP_INT64
    class default
        call mpp_error(FATAL,'MPP_BROADCAST_4D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         1,null_pelist)
    endif

  end subroutine mpp_broadcast_4D

 !-----------------------------------------------------------------------------
 !>Wrapper for five-dimensional data.
  subroutine mpp_broadcast_5D(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:,:),intent(inout)        :: bcast_data
    integer(INT_KIND),intent(in)                       :: bcast_len
    integer(INT_KIND),intent(in)                       :: from_pe
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t)              :: mpp_type
    integer(c_int32_t),dimension(1) :: null_pelist

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_BROADCAST_5D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (bcast_len .ne. size(bcast_data)) then
        call mpp_error(FATAL,'MPP_BROADCAST_5D: inputted length does not' &
                             //' match the size of the inputted data.')
    endif

   !<Set the mpp_type value.
    select type (bcast_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (integer(LONG_KIND))
        mpp_type = MPP_INT64
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    type is (logical(INT_KIND))
        mpp_type = MPP_INT32
    type is (logical(LONG_KIND))
        mpp_type = MPP_INT64
    class default
        call mpp_error(FATAL,'MPP_BROADCAST_5D: unsupported type.')
    end select

    if (present(pelist)) then
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         1,null_pelist)
    endif

  end subroutine mpp_broadcast_5D

 !-----------------------------------------------------------------------------
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style_scalar(bcast_data,bcast_len,from_pe,mpp_type, &
                                          n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),intent(inout)                     :: bcast_data
    integer(INT_KIND),intent(in)              :: bcast_len
    integer(INT_KIND),intent(in)              :: from_pe
    integer(c_int32_t),intent(in)             :: mpp_type
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist

    call iso_c_mpp_broadcast_scalar(bcast_data,int(bcast_len,kind=c_size_t), &
                                    from_pe,mpp_type,pelist, &
                                    int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_broadcast_c_style_scalar

 !-----------------------------------------------------------------------------
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style_array(bcast_data,bcast_len,from_pe,mpp_type, &
                                         n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(*),intent(inout)        :: bcast_data
    integer(INT_KIND),intent(in)              :: bcast_len
    integer(INT_KIND),intent(in)              :: from_pe
    integer(c_int32_t),intent(in)             :: mpp_type
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist

    call iso_c_mpp_broadcast_array(bcast_data,int(bcast_len,kind=c_size_t), &
                                   from_pe,mpp_type,pelist, &
                                   int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_broadcast_c_style_array

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
