 !*****************************************************************************
 !Public facing mpp_send routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for scalar data.
  subroutine mpp_send_scalar(put_data,put_len,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),intent(inout)                   :: put_data
    integer(INT_KIND),intent(in)             :: put_len
    integer(INT_KIND),intent(in)             :: to_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: mpp_type

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

   !<Set the mpp_type value.
    select type (put_data)
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
        call mpp_error(FATAL,'MPP_SEND_SCALAR: unsupported type.')
    end select

    call mpp_send_c_style_scalar(put_data,put_len,to_pe,mpp_type,comm_tag, &
                                 request)

  end subroutine mpp_send_scalar

 !-----------------------------------------------------------------------------
 !>Wrapper for one-dimensional data.
  subroutine mpp_send_1D(put_data,put_len,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:),intent(inout)      :: put_data
    integer(INT_KIND),intent(in)             :: put_len
    integer(INT_KIND),intent(in)             :: to_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_SEND_1D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (put_len .ne. size(put_data)) then
        call mpp_error(FATAL,'MPP_SEND_1D: inputted length does not' &
                              //' match the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

   !<Set the mpp_type value.
    select type (put_data)
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
        call mpp_error(FATAL,'MPP_SEND_1D: unsupported type.')
    end select

  call mpp_send_c_style_array(put_data,put_len,to_pe,mpp_type,comm_tag, &
                              request)

  end subroutine mpp_send_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for two-dimensional data.
  subroutine mpp_send_2D(put_data,put_len,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:,:),intent(inout)    :: put_data
    integer(INT_KIND),intent(in)             :: put_len
    integer(INT_KIND),intent(in)             :: to_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_SEND_2D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (put_len .ne. size(put_data)) then
        call mpp_error(FATAL,'MPP_SEND_2D: inputted length does not' &
                              //' match the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

   !<Set the mpp_type value.
    select type (put_data)
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
        call mpp_error(FATAL,'MPP_SEND_2D: unsupported type.')
    end select

  call mpp_send_c_style_array(put_data,put_len,to_pe,mpp_type,comm_tag, &
                              request)

  end subroutine mpp_send_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for three-dimensional data.
  subroutine mpp_send_3D(put_data,put_len,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:),intent(inout)  :: put_data
    integer(INT_KIND),intent(in)             :: put_len
    integer(INT_KIND),intent(in)             :: to_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_SEND_3D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (put_len .ne. size(put_data)) then
        call mpp_error(FATAL,'MPP_SEND_3D: inputted length does not' &
                              //' match the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

   !<Set the mpp_type value.
    select type (put_data)
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
        call mpp_error(FATAL,'MPP_SEND_3D: unsupported type.')
    end select

  call mpp_send_c_style_array(put_data,put_len,to_pe,mpp_type,comm_tag, &
                              request)

  end subroutine mpp_send_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for four-dimensional data.
  subroutine mpp_send_4D(put_data,put_len,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:),intent(inout) :: put_data
    integer(INT_KIND),intent(in)              :: put_len
    integer(INT_KIND),intent(in)              :: to_pe
    integer(INT_KIND),intent(in),optional     :: tag
    integer(INT_KIND),intent(inout),optional  :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_SEND_4D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (put_len .ne. size(put_data)) then
        call mpp_error(FATAL,'MPP_SEND_4D: inputted length does not' &
                              //' match the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

   !<Set the mpp_type value.
    select type (put_data)
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
        call mpp_error(FATAL,'MPP_SEND_4D: unsupported type.')
    end select

  call mpp_send_c_style_array(put_data,put_len,to_pe,mpp_type,comm_tag, &
                              request)

  end subroutine mpp_send_4D

 !-----------------------------------------------------------------------------
 !>Wrapper for five-dimensional data.
  subroutine mpp_send_5D(put_data,put_len,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:,:),intent(inout) :: put_data
    integer(INT_KIND),intent(in)                :: put_len
    integer(INT_KIND),intent(in)                :: to_pe
    integer(INT_KIND),intent(in),optional       :: tag
    integer(INT_KIND),intent(inout),optional    :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(put_data)) then
!       call mpp_error(FATAL,'MPP_SEND_5D: data is not contiguous.')
!   endif

   !<Make sure that the inputted length matches the size of the input data.
    if (put_len .ne. size(put_data)) then
        call mpp_error(FATAL,'MPP_SEND_5D: inputted length does not' &
                              //' match the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

   !<Set the mpp_type value.
    select type (put_data)
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
        call mpp_error(FATAL,'MPP_SEND_5D: unsupported type.')
    end select

  call mpp_send_c_style_array(put_data,put_len,to_pe,mpp_type,comm_tag, &
                              request)

  end subroutine mpp_send_5D

 !-----------------------------------------------------------------------------
 !>Calls the mpp_c_send routine.
  subroutine mpp_send_c_style_scalar(put_data,put_len,to_pe,mpp_type, &
                                     comm_tag,request)
    use iso_c_binding, only: c_int32_t,c_int,c_size_t,c_null_ptr

   !<Inputs/Outputs
    type(*),intent(inout)                    :: put_data
    integer(INT_KIND),intent(in)             :: put_len
    integer(INT_KIND),intent(in)             :: to_pe
    integer(c_int32_t),intent(in)            :: mpp_type
    integer(c_int32_t),intent(in)            :: comm_tag
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int) :: null_request

    if (present(request)) then
        call iso_c_mpp_send_scalar(put_data,int(put_len,kind=c_size_t), &
                                   int(to_pe,kind=c_int32_t),mpp_type, &
                                   comm_tag,request,mpp_context)
    else
        null_request = int(MPP_C_NULL_REQUEST,kind=c_int)
        call iso_c_mpp_send_scalar(put_data,int(put_len,kind=c_size_t), &
                                   int(to_pe,kind=c_int32_t),mpp_type, &
                                   comm_tag,null_request,mpp_context)
    endif

  end subroutine mpp_send_c_style_scalar

 !-----------------------------------------------------------------------------
 !>Calls the mpp_c_send routine.
  subroutine mpp_send_c_style_array(put_data,put_len,to_pe,mpp_type, &
                                    comm_tag,request)
    use iso_c_binding, only: c_int32_t,c_int,c_size_t,c_null_ptr

   !<Inputs/Outputs
    type(*),dimension(*),intent(inout)       :: put_data
    integer(INT_KIND),intent(in)             :: put_len
    integer(INT_KIND),intent(in)             :: to_pe
    integer(c_int32_t),intent(in)            :: mpp_type
    integer(c_int32_t),intent(in)            :: comm_tag
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int) :: null_request

    if (present(request)) then
        call iso_c_mpp_send_array(put_data,int(put_len,kind=c_size_t), &
                                  int(to_pe,kind=c_int32_t),mpp_type, &
                                  comm_tag,request,mpp_context)
    else
        null_request = int(MPP_C_NULL_REQUEST,kind=c_int)
        call iso_c_mpp_send_array(put_data,int(put_len,kind=c_size_t), &
                                  int(to_pe,kind=c_int32_t),mpp_type, &
                                  comm_tag,null_request,mpp_context)
    endif

  end subroutine mpp_send_c_style_array

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
