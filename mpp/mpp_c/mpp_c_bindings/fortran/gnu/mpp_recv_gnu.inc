 !*****************************************************************************
 !Public facing mpp_recv routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for scalar data.
  subroutine mpp_recv_scalar(get_data,get_len,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),intent(inout)                   :: get_data
    integer(INT_KIND),intent(in)             :: get_len
    integer(INT_KIND),intent(in)             :: from_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request
    logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block
    integer(c_int32_t) :: mpp_type

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = tag
    else
        comm_tag = DEFAULT_TAG
    endif

   !<Set the value of comm_block.
    comm_block = MPP_C_TRUE
    if (present(block)) then
        if (.not. block) then
            comm_block = MPP_C_FALSE
        endif
    endif

   !<Set the mpp_type value.
    select type (get_data)
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
        call mpp_error(FATAL,'MPP_RECV_SCALAR: unsupported type.')
    end select

    call mpp_recv_c_style_scalar(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                 comm_block,request)

  end subroutine mpp_recv_scalar

 !-----------------------------------------------------------------------------
 !>Wrapper for one-dimensional data.
  subroutine mpp_recv_1D(get_data,get_len,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:),intent(inout)      :: get_data
    integer(INT_KIND),intent(in)             :: get_len
    integer(INT_KIND),intent(in)             :: from_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request
    logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(get_data)) then
!       call mpp_error(FATAL,'MPP_RECV_1D: data is not contiguous.')
!   endif

   !<Make sure that put_len matches the size of the input data
    if (get_len .ne. size(get_data)) then
        call mpp_error(FATAL,'MPP_RECV_1D: inputted length does not match' &
                             //' the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = tag
    else
        comm_tag = DEFAULT_TAG
    endif

   !<Set the value of comm_block.
    comm_block = MPP_C_TRUE
    if (present(block)) then
        if (.not. block) then
            comm_block = MPP_C_FALSE
        endif
    endif

   !<Set the mpp_type value.
    select type (get_data)
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
        call mpp_error(FATAL,'MPP_RECV_1D: unsupported type.')
    end select

    call mpp_recv_c_style_array(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                comm_block,request)

  end subroutine mpp_recv_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for two-dimensional data.
  subroutine mpp_recv_2D(get_data,get_len,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:,:),intent(inout)    :: get_data
    integer(INT_KIND),intent(in)             :: get_len
    integer(INT_KIND),intent(in)             :: from_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request
    logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(get_data)) then
!       call mpp_error(FATAL,'MPP_RECV_2D: data is not contiguous.')
!   endif

   !<Make sure that put_len matches the size of the input data
    if (get_len .ne. size(get_data)) then
        call mpp_error(FATAL,'MPP_RECV_2D: inputted length does not match' &
                             //' the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = tag
    else
        comm_tag = DEFAULT_TAG
    endif

   !<Set the value of comm_block.
    comm_block = MPP_C_TRUE
    if (present(block)) then
        if (.not. block) then
            comm_block = MPP_C_FALSE
        endif
    endif

   !<Set the mpp_type value.
    select type (get_data)
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
        call mpp_error(FATAL,'MPP_RECV_2D: unsupported type.')
    end select

    call mpp_recv_c_style_array(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                comm_block,request)

  end subroutine mpp_recv_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for three-dimensional data.
  subroutine mpp_recv_3D(get_data,get_len,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:),intent(inout)  :: get_data
    integer(INT_KIND),intent(in)             :: get_len
    integer(INT_KIND),intent(in)             :: from_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request
    logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(get_data)) then
!       call mpp_error(FATAL,'MPP_RECV_3D: data is not contiguous.')
!   endif

   !<Make sure that put_len matches the size of the input data
    if (get_len .ne. size(get_data)) then
        call mpp_error(FATAL,'MPP_RECV_3D: inputted length does not match' &
                             //' the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = tag
    else
        comm_tag = DEFAULT_TAG
    endif

   !<Set the value of comm_block.
    comm_block = MPP_C_TRUE
    if (present(block)) then
        if (.not. block) then
            comm_block = MPP_C_FALSE
        endif
    endif

   !<Set the mpp_type value.
    select type (get_data)
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
        call mpp_error(FATAL,'MPP_RECV_3D: unsupported type.')
    end select

    call mpp_recv_c_style_array(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                comm_block,request)

  end subroutine mpp_recv_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for four-dimensional data.
  subroutine mpp_recv_4D(get_data,get_len,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:),intent(inout) :: get_data
    integer(INT_KIND),intent(in)              :: get_len
    integer(INT_KIND),intent(in)              :: from_pe
    integer(INT_KIND),intent(in),optional     :: tag
    integer(INT_KIND),intent(inout),optional  :: request
    logical(INT_KIND),intent(in),optional     :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(get_data)) then
!       call mpp_error(FATAL,'MPP_RECV_4D: data is not contiguous.')
!   endif

   !<Make sure that put_len matches the size of the input data
    if (get_len .ne. size(get_data)) then
        call mpp_error(FATAL,'MPP_RECV_4D: inputted length does not match' &
                             //' the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = tag
    else
        comm_tag = DEFAULT_TAG
    endif

   !<Set the value of comm_block.
    comm_block = MPP_C_TRUE
    if (present(block)) then
        if (.not. block) then
            comm_block = MPP_C_FALSE
        endif
    endif

   !<Set the mpp_type value.
    select type (get_data)
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
        call mpp_error(FATAL,'MPP_RECV_4D: unsupported type.')
    end select

    call mpp_recv_c_style_array(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                comm_block,request)

  end subroutine mpp_recv_4D

 !-----------------------------------------------------------------------------
 !>Wrapper for five-dimensional data.
  subroutine mpp_recv_5D(get_data,get_len,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    class(*),dimension(:,:,:,:,:),intent(inout) :: get_data
    integer(INT_KIND),intent(in)                :: get_len
    integer(INT_KIND),intent(in)                :: from_pe
    integer(INT_KIND),intent(in),optional       :: tag
    integer(INT_KIND),intent(inout),optional    :: request
    logical(INT_KIND),intent(in),optional       :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block
    integer(c_int32_t) :: mpp_type

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(get_data)) then
!       call mpp_error(FATAL,'MPP_RECV_5D: data is not contiguous.')
!   endif

   !<Make sure that put_len matches the size of the input data
    if (get_len .ne. size(get_data)) then
        call mpp_error(FATAL,'MPP_RECV_5D: inputted length does not match' &
                             //' the size of the inputted data.')
    endif

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = tag
    else
        comm_tag = DEFAULT_TAG
    endif

   !<Set the value of comm_block.
    comm_block = MPP_C_TRUE
    if (present(block)) then
        if (.not. block) then
            comm_block = MPP_C_FALSE
        endif
    endif

   !<Set the mpp_type value.
    select type (get_data)
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
        call mpp_error(FATAL,'MPP_RECV_5D: unsupported type.')
    end select

    call mpp_recv_c_style_array(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                comm_block,request)

  end subroutine mpp_recv_5D

 !-----------------------------------------------------------------------------
 !>Receive data from another rank on the world pelist.
  subroutine mpp_recv_c_style_scalar(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                     comm_block,request)
    use iso_c_binding, only: c_int32_t,c_int,c_size_t

   !<Inputs/Outputs
    type(*),intent(inout)                    :: get_data
    integer(INT_KIND),intent(in)             :: get_len
    integer(INT_KIND),intent(in)             :: from_pe
    integer(c_int32_t),intent(in)            :: mpp_type
    integer(c_int32_t),intent(in)            :: comm_tag
    integer(c_int32_t),intent(in)            :: comm_block
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int) :: null_request

    if (present(request)) then
        call iso_c_mpp_recv_scalar(get_data,int(get_len,kind=c_size_t), &
                                   int(from_pe,kind=c_int32_t),mpp_type, &
                                   comm_tag,request,comm_block,mpp_context)
    else
        null_request = MPP_C_NULL_REQUEST
        call iso_c_mpp_recv_scalar(get_data,int(get_len,kind=c_size_t), &
                                   int(from_pe,kind=c_int32_t),mpp_type, &
                                   comm_tag,null_request,comm_block, &
                                   mpp_context)
    endif

  end subroutine mpp_recv_c_style_scalar

 !-----------------------------------------------------------------------------
 !>Receive data from another rank on the world pelist.
  subroutine mpp_recv_c_style_array(get_data,get_len,from_pe,mpp_type,comm_tag, &
                                    comm_block,request)
    use iso_c_binding, only: c_int32_t,c_int,c_size_t

   !<Inputs/Outputs
    type(*),dimension(*),intent(inout)       :: get_data
    integer(INT_KIND),intent(in)             :: get_len
    integer(INT_KIND),intent(in)             :: from_pe
    integer(c_int32_t),intent(in)            :: mpp_type
    integer(c_int32_t),intent(in)            :: comm_tag
    integer(c_int32_t),intent(in)            :: comm_block
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int) :: null_request

    if (present(request)) then
        call iso_c_mpp_recv_array(get_data,int(get_len,kind=c_size_t), &
                                  int(from_pe,kind=c_int32_t),mpp_type, &
                                  comm_tag,request,comm_block,mpp_context)
    else
        null_request = MPP_C_NULL_REQUEST
        call iso_c_mpp_recv_array(get_data,int(get_len,kind=c_size_t), &
                                  int(from_pe,kind=c_int32_t),mpp_type, &
                                  comm_tag,null_request,comm_block, &
                                  mpp_context)
    endif

  end subroutine mpp_recv_c_style_array

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
