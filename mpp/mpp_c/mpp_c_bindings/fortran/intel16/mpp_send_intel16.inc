 !*****************************************************************************
 !Public facing mpp_send routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer data.
! subroutine mpp_send_i32s(put_data,to_pe,tag,request)
!   use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
!   integer(c_int32_t),intent(inout)         :: put_data
!   integer(c_int32_t),intent(in)            :: to_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
!   integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = int(tag,kind=c_int32_t)
!   else
!       comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
!   endif

!   call mpp_send_c_style(put_data,1,to_pe,MPP_INT32,comm_tag,request)

! end subroutine mpp_send_i32s

  subroutine mpp_send_i32(put_data,plen,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(..),intent(inout) :: put_data
    integer(INT_KIND),intent(in)                   :: plen
    integer(c_int32_t),intent(in)                  :: to_pe
    integer(INT_KIND),intent(in),optional          :: tag
    integer(INT_KIND),intent(inout),optional       :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

    call mpp_send_c_style(put_data,plen,to_pe,MPP_INT32,comm_tag,request)

  end subroutine mpp_send_i32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer data.
! subroutine mpp_send_i64s(put_data,to_pe,tag,request)
!   use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
!   integer(c_int64_t),intent(inout)         :: put_data
!   integer(c_int32_t),intent(in)            :: to_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
!   integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = int(tag,kind=c_int32_t)
!   else
!       comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
!   endif

!   call mpp_send_c_style(put_data,1,MPP_INT64,comm_tag,request)

! end subroutine mpp_send_i64s

  subroutine mpp_send_i64(put_data,plen,to_pe,tag,request)
    use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(..),intent(inout) :: put_data
    integer(INT_KIND),intent(in)                   :: plen
    integer(c_int32_t),intent(in)                  :: to_pe
    integer(INT_KIND),intent(in),optional          :: tag
    integer(INT_KIND),intent(inout),optional       :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

    call mpp_send_c_style(put_data,plen,to_pe,MPP_INT64,comm_tag,request)

  end subroutine mpp_send_i64

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real data.
! subroutine mpp_send_r32s(put_data,to_pe,tag,request)
!   use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
!   real(c_float),intent(inout)              :: put_data
!   integer(c_int32_t),intent(in)            :: to_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
!   integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = int(tag,kind=c_int32_t)
!   else
!       comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
!   endif

!   call mpp_send_c_style(put_data,1,to_pe,MPP_REAL32,comm_tag,request)

! end subroutine mpp_send_r32s

  subroutine mpp_send_r32(put_data,plen,to_pe,tag,request)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),dimension(..),intent(inout) :: put_data
    integer(INT_KIND),intent(in)              :: plen
    integer(c_int32_t),intent(in)             :: to_pe
    integer(INT_KIND),intent(in),optional     :: tag
    integer(INT_KIND),intent(inout),optional  :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

    call mpp_send_c_style(put_data,plen,to_pe,MPP_REAL32,comm_tag,request)

  end subroutine mpp_send_r32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real data.
! subroutine mpp_send_r64s(put_data,to_pe,tag,request)
!   use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
!   real(c_double),intent(inout)             :: put_data
!   integer(c_int32_t),intent(in)            :: to_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
!   integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = int(tag,kind=c_int32_t)
!   else
!       comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
!   endif

!   call mpp_send_c_style(put_data,1,to_pe,MPP_REAL64,comm_tag,request)

! end subroutine mpp_send_r64s

  subroutine mpp_send_r64(put_data,plen,to_pe,tag,request)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),dimension(..),intent(inout) :: put_data
    integer(INT_KIND),intent(in)               :: plen
    integer(c_int32_t),intent(in)              :: to_pe
    integer(INT_KIND),intent(in),optional      :: tag
    integer(INT_KIND),intent(inout),optional   :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

    call mpp_send_c_style(put_data,plen,to_pe,MPP_REAL64,comm_tag,request)

  end subroutine mpp_send_r64

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit logical data.
! subroutine mpp_send_l32s(put_data,to_pe,tag,request)
!   use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
!   logical(INT_KIND),intent(inout)          :: put_data
!   integer(c_int32_t),intent(in)            :: to_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
!   integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = int(tag,kind=c_int32_t)
!   else
!       comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
!   endif

!   call mpp_send_c_style(put_data,1,to_pe,MPP_INT32,comm_tag,request)

! end subroutine mpp_send_l32s

  subroutine mpp_send_l32(put_data,plen,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    logical(INT_KIND),dimension(..),intent(inout) :: put_data
    integer(INT_KIND),intent(in)                  :: plen
    integer(c_int32_t),intent(in)                 :: to_pe
    integer(INT_KIND),intent(in),optional         :: tag
    integer(INT_KIND),intent(inout),optional      :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

    call mpp_send_c_style(put_data,plen,to_pe,MPP_INT32,comm_tag,request)

  end subroutine mpp_send_l32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit logical data.
! subroutine mpp_send_l64s(put_data,to_pe,tag,request)
!   use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
!   logical(LONG_KIND),intent(inout)         :: put_data
!   integer(c_int32_t),intent(in)            :: to_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
!   integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = int(tag,kind=c_int32_t)
!   else
!       comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
!   endif

!   call mpp_send_c_style(put_data,1,to_pe,MPP_INT64,comm_tag,request)

! end subroutine mpp_send_l64s

  subroutine mpp_send_l64(put_data,plen,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    logical(LONG_KIND),dimension(..),intent(inout) :: put_data
    integer(INT_KIND),intent(in)                   :: plen
    integer(c_int32_t),intent(in)                  :: to_pe
    integer(INT_KIND),intent(in),optional          :: tag
    integer(INT_KIND),intent(inout),optional       :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

    call mpp_send_c_style(put_data,plen,to_pe,MPP_INT64,comm_tag,request)

  end subroutine mpp_send_l64

 !-----------------------------------------------------------------------------
 !>Calls the mpp_c_send routine.
  subroutine mpp_send_c_style(put_data,put_len,to_pe,mpp_type,comm_tag, &
                              request)
    use iso_c_binding, only: c_int32_t,c_int,c_size_t,c_null_ptr

   !<Inputs/Outputs
    type(*),dimension(..),intent(inout)      :: put_data
    integer(INT_KIND),intent(in)             :: put_len
    integer(c_int32_t),intent(in)            :: to_pe
    integer(c_int32_t),intent(in)            :: mpp_type
    integer(c_int32_t),intent(in)            :: comm_tag
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int) :: null_request

    if (present(request)) then
        call iso_c_mpp_send(put_data,int(put_len,kind=c_size_t), &
                            to_pe,mpp_type,comm_tag,request, &
                            mpp_context)
    else
        null_request = int(MPP_C_NULL_REQUEST,kind=c_int)
        call iso_c_mpp_send(put_data,int(put_len,kind=c_size_t), &
                            to_pe,mpp_type,comm_tag,null_request,mpp_context)
    endif

  end subroutine mpp_send_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
