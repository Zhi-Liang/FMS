 !*****************************************************************************
 !Public facing mpp_recv routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer data.
! subroutine mpp_recv_i32s(get_data,from_pe,tag,request,block)
!   use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
!   integer(c_int32_t),intent(inout)         :: get_data
!   integer(INT_KIND),intent(in)             :: from_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request
!   logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
!   integer(c_int32_t) :: comm_tag
!   integer(c_int32_t) :: comm_block

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = tag
!   else
!       comm_tag = DEFAULT_TAG
!   endif

   !<Set the value of comm_block.
!   comm_block = MPP_C_TRUE
!   if (present(block)) then
!       if (.not. block) then
!           comm_block = MPP_C_FALSE
!       endif
!   endif

!   call mpp_recv_c_style(get_data,1,from_pe,MPP_INT32,comm_tag, &
!                         comm_block,request)

! end subroutine mpp_recv_i32s

  subroutine mpp_recv_i32(get_data,glen,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(..),intent(inout) :: get_data
    integer(INT_KIND),intent(in)                   :: glen
    integer(INT_KIND),intent(in)                   :: from_pe
    integer(INT_KIND),intent(in),optional          :: tag
    integer(INT_KIND),intent(inout),optional       :: request
    logical(INT_KIND),intent(in),optional          :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block

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

    call mpp_recv_c_style(get_data,glen,from_pe,MPP_INT32,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_i32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer data.
! subroutine mpp_recv_i64s(get_data,from_pe,tag,request,block)
!   use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
!   integer(c_int64_t),intent(inout)         :: get_data
!   integer(INT_KIND),intent(in)             :: from_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request
!   logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
!   integer(c_int32_t) :: comm_tag
!   integer(c_int32_t) :: comm_block

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = tag
!   else
!       comm_tag = DEFAULT_TAG
!   endif

   !<Set the value of comm_block.
!   comm_block = MPP_C_TRUE
!   if (present(block)) then
!       if (.not. block) then
!           comm_block = MPP_C_FALSE
!       endif
!   endif

!   call mpp_recv_c_style(get_data,1,from_pe,MPP_INT64,comm_tag, &
!                         comm_block,request)

! end subroutine mpp_recv_i64s

  subroutine mpp_recv_i64(get_data,glen,from_pe,tag,request,block)
    use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(..),intent(inout) :: get_data
    integer(INT_KIND),intent(in)                   :: glen
    integer(INT_KIND),intent(in)                   :: from_pe
    integer(INT_KIND),intent(in),optional          :: tag
    integer(INT_KIND),intent(inout),optional       :: request
    logical(INT_KIND),intent(in),optional          :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block

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

    call mpp_recv_c_style(get_data,glen,from_pe,MPP_INT64,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_i64

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real data.
! subroutine mpp_recv_r32s(get_data,from_pe,tag,request,block)
!   use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
!   real(c_float),intent(inout)              :: get_data
!   integer(INT_KIND),intent(in)             :: from_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request
!   logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
!   integer(c_int32_t) :: comm_tag
!   integer(c_int32_t) :: comm_block

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = tag
!   else
!       comm_tag = DEFAULT_TAG
!   endif

   !<Set the value of comm_block.
!   comm_block = MPP_C_TRUE
!   if (present(block)) then
!       if (.not. block) then
!           comm_block = MPP_C_FALSE
!       endif
!   endif

!   call mpp_recv_c_style(get_data,1,from_pe,MPP_REAL32,comm_tag, &
!                         comm_block,request)

! end subroutine mpp_recv_r32s

  subroutine mpp_recv_r32(get_data,glen,from_pe,tag,request,block)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),dimension(..),intent(inout) :: get_data
    integer(INT_KIND),intent(in)              :: glen
    integer(INT_KIND),intent(in)              :: from_pe
    integer(INT_KIND),intent(in),optional     :: tag
    integer(INT_KIND),intent(inout),optional  :: request
    logical(INT_KIND),intent(in),optional     :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block

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

    call mpp_recv_c_style(get_data,glen,from_pe,MPP_REAL32,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_r32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real data.
! subroutine mpp_recv_r64s(get_data,from_pe,tag,request,block)
!   use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
!   real(c_double),intent(inout)             :: get_data
!   integer(INT_KIND),intent(in)             :: from_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request
!   logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
!   integer(c_int32_t) :: comm_tag
!   integer(c_int32_t) :: comm_block

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = tag
!   else
!       comm_tag = DEFAULT_TAG
!   endif

   !<Set the value of comm_block.
!   comm_block = MPP_C_TRUE
!   if (present(block)) then
!       if (.not. block) then
!           comm_block = MPP_C_FALSE
!       endif
!   endif

!   call mpp_recv_c_style(get_data,1,from_pe,MPP_REAL64,comm_tag, &
!                         comm_block,request)

! end subroutine mpp_recv_r64s

  subroutine mpp_recv_r64(get_data,glen,from_pe,tag,request,block)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),dimension(..),intent(inout) :: get_data
    integer(INT_KIND),intent(in)               :: glen
    integer(INT_KIND),intent(in)               :: from_pe
    integer(INT_KIND),intent(in),optional      :: tag
    integer(INT_KIND),intent(inout),optional   :: request
    logical(INT_KIND),intent(in),optional      :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block

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

    call mpp_recv_c_style(get_data,glen,from_pe,MPP_REAL64,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_r64

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit logical data.
! subroutine mpp_recv_l32s(get_data,from_pe,tag,request,block)
!   use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
!   logical(INT_KIND),intent(inout)          :: get_data
!   integer(INT_KIND),intent(in)             :: from_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request
!   logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
!   integer(c_int32_t) :: comm_tag
!   integer(c_int32_t) :: comm_block

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = tag
!   else
!       comm_tag = DEFAULT_TAG
!   endif

   !<Set the value of comm_block.
!   comm_block = MPP_C_TRUE
!   if (present(block)) then
!       if (.not. block) then
!           comm_block = MPP_C_FALSE
!       endif
!   endif

!   call mpp_recv_c_style(get_data,1,from_pe,MPP_INT32,comm_tag, &
!                         comm_block,request)

! end subroutine mpp_recv_l32s

  subroutine mpp_recv_l32(get_data,glen,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    logical(INT_KIND),dimension(..),intent(inout) :: get_data
    integer(INT_KIND),intent(in)                  :: glen
    integer(INT_KIND),intent(in)                  :: from_pe
    integer(INT_KIND),intent(in),optional         :: tag
    integer(INT_KIND),intent(inout),optional      :: request
    logical(INT_KIND),intent(in),optional         :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block

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

    call mpp_recv_c_style(get_data,glen,from_pe,MPP_INT32,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_l32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit logical data.
! subroutine mpp_recv_l64s(get_data,from_pe,tag,request,block)
!   use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
!   logical(LONG_KIND),intent(inout)         :: get_data
!   integer(INT_KIND),intent(in)             :: from_pe
!   integer(INT_KIND),intent(in),optional    :: tag
!   integer(INT_KIND),intent(inout),optional :: request
!   logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
!   integer(c_int32_t) :: comm_tag
!   integer(c_int32_t) :: comm_block

   !<Set the value of comm_tag.
!   if (present(tag)) then
!       comm_tag = tag
!   else
!       comm_tag = DEFAULT_TAG
!   endif

   !<Set the value of comm_block.
!   comm_block = MPP_C_TRUE
!   if (present(block)) then
!       if (.not. block) then
!           comm_block = MPP_C_FALSE
!       endif
!   endif

!   call mpp_recv_c_style(get_data,1,from_pe,MPP_INT64,comm_tag, &
!                         comm_block,request)

! end subroutine mpp_recv_l64s

  subroutine mpp_recv_l64(get_data,glen,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    logical(LONG_KIND),dimension(..),intent(inout) :: get_data
    integer(INT_KIND),intent(in)                   :: glen
    integer(INT_KIND),intent(in)                   :: from_pe
    integer(INT_KIND),intent(in),optional          :: tag
    integer(INT_KIND),intent(inout),optional       :: request
    logical(INT_KIND),intent(in),optional          :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block

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

    call mpp_recv_c_style(get_data,glen,from_pe,MPP_INT64,comm_tag, &
                          comm_block,request)

  end subroutine mpp_recv_l64

 !-----------------------------------------------------------------------------
 !>Receive data from another rank on the world pelist.
  subroutine mpp_recv_c_style(get_data,get_len,from_pe,mpp_type,comm_tag, &
                              comm_block,request)
    use iso_c_binding, only: c_int32_t,c_int,c_size_t

   !<Inputs/Outputs
    type(*),dimension(..),intent(inout)      :: get_data
    integer(INT_KIND),intent(in)             :: get_len
    integer(INT_KIND),intent(in)             :: from_pe
    integer(c_int32_t),intent(in)            :: mpp_type
    integer(c_int32_t),intent(in)            :: comm_tag
    integer(c_int32_t),intent(in)            :: comm_block
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int) :: null_request

    if (present(request)) then
        call iso_c_mpp_recv(get_data,int(get_len,kind=c_size_t), &
                            int(from_pe,kind=c_int32_t),mpp_type, &
                            comm_tag,request, &
                            comm_block,mpp_context)
    else
        null_request = MPP_C_NULL_REQUEST
        call iso_c_mpp_recv(get_data,int(get_len,kind=c_size_t), &
                            int(from_pe,kind=c_int32_t),mpp_type, &
                            comm_tag,null_request,comm_block,mpp_context)
    endif

  end subroutine mpp_recv_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
