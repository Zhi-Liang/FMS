 !*****************************************************************************
 !Public facing mpp_broadcast routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for character data
  subroutine mpp_broadcast_char(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_char,c_int32_t

   !<Inputs/Outputs
    character(len=*),dimension(:),target,intent(inout)  :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(INT_KIND)                                     :: str_len
    integer(INT_KIND)                                     :: buffer_size
    character(kind=c_char),dimension(:),allocatable       :: buffer
    character(kind=c_char,len=len(bcast_data(1))),pointer :: tmp_str
    integer(c_int32_t),dimension(1)                       :: null_pelist
    integer(INT_KIND)                                     :: i
    integer(INT_KIND)                                     :: j

   !<Copy the character array into a c interoperable buffer.
    str_len = len(bcast_data(1))
    buffer_size = str_len*bcast_len
    allocate(buffer(buffer_size))
    do i = 1,bcast_len
        tmp_str => bcast_data(i)
        do j = 1,str_len
            buffer((i-1)*str_len+j) = tmp_str(j:j)
        enddo
    enddo

    if (present(pelist)) then
        call mpp_broadcast_c_style(buffer,buffer_size,from_pe,MPP_CHAR, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(buffer,buffer_size,from_pe,MPP_CHAR, &
                                   1,null_pelist)
    endif

   !<Copy the c interoperable buffer back into the character array.
    do i = 1,bcast_len
        tmp_str => bcast_data(i)
        do j = 1,str_len
            tmp_str(j:j) = buffer((i-1)*str_len+j)
        enddo
    enddo

   !<Deallocate local allocatables.
    deallocate(buffer)

   !<Nullify local pointers.
    tmp_str => NULL()

  end subroutine mpp_broadcast_char

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer data.
  subroutine mpp_broadcast_i32s(bcast_data,from_pe,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),intent(inout)                    :: bcast_data
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_INT32, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_INT32, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_i32s

  subroutine mpp_broadcast_i32(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(c_int32_t),dimension(..),intent(inout)      :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_INT32, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_INT32, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_i32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit integer data.
  subroutine mpp_broadcast_i64s(bcast_data,from_pe,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
    integer(c_int64_t),intent(inout)                    :: bcast_data
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_INT64, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_INT64, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_i64s

  subroutine mpp_broadcast_i64(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t

   !<Inputs/Outputs
    integer(c_int64_t),dimension(..),intent(inout)      :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_INT64, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_INT64, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_i64

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real data.
  subroutine mpp_broadcast_r32s(bcast_data,from_pe,pelist)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),intent(inout)                         :: bcast_data
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_REAL32, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_REAL32, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_r32s

  subroutine mpp_broadcast_r32(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    real(c_float),dimension(..),intent(inout)           :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_REAL32, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_REAL32, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_r32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real data.
  subroutine mpp_broadcast_r64s(bcast_data,from_pe,pelist)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),intent(inout)                        :: bcast_data
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_REAL64, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_REAL64, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_r64s

  subroutine mpp_broadcast_r64(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    real(c_double),dimension(..),intent(inout)          :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_REAL64, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_REAL64, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_r64

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit logical data.
  subroutine mpp_broadcast_l32s(bcast_data,from_pe,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    logical(INT_KIND),intent(inout)                     :: bcast_data
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_INT32, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_INT32, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_l32s

  subroutine mpp_broadcast_l32(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    logical(INT_KIND),dimension(..),intent(inout)       :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_INT32, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_INT32, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_l32

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit logical data.
  subroutine mpp_broadcast_l64s(bcast_data,from_pe,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    logical(LONG_KIND),intent(inout)                    :: bcast_data
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_INT64, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,1,from_pe,MPP_INT64, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_l64s

  subroutine mpp_broadcast_l64(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    logical(LONG_KIND),dimension(..),intent(inout)      :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_INT64, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,MPP_INT64, &
                                   1,null_pelist)
    endif

  end subroutine mpp_broadcast_l64

 !-----------------------------------------------------------------------------
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,mpp_type, &
                                   n,pelist)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    type(*),dimension(..),intent(inout)        :: bcast_data
    integer(INT_KIND),intent(in)               :: bcast_len
    integer(c_int32_t),intent(in)              :: from_pe
    integer(c_int32_t),intent(in)              :: mpp_type
    integer(INT_KIND),intent(in)               :: n
    integer(c_int32_t),dimension(n),intent(in) :: pelist

    call iso_c_mpp_broadcast(bcast_data,int(bcast_len,kind=c_size_t),from_pe, &
                             mpp_type,pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_broadcast_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
