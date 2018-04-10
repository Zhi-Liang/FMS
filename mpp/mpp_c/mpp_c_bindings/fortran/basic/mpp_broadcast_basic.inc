 !-----------------------------------------------------------------------------
 !>Wrapper for character data
  subroutine mpp_broadcast_char(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_char,c_int32_t,c_loc

   !<Inputs/Outputs
    character(len=*),dimension(:),target,intent(inout)  :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(INT_KIND)                                      :: str_len
    integer(INT_KIND)                                      :: buffer_size
    character(kind=c_char),dimension(:),allocatable,target :: buffer
    character(kind=c_char,len=len(bcast_data(1))),pointer  :: tmp_str
    integer(c_int32_t),dimension(1)                        :: null_pelist
    integer(INT_KIND)                                      :: i
    integer(INT_KIND)                                      :: j

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
        call mpp_broadcast_c_style(c_loc(buffer),buffer_size,from_pe,MPP_CHAR, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(c_loc(buffer),buffer_size,from_pe,MPP_CHAR, &
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
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style_i32a(bcast_data,bcast_len,from_pe,mpp_type, &
                                        n,pelist)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int32_t),dimension(*),target,intent(inout) :: bcast_data
    integer(INT_KIND),intent(in)                         :: bcast_len
    integer(c_int32_t),intent(in)                        :: from_pe
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist

    call mpp_broadcast_c_style(c_loc(bcast_data),bcast_len,from_pe,mpp_type, &
                               n,pelist)

  end subroutine mpp_broadcast_c_style_i32a

 !-----------------------------------------------------------------------------
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style_i64a(bcast_data,bcast_len,from_pe,mpp_type, &
                                        n,pelist)
    use iso_c_binding, only: c_int64_t,c_int32_t,c_loc

   !<Inputs/Outputs
    integer(c_int64_t),dimension(*),target,intent(inout) :: bcast_data
    integer(INT_KIND),intent(in)                         :: bcast_len
    integer(c_int32_t),intent(in)                        :: from_pe
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist

    call mpp_broadcast_c_style(c_loc(bcast_data),bcast_len,from_pe,mpp_type, &
                               n,pelist)

  end subroutine mpp_broadcast_c_style_i64a

 !-----------------------------------------------------------------------------
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style_r32a(bcast_data,bcast_len,from_pe,mpp_type, &
                                        n,pelist)
    use iso_c_binding, only: c_float,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_float),dimension(*),target,intent(inout) :: bcast_data
    integer(INT_KIND),intent(in)                    :: bcast_len
    integer(c_int32_t),intent(in)                   :: from_pe
    integer(c_int32_t),intent(in)                   :: mpp_type
    integer(INT_KIND),intent(in)                    :: n
    integer(c_int32_t),dimension(n),intent(in)      :: pelist

    call mpp_broadcast_c_style(c_loc(bcast_data),bcast_len,from_pe,mpp_type, &
                               n,pelist)

  end subroutine mpp_broadcast_c_style_r32a

 !-----------------------------------------------------------------------------
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style_r64a(bcast_data,bcast_len,from_pe,mpp_type, &
                                        n,pelist)
    use iso_c_binding, only: c_double,c_int32_t,c_loc

   !<Inputs/Outputs
    real(c_double),dimension(*),target,intent(inout) :: bcast_data
    integer(INT_KIND),intent(in)                     :: bcast_len
    integer(c_int32_t),intent(in)                    :: from_pe
    integer(c_int32_t),intent(in)                    :: mpp_type
    integer(INT_KIND),intent(in)                     :: n
    integer(c_int32_t),dimension(n),intent(in)       :: pelist

    call mpp_broadcast_c_style(c_loc(bcast_data),bcast_len,from_pe,mpp_type, &
                               n,pelist)

  end subroutine mpp_broadcast_c_style_r64a

 !-----------------------------------------------------------------------------
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style_l32a(bcast_data,bcast_len,from_pe,mpp_type, &
                                        n,pelist)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    logical(INT_KIND),dimension(*),target,intent(inout) :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),intent(in)                       :: mpp_type
    integer(INT_KIND),intent(in)                        :: n
    integer(c_int32_t),dimension(n),intent(in)          :: pelist

    call mpp_broadcast_c_style(c_loc(bcast_data),bcast_len,from_pe,mpp_type, &
                               n,pelist)

  end subroutine mpp_broadcast_c_style_l32a

 !-----------------------------------------------------------------------------
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style_l64a(bcast_data,bcast_len,from_pe,mpp_type, &
                                        n,pelist)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    logical(LONG_KIND),dimension(*),target,intent(inout) :: bcast_data
    integer(INT_KIND),intent(in)                         :: bcast_len
    integer(c_int32_t),intent(in)                        :: from_pe
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist

    call mpp_broadcast_c_style(c_loc(bcast_data),bcast_len,from_pe,mpp_type, &
                               n,pelist)

  end subroutine mpp_broadcast_c_style_l64a

 !-----------------------------------------------------------------------------
 !>Broadcast the data to the rest of ranks on the pelist.
  subroutine mpp_broadcast_c_style(bcast_data,bcast_len,from_pe,mpp_type, &
                                   n,pelist)
    use iso_c_binding, only: c_ptr,c_int32_t,c_size_t

   !<Inputs/Outputs
    type(c_ptr)                                :: bcast_data
    integer(INT_KIND),intent(in)               :: bcast_len
    integer(c_int32_t),intent(in)              :: from_pe
    integer(c_int32_t),intent(in)              :: mpp_type
    integer(INT_KIND),intent(in)               :: n
    integer(c_int32_t),dimension(n),intent(in) :: pelist

    call iso_c_mpp_broadcast(bcast_data,int(bcast_len,kind=c_size_t),from_pe, &
                             mpp_type,pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_broadcast_c_style

 !-----------------------------------------------------------------------------

