 !*****************************************************************************
 !Public facing mpp_sync routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#if !defined(_GNU_FBINDINGS) && !defined(_INTEL16_FBINDINGS)
 !-----------------------------------------------------------------------------
 !>Synchronize all ranks in a pelist at a barrier.
  subroutine mpp_sync(pelist,do_self)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    integer(INT_KIND),dimension(:),intent(in),optional :: pelist
    logical(INT_KIND),intent(in),optional              :: do_self

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sync_c_style(size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call iso_c_mpp_sync(null_pelist,int(1,kind=c_size_t),mpp_context)
    endif

  end subroutine mpp_sync

  subroutine mpp_sync_c_style(n,pelist)
    use iso_c_binding, only: c_size_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist

    call iso_c_mpp_sync(pelist,int(n,kind=c_size_t),mpp_context)

  end subroutine mpp_sync_c_style

 !-----------------------------------------------------------------------------
 !>Complete outstanding send or receive requests.
  subroutine mpp_sync_self(pelist,check,request,msg_size,msg_type)
    use iso_c_binding, only: c_int32_t,c_int,c_size_t
!   use mpi

   !<Inputs/Outputs
    integer(INT_KIND),dimension(:),intent(in),optional    :: pelist
    integer(INT_KIND),intent(in),optional                 :: check
    integer(INT_KIND),dimension(:),intent(inout),optional :: request
    integer(INT_KIND),dimension(:),intent(in),optional    :: msg_size
    integer(INT_KIND),dimension(:),intent(in),optional    :: msg_type

   !<Local variables
    integer(c_int32_t)                          :: comm_check
    integer(c_int),dimension(1)                 :: null_request
    integer(c_size_t),dimension(1)              :: null_msg_size
    integer(c_int32_t),dimension(1)             :: null_msg_type
    integer(INT_KIND),dimension(:),allocatable  :: tmp_null_msg_size
    integer(INT_KIND),dimension(:),allocatable  :: tmp_null_msg_type
    integer(c_int32_t),dimension(:),allocatable :: tmp_msg_type
    integer(INT_KIND)                           :: i

   !<Set the value of comm_check.
    comm_check = EVENT_SEND
    if (present(check)) then
        if (check .ne. EVENT_SEND .and. check .ne. EVENT_RECV) then
            call mpp_error(FATAL,'MPP_SYNC_SELF: Invalid value for check.' &
                                 //' Check must be equal to either' &
                                 //' EVENT_SEND or EVENT_RECV.')
        else
            comm_check = check
        endif
    endif

   !<Make sure all necessary data is present.
    if (present(request)) then
        if (.not. present(check)) then
            call mpp_error(FATAL,'MPP_SYNC_SELF: check must be present' &
                                 //' if request is present.')
        else
            if (check .eq. EVENT_RECV) then
                if (.not. present(msg_size) .or. .not. present(msg_type)) then
                    call mpp_error(FATAL,'MPP_SYNC_SELF: msg_size and' &
                                         //' msg_type must be present if' &
                                         //' request is present and check' &
                                         //' equals EVENT_RECV.')
                endif
            elseif (check .eq. EVENT_SEND) then
                if (present(msg_size) .or. present(msg_type)) then
                    call mpp_error(FATAL,'MPP_SYNC_SELF: msg_size and' &
                                         //' msg_type are not used when' &
                                         //' request is present and check' &
                                         //' equals EVENT_SEND.')
                endif
            endif
        endif
    else
        if (present(msg_size) .or. present(msg_type)) then
            call mpp_error(FATAL,'MPP_SYNC_SELF: msg_size and msg_type' &
                                 //' are not used when request is not' &
                                 //' present.')
        endif
    endif

    if (present(msg_type)) then
        allocate(tmp_msg_type(size(msg_type)))
        do i = 1,size(msg_type)
            if (msg_type(i) .eq. MPP_INT32) then
                tmp_msg_type(i) = msg_type(i)
            elseif (msg_type(i) .eq. MPP_INT64) then
                tmp_msg_type(i) = msg_type(i)
            elseif (msg_type(i) .eq. MPP_REAL32) then
                tmp_msg_type(i) = msg_type(i)
            elseif (msg_type(i) .eq. MPP_REAL64) then
                tmp_msg_type(i) = msg_type(i)
            elseif (msg_type(i) .eq. MPP_CHAR) then
                tmp_msg_type(i) = msg_type(i)
!           elseif (msg_type(i) .eq. MPI_INTEGER4) then
!               tmp_msg_type(i) = MPP_INT32
!           elseif (msg_type(i) .eq. MPI_INTEGER8) then
!               tmp_msg_type(i) = MPP_INT64
!           elseif (msg_type(i) .eq. MPI_REAL4) then
!               tmp_msg_type(i) = MPP_REAL32
!           elseif (msg_type(i) .eq. MPI_REAL8) then
!               tmp_msg_type(i) = MPP_REAL64
!           elseif (msg_type(i) .eq. MPI_CHARACTER) then
!               tmp_msg_type(i) = MPP_CHAR
            else
                call mpp_error(FATAL,'MPP_SYNC_SELF: unsupported msg type.')
            endif
        enddo
    endif

   !<Call the c-bound mpp_sync_self routine.
    if (present(request)) then
        if (check .eq. EVENT_SEND) then
            allocate(tmp_null_msg_size(size(request)))
            allocate(tmp_null_msg_type(size(request)))
            tmp_null_msg_size = MPP_C_NULL_MSG_SIZE
            tmp_null_msg_type = MPP_C_NULL_MSG_TYPE
            call mpp_sync_self_c_style(comm_check,size(request),request, &
                                       tmp_null_msg_size,tmp_null_msg_type)
            deallocate(tmp_null_msg_size)
            deallocate(tmp_null_msg_type)
        else
            call mpp_sync_self_c_style(comm_check,size(request),request, &
                                       msg_size,tmp_msg_type)
        endif
    else
        null_request(1) = MPP_C_NULL_REQUEST
        null_msg_size(1) = MPP_C_NULL_MSG_SIZE
        null_msg_type(1) = MPP_C_NULL_MSG_TYPE
        call iso_c_mpp_sync_self(comm_check,null_request, &
                                 int(1,kind=c_size_t),null_msg_size, &
                                 null_msg_type,mpp_context)
    endif

    if (present(msg_type)) then
        deallocate(tmp_msg_type)
    endif

  end subroutine mpp_sync_self

  subroutine mpp_sync_self_c_style(comm_check,n,request,msg_size,msg_type)
    use iso_c_binding, only: c_int32_t,c_int,c_size_t

   !<Inputs/Outputs
    integer(c_int32_t),intent(in)                :: comm_check
    integer(INT_KIND),intent(in)                 :: n
    integer(INT_KIND),dimension(n),intent(inout) :: request
    integer(INT_KIND),dimension(n),intent(in)    :: msg_size
    integer(INT_KIND),dimension(n),intent(in)    :: msg_type

   !<Local variables
    integer(c_int),dimension(n)     :: tmp_request
    integer(c_size_t),dimension(n)  :: tmp_msg_size
    integer(c_size_t),dimension(1)  :: null_msg_size
    integer(c_int32_t),dimension(1) :: null_msg_type
    integer(INT_KIND)               :: i

    do i = 1,n
        tmp_request(i) = int(request(i),kind=c_int)
        tmp_msg_size(i) = int(msg_size(i),kind=c_size_t)
    enddo

    if (comm_check .eq. EVENT_SEND) then
        null_msg_size(1) = int(MPP_C_NULL_MSG_SIZE,kind=c_size_t)
        null_msg_type(1) = int(MPP_C_NULL_MSG_TYPE,kind=c_int32_t)
        call iso_c_mpp_sync_self(comm_check,tmp_request, &
                                 int(size(request),kind=c_size_t), &
                                 null_msg_size,null_msg_type,mpp_context)
    elseif (comm_check .eq. EVENT_RECV) then
        call iso_c_mpp_sync_self(comm_check,tmp_request, &
                                 int(size(request),kind=c_size_t), &
                                 tmp_msg_size,msg_type, &
                                 mpp_context)
    endif

    do i = 1,n
        request(i) = int(tmp_request(i),kind=INT_KIND)
    enddo

  end subroutine mpp_sync_self_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
