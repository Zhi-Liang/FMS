 !*****************************************************************************
 !Public facing mpp error routines.

 !NOTE: The variable mpp_context is private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Subroutine that handles mpp error messages for inputted scalar arguments.
  subroutine mpp_error_scalar(error_type,arg1,arg2,arg3,arg4,arg5)
    use iso_c_binding, only: c_char,c_null_char,c_int32_t,c_size_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)         :: error_type !<Type of error.
    character(len=*),intent(in),optional :: arg1       !<Optional message/data.
    class(*),intent(in),optional         :: arg2       !<Optional message/data.
    character(len=*),intent(in),optional :: arg3       !<Optional message/data.
    class(*),intent(in),optional         :: arg4       !<Optional message/data.
    character(len=*),intent(in),optional :: arg5       !<Optional message/data.

   !<Local variables
    character(kind=c_char),dimension(1024) :: err_mesg !<Error message.
    character(len=512)                     :: tmp_str  !<Temporary dummy string.
    integer(INT_KIND)                      :: counter
    integer(INT_KIND)                      :: i

   !<Initialize the error message array.
    do i = 1,size(err_mesg)
        err_mesg(i) = c_null_char
    enddo

   !<Add all included messages/data to the error message.
    counter = 0
    if (present(arg1)) then
        do i = 1,len_trim(arg1)
            if (counter .ge. 1023) then
                exit
            endif
            counter = counter + 1
            err_mesg(counter) = arg1(i:i)
        enddo
    endif
    if (present(arg2)) then
        tmp_str = arg_to_string_scalar(arg2)
        do i = 1,len_trim(tmp_str)
            if (counter .ge. 1023) then
                exit
            endif
            counter = counter + 1
            err_mesg(counter) = tmp_str(i:i)
        enddo
    endif
    if (present(arg3)) then
        do i = 1,len_trim(arg3)
            if (counter .ge. 1023) then
                exit
            endif
            counter = counter + 1
            err_mesg(counter) = arg3(i:i)
        enddo
    endif
    if (present(arg4)) then
        tmp_str = arg_to_string_scalar(arg4)
        do i = 1,len_trim(tmp_str)
            if (counter .ge. 1023) then
                exit
            endif
            counter = counter + 1
            err_mesg(counter) = tmp_str(i:i)
        enddo
    endif
    if (present(arg5)) then
        do i = 1,len_trim(arg5)
            if (counter .ge. 1023) then
                exit
            endif
            counter = counter + 1
            err_mesg(counter) = arg5(i:i)
        enddo
    endif

   !<Write out the error message.
    call iso_c_mpp_error(int(error_type,kind=c_int32_t),err_mesg, &
                         int(counter,kind=c_size_t),mpp_context)

  end subroutine mpp_error_scalar

 !-----------------------------------------------------------------------------
 !>Wrapper for calls with up to two inputted one-dimensional array arguments.
  subroutine mpp_error_1D(error_type,arg1,arg2,arg3,arg4,arg5)

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)              :: error_type !<Type of error.
    character(len=*),intent(in)               :: arg1       !<Optional message/data.
    class(*),dimension(:),intent(in)          :: arg2       !<Optional message/data.
    character(len=*),intent(in),optional      :: arg3       !<Optional message/data.
    class(*),dimension(:),intent(in),optional :: arg4       !<Optional message/data.
    character(len=*),intent(in),optional      :: arg5       !<Optional message/data.

   !<Local variables
    character(len=1024) :: err_mesg !<Error message.
    character(len=512)  :: tmp_str  !<Temporary dummy string.

   !<Add all included messages/data to the error message.
    tmp_str = arg_to_string_1D(arg2)
    err_mesg = trim(arg1)//trim(tmp_str)
    if (present(arg3)) then
        err_mesg = trim(err_mesg)//trim(arg3)
    endif
    if (present(arg4)) then
        tmp_str = arg_to_string_1D(arg4)
        err_mesg = trim(err_mesg)//trim(tmp_str)
    endif
    if (present(arg5)) then
        err_mesg = trim(err_mesg)//trim(arg5)
    endif

    call mpp_error_scalar(error_type,trim(err_mesg))

  end subroutine mpp_error_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for calls with a scalar and a one-dimensional array argument.
  subroutine mpp_error_scalar_1D(error_type,arg1,arg2,arg3,arg4,arg5)

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)         :: error_type !<Type of error.
    character(len=*),intent(in)          :: arg1       !<Optional message/data.
    class(*),intent(in)                  :: arg2       !<Optional message/data.
    character(len=*),intent(in)          :: arg3       !<Optional message/data.
    class(*),dimension(:),intent(in)     :: arg4       !<Optional message/data.
    character(len=*),intent(in),optional :: arg5       !<Optional message/data.

   !<Local variables
    character(len=1024) :: err_mesg !<Error message.
    character(len=512)  :: tmp_str1 !<Temporary dummy string.
    character(len=512)  :: tmp_str2 !<Temporary dummy string.

   !<Add all included messages/data to the error message.
    tmp_str1 = arg_to_string_scalar(arg2)
    tmp_str2 = arg_to_string_1D(arg4)
    err_mesg = trim(arg1)//trim(tmp_str1)//trim(arg3)//trim(tmp_str2)
    if (present(arg5)) then
        err_mesg = trim(err_mesg)//trim(arg5)
    endif

    call mpp_error_scalar(error_type,trim(err_mesg))

  end subroutine mpp_error_scalar_1D

 !-----------------------------------------------------------------------------
 !>Wrapper for calls with a one-dimensional array and a scalar argument.
  subroutine mpp_error_1D_scalar(error_type,arg1,arg2,arg3,arg4,arg5)

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)         :: error_type !<Type of error.
    character(len=*),intent(in)          :: arg1       !<Optional message/data.
    class(*),dimension(:),intent(in)     :: arg2       !<Optional message/data.
    character(len=*),intent(in)          :: arg3       !<Optional message/data.
    class(*),intent(in)                  :: arg4       !<Optional message/data.
    character(len=*),intent(in),optional :: arg5       !<Optional message/data.

   !<Local variables
    character(len=1024) :: err_mesg !<Error message.
    character(len=512)  :: tmp_str1 !<Temporary dummy string.
    character(len=512)  :: tmp_str2 !<Temporary dummy string.

   !<Add all included messages/data to the error message.
    tmp_str1 = arg_to_string_1D(arg2)
    tmp_str2 = arg_to_string_scalar(arg4)
    err_mesg = trim(arg1)//trim(tmp_str1)//trim(arg3)//trim(tmp_str2)
    if (present(arg5)) then
        err_mesg = trim(err_mesg)//trim(arg5)
    endif

    call mpp_error_scalar(error_type,trim(err_mesg))

  end subroutine mpp_error_1D_scalar

 !-----------------------------------------------------------------------------
 !>Wrapper for error_mesg style calls.
  subroutine mpp_error_mesg(routine,message,error_type)

   !<Inputs/Outputs
    character(len=*),intent(in)  :: routine
    character(len=*),intent(in)  :: message
    integer(INT_KIND),intent(in) :: error_type

   !<Write out the error message.
    call mpp_error_scalar(error_type,trim(routine),trim(message))

  end subroutine mpp_error_mesg

 !-----------------------------------------------------------------------------
 !>Wrapper for calls with no arguments.
  subroutine mpp_error_no_args()

    call mpp_error_scalar(FATAL)

  end subroutine mpp_error_no_args

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
