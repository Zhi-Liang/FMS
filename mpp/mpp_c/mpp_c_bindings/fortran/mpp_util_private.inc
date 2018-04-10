 !*****************************************************************************
 !Private mpp utility routines.

 !-----------------------------------------------------------------------------
 !>Function that converts any supported scalar type to a string.
  function arg_to_string_scalar(arg) result(tmp_str)

   !<Inputs/Outputs
    class(*),intent(in) :: arg     !<Scalar argument.
    character(len=256)  :: tmp_str !<Argument as a string.

   !<Convert the argument to a string.
    tmp_str = ''
    select type(arg)
    type is (integer(INT_KIND))
        write(tmp_str,'(i16)') arg
    type is (integer(LONG_KIND))
        write(tmp_str,'(i16)') arg
    type is (real(FLOAT_KIND))
        write(tmp_str,'(g16.9)') arg
    type is (real(DOUBLE_KIND))
        write(tmp_str,'(g16.9)') arg
    class default
        call mpp_error(FATAL,'ARG_TO_STRING_SCALAR: unsupported type.')
    end select

  end function arg_to_string_scalar

 !-----------------------------------------------------------------------------
 !>Function that converts any supported one-dimensional type to a string.
  function arg_to_string_1D(arg) result(tmp_str)

   !<Inputs/Outputs
    class(*),dimension(:),intent(in) :: arg     !<One-dimensional argument.
    character(len=256)               :: tmp_str !<Argument as a string.

   !<Local variables
    character(len=32) :: dum         !<Local dummy string.
    integer(INT_KIND) :: tmp_str_len !<Occupied length of tmp_str
    integer(INT_KIND) :: dum_len     !<Occupied lenght of dum
    integer(INT_KIND) :: i           !<Loop variable.

   !<Convert the argument to a string.  Place a comma between subsequent
   !!elements of arg.
    tmp_str = ''
    do i = 1,size(arg)
        select type(arg)
        type is (integer(INT_KIND))
            write(dum,'(i16)') arg(i)
        type is (integer(LONG_KIND))
            write(dum,'(i16)') arg(i)
        type is (real(FLOAT_KIND))
            write(dum,'(g16.9)') arg(i)
        type is (real(DOUBLE_KIND))
            write(dum,'(g16.9)') arg(i)
        class default
            call mpp_error(FATAL,'ARG_TO_STRING_1D: unsupported type.')
        end select
        dum_len = len_trim(dum)
        tmp_str_len = len_trim(tmp_str)
        tmp_str(tmp_str_len+1:tmp_str_len+dum_len) = trim(dum)
        tmp_str(tmp_str_len+dum_len+1:tmp_str_len+dum_len+1) = ','
    enddo

   !<Remove the trailing comma.
    tmp_str_len = len_trim(tmp_str)
    tmp_str(tmp_str_len:tmp_str_len) = ' '

  end function arg_to_string_1D

 !-----------------------------------------------------------------------------

 !*****************************************************************************
