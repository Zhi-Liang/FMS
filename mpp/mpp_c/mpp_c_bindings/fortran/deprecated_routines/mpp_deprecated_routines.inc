 !*****************************************************************************
 !Public facing mpp routines that are now deprecated.

 !-----------------------------------------------------------------------------
  function stdin() result(stdin_unit)
    use,intrinsic :: iso_fortran_env, only: input_unit
    integer(INT_KIND) :: stdin_unit
    stdin_unit = input_unit
  end function stdin

 !-----------------------------------------------------------------------------
  function mpp_error_state() result(mpp_error_state_value)
    integer(INT_KIND) :: mpp_error_state_value
    mpp_error_state_value = -1
  end function mpp_error_state

 !-----------------------------------------------------------------------------
  subroutine mpp_set_warn_level(flag)
    integer(INT_KIND),intent(in) :: flag
    integer(INT_KIND)            :: dum
    dum = flag
  end subroutine mpp_set_warn_level

 !-----------------------------------------------------------------------------
  subroutine mpp_set_stack_size(n)
    integer(INT_KIND),intent(in) :: n
    integer(INT_KIND)            :: dum
    dum = n
  end subroutine mpp_set_stack_size

 !-----------------------------------------------------------------------------
  function mpp_node() result(dum)
    integer(INT_KIND) :: dum
    dum = -1
  end function mpp_node

 !-----------------------------------------------------------------------------
  subroutine mpp_malloc(ptr,newlen,len)
    integer(LONG_KIND),intent(in)   :: ptr
    integer(INT_KIND),intent(in)    :: newlen
    integer(INT_KIND),intent(inout) :: len
    len = -1*int(ptr,kind=INT_KIND)*newlen
  end subroutine mpp_malloc

 !-----------------------------------------------------------------------------
  subroutine mpp_gsm_malloc()
  end subroutine mpp_gsm_malloc

 !-----------------------------------------------------------------------------
  subroutine mpp_gsm_free()
  end subroutine mpp_gsm_free

 !-----------------------------------------------------------------------------

 !*****************************************************************************
