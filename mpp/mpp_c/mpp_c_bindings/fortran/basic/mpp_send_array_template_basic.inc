 !-----------------------------------------------------------------------------
 !>mpp_send template for array data.
  subroutine _A_NAME(put_data,plen,to_pe,tag,request)
    use iso_c_binding, only: c_int32_t _C_TYPE

   !<Inputs/Outputs
    _MPP_TYPE,_DIMENSION,intent(inout)       :: put_data
    integer(INT_KIND),intent(in)             :: plen
    integer(c_int32_t),intent(in)            :: to_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request

   !<Local variables
    integer(c_int32_t) :: comm_tag

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

    call _A_SUB(put_data,plen,to_pe,_MPP_PARAM,comm_tag,request)

  end subroutine _A_NAME

 !-----------------------------------------------------------------------------

