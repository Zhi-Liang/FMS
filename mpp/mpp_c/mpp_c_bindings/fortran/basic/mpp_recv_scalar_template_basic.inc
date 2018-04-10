 !-----------------------------------------------------------------------------
 !>mpp_recv template for scalar data.
  subroutine _S_NAME(get_data,glen,from_pe,tag,request,block)
    use iso_c_binding, only: c_int32_t,c_loc _C_TYPE

   !<Inputs/Outputs
    _MPP_TYPE,target,intent(inout)           :: get_data
    integer(INT_KIND),intent(in)             :: glen
    integer(c_int32_t),intent(in)            :: from_pe
    integer(INT_KIND),intent(in),optional    :: tag
    integer(INT_KIND),intent(inout),optional :: request
    logical(INT_KIND),intent(in),optional    :: block

   !<Local variables
    integer(c_int32_t) :: comm_tag
    integer(c_int32_t) :: comm_block

   !<Set the value of comm_tag.
    if (present(tag)) then
        comm_tag = int(tag,kind=c_int32_t)
    else
        comm_tag = int(DEFAULT_TAG,kind=c_int32_t)
    endif

   !<Set the value of comm_block.
    comm_block = MPP_C_TRUE
    if (present(block)) then
        if (.not. block) then
            comm_block = MPP_C_FALSE
        endif
    endif

    call mpp_recv_c_style(c_loc(get_data),glen,from_pe,_MPP_PARAM, &
                          comm_tag,comm_block,request)

  end subroutine _S_NAME

 !-----------------------------------------------------------------------------

