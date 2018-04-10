 !-----------------------------------------------------------------------------
 !>mpp_broadcast template for scalar data.
  subroutine _S_NAME(bcast_data,from_pe,pelist)
    use iso_c_binding, only: c_int32_t,c_loc _C_TYPE

   !<Inputs/Outputs
    _MPP_TYPE,target,intent(inout)                      :: bcast_data
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_broadcast_c_style(c_loc(bcast_data),1,from_pe,_MPP_PARAM, &
                                   size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_broadcast_c_style(c_loc(bcast_data),1,from_pe,_MPP_PARAM, &
                                   1,null_pelist)
    endif

  end subroutine _S_NAME

 !-----------------------------------------------------------------------------

