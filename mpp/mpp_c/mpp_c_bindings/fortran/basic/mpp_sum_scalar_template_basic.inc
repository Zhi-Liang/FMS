 !-----------------------------------------------------------------------------
 !>mpp_sum template for scalar data.
  subroutine _S_NAME(sum_data,pelist)
    use iso_c_binding, only: c_int32_t,c_loc _C_TYPE

   !<Inputs/Outputs
    _MPP_TYPE,target,intent(inout)                      :: sum_data
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call mpp_sum_c_style(c_loc(sum_data),1,_MPP_PARAM,size(pelist), &
                             pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call mpp_sum_c_style(c_loc(sum_data),1,_MPP_PARAM,1, &
                             null_pelist)
    endif

  end subroutine _S_NAME

 !-----------------------------------------------------------------------------

