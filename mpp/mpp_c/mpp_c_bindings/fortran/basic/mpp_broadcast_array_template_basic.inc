 !-----------------------------------------------------------------------------
 !>mpp_broadcast template for array data.
  subroutine _A_NAME(bcast_data,bcast_len,from_pe,pelist)
    use iso_c_binding, only: c_int32_t _C_TYPE

   !<Inputs/Outputs
    _MPP_TYPE,_DIMENSION,intent(inout)                  :: bcast_data
    integer(INT_KIND),intent(in)                        :: bcast_len
    integer(c_int32_t),intent(in)                       :: from_pe
    integer(c_int32_t),dimension(:),intent(in),optional :: pelist

   !<Local variables
    integer(c_int32_t),dimension(1) :: null_pelist

    if (present(pelist)) then
        call _A_SUB(bcast_data,bcast_len,from_pe,_MPP_PARAM, &
                    size(pelist),pelist)
    else
        null_pelist(1) = MPP_C_NULL_RANK_LIST
        call _A_SUB(bcast_data,bcast_len,from_pe,_MPP_PARAM, &
                    1,null_pelist)
    endif

  end subroutine _A_NAME

 !-----------------------------------------------------------------------------

