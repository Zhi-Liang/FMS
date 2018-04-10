 !-----------------------------------------------------------------------------
 !>mpp_gather_pelist template for 3D array data.
  subroutine _A_NAME(is,ie,js,je,nk,pelist,array_seg, &
                     gather_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_int32_t _C_TYPE

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)               :: is
    integer(INT_KIND),intent(in)               :: ie
    integer(INT_KIND),intent(in)               :: js
    integer(INT_KIND),intent(in)               :: je
    integer(INT_KIND),intent(in)               :: nk
    integer(c_int32_t),dimension(:),intent(in) :: pelist
    _MPP_TYPE,_DIMENSION,intent(in)            :: array_seg
    _MPP_TYPE,_DIMENSION,intent(inout)         :: gather_data
    logical(INT_KIND),intent(in)               :: is_root
    integer(INT_KIND),intent(in),optional      :: ishift
    integer(INT_KIND),intent(in),optional      :: jshift

   !<Local variables
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

   !<Make sure that the inputted data is contiguous.
!   if (.not. is_contiguous(array_seg)) then
!       call mpp_error(FATAL,'_A_NAME: data is not contiguous')
!   endif
!   if (.not. is_contiguous(gather_data)) then
!       call mpp_error(FATAL,'_A_NAME: data is not contiguous')
!   endif

    if (is_root) then
        tmp_is_root = MPP_C_TRUE
    else
        tmp_is_root = MPP_C_FALSE
    endif
    if (present(ishift)) then
        tmp_ishift = ishift
    else
        tmp_ishift = 0
    endif
    if (present(jshift)) then
        tmp_jshift = jshift
    else
        tmp_jshift = 0
    endif

    call _A_SUB(is,ie,js,je,nk,_MPP_PARAM,size(pelist), &
                pelist,array_seg,size(gather_data,1), &
                size(gather_data,2),size(gather_data,3),gather_data, &
                tmp_is_root,tmp_ishift,tmp_jshift)

  end subroutine _A_NAME

 !-----------------------------------------------------------------------------
