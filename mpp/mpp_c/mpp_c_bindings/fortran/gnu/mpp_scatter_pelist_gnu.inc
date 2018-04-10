 !*****************************************************************************
 !Public facing mpp_scatter_pelist routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _GNU_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for two-dimensional data.
  subroutine mpp_scatter_pelist_2D(is,ie,js,je,pelist,array_seg, &
                                   scatter_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)              :: is
    integer(INT_KIND),intent(in)              :: ie
    integer(INT_KIND),intent(in)              :: js
    integer(INT_KIND),intent(in)              :: je
    integer(INT_KIND),dimension(:),intent(in) :: pelist
    class(*),dimension(:,:),intent(inout)     :: array_seg
    class(*),dimension(:,:),intent(in)        :: scatter_data
    logical(INT_KIND),intent(in)              :: is_root
    integer(INT_KIND),intent(in),optional     :: ishift
    integer(INT_KIND),intent(in),optional     :: jshift

   !<Local variables
    integer(c_int32_t) :: mpp_type
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

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

   !<Set the mpp_type value.
    select type (scatter_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_SCATTER_PELIST_2D: unsupported type.')
    end select

    call mpp_scatter_pelist_c_style_array(is,ie,js,je,1,mpp_type,size(pelist), &
                                          pelist,array_seg,size(scatter_data,1), &
                                          size(scatter_data,2),1,scatter_data, &
                                          tmp_is_root,tmp_ishift,tmp_jshift)

  end subroutine mpp_scatter_pelist_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for three-dimensional data.
  subroutine mpp_scatter_pelist_3D(is,ie,js,je,nk,pelist,array_seg, &
                                   scatter_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)              :: is
    integer(INT_KIND),intent(in)              :: ie
    integer(INT_KIND),intent(in)              :: js
    integer(INT_KIND),intent(in)              :: je
    integer(INT_KIND),intent(in)              :: nk
    integer(INT_KIND),dimension(:),intent(in) :: pelist
    class(*),dimension(:,:,:),intent(inout)   :: array_seg
    class(*),dimension(:,:,:),intent(in)      :: scatter_data
    logical(INT_KIND),intent(in)              :: is_root
    integer(INT_KIND),intent(in),optional     :: ishift
    integer(INT_KIND),intent(in),optional     :: jshift

   !<Local variables
    integer(c_int32_t) :: mpp_type
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

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

   !<Set the mpp_type value.
    select type (scatter_data)
    type is (integer(INT_KIND))
        mpp_type = MPP_INT32
    type is (real(FLOAT_KIND))
        mpp_type = MPP_REAL32
    type is (real(DOUBLE_KIND))
        mpp_type = MPP_REAL64
    class default
        call mpp_error(FATAL,'MPP_SCATTER_PELIST_3D: unsupported type.')
    end select

    call mpp_scatter_pelist_c_style_array(is,ie,js,je,nk,mpp_type,size(pelist), &
                                          pelist,array_seg,size(scatter_data,1), &
                                          size(scatter_data,2),size(scatter_data,3), &
                                          scatter_data,tmp_is_root,tmp_ishift, &
                                          tmp_jshift)

  end subroutine mpp_scatter_pelist_3D

 !-----------------------------------------------------------------------------
 !>Gather data across all ranks on a pelist.
  subroutine mpp_scatter_pelist_c_style_array(is,ie,js,je,nk,mpp_type,n,pelist, &
                                              array_seg,sd1,sd2,sd3,scatter_data, &
                                              is_root,ishift,jshift)
    use iso_c_binding, only: c_int32_t,c_size_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)              :: is
    integer(INT_KIND),intent(in)              :: ie
    integer(INT_KIND),intent(in)              :: js
    integer(INT_KIND),intent(in)              :: je
    integer(INT_KIND),intent(in)              :: nk
    integer(c_int32_t),intent(in)             :: mpp_type
    integer(INT_KIND),intent(in)              :: n
    integer(INT_KIND),dimension(n),intent(in) :: pelist
    type(*),dimension(*),intent(inout)        :: array_seg
    integer(INT_KIND),intent(in)              :: sd1
    integer(INT_KIND),intent(in)              :: sd2
    integer(INT_KIND),intent(in)              :: sd3
    type(*),dimension(*),intent(in)           :: scatter_data
    integer(c_int32_t),intent(in)             :: is_root
    integer(INT_KIND),intent(in)              :: ishift
    integer(INT_KIND),intent(in)              :: jshift

    call iso_c_mpp_scatter_pelist_array(scatter_data,int(sd3,kind=c_size_t), &
                                        int(sd2,kind=c_size_t), &
                                        int(sd1,kind=c_size_t),mpp_type,array_seg, &
                                        0,nk-1,js-1,je-1,is-1,ie-1,pelist, &
                                        int(n,kind=c_size_t),is_root,0,jshift, &
                                        ishift,mpp_context)

  end subroutine mpp_scatter_pelist_c_style_array

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
