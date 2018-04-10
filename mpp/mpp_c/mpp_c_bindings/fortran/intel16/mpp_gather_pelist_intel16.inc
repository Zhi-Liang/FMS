 !*****************************************************************************
 !Public facing mpp_gather_pelist routines.

 !NOTE: The variable mpp_context is a private module c_ptr, which is needed by
 !      the c routines.

#ifdef _INTEL16_FBINDINGS
 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer two-dimensional data.
  subroutine mpp_gather_pelist_i32_2D(is,ie,js,je,pelist,array_seg, &
                                      gather_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                    :: is
    integer(INT_KIND),intent(in)                    :: ie
    integer(INT_KIND),intent(in)                    :: js
    integer(INT_KIND),intent(in)                    :: je
    integer(INT_KIND),dimension(:),intent(in)       :: pelist
    integer(c_int32_t),dimension(:,:),intent(in)    :: array_seg
    integer(c_int32_t),dimension(:,:),intent(inout) :: gather_data
    logical(INT_KIND),intent(in)                    :: is_root
    integer(INT_KIND),intent(in),optional           :: ishift
    integer(INT_KIND),intent(in),optional           :: jshift

   !<Local variables
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(array_seg)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_I32_2D: data is not contiguous')
    endif
    if (.not. is_contiguous(gather_data)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_I32_2D: data is not contiguous')
    endif

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

    call mpp_gather_pelist_c_style(is,ie,js,je,1,MPP_INT32,size(pelist), &
                                   pelist,array_seg,size(gather_data,1), &
                                   size(gather_data,2),1,gather_data, &
                                   tmp_is_root,tmp_ishift,tmp_jshift)

  end subroutine mpp_gather_pelist_i32_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit integer three-dimensional data.
  subroutine mpp_gather_pelist_i32_3D(is,ie,js,je,nk,pelist,array_seg, &
                                      gather_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                      :: is
    integer(INT_KIND),intent(in)                      :: ie
    integer(INT_KIND),intent(in)                      :: js
    integer(INT_KIND),intent(in)                      :: je
    integer(INT_KIND),intent(in)                      :: nk
    integer(INT_KIND),dimension(:),intent(in)         :: pelist
    integer(c_int32_t),dimension(:,:,:),intent(in)    :: array_seg
    integer(c_int32_t),dimension(:,:,:),intent(inout) :: gather_data
    logical(INT_KIND),intent(in)                      :: is_root
    integer(INT_KIND),intent(in),optional             :: ishift
    integer(INT_KIND),intent(in),optional             :: jshift

   !<Local variables
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(array_seg)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_I32_3D: data is not contiguous')
    endif
    if (.not. is_contiguous(gather_data)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_I32_3D: data is not contiguous')
    endif

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

    call mpp_gather_pelist_c_style(is,ie,js,je,nk,MPP_INT32,size(pelist), &
                                   pelist,array_seg,size(gather_data,1), &
                                   size(gather_data,2),size(gather_data,3), &
                                   gather_data,tmp_is_root,tmp_ishift, &
                                   tmp_jshift)

  end subroutine mpp_gather_pelist_i32_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real two-dimensional data.
  subroutine mpp_gather_pelist_r32_2D(is,ie,js,je,pelist,array_seg, &
                                      gather_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)               :: is
    integer(INT_KIND),intent(in)               :: ie
    integer(INT_KIND),intent(in)               :: js
    integer(INT_KIND),intent(in)               :: je
    integer(INT_KIND),dimension(:),intent(in)  :: pelist
    real(c_float),dimension(:,:),intent(in)    :: array_seg
    real(c_float),dimension(:,:),intent(inout) :: gather_data
    logical(INT_KIND),intent(in)               :: is_root
    integer(INT_KIND),intent(in),optional      :: ishift
    integer(INT_KIND),intent(in),optional      :: jshift

   !<Local variables
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(array_seg)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_R32_2D: data is not contiguous')
    endif
    if (.not. is_contiguous(gather_data)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_R32_2D: data is not contiguous')
    endif

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

    call mpp_gather_pelist_c_style(is,ie,js,je,1,MPP_REAL32,size(pelist), &
                                   pelist,array_seg,size(gather_data,1), &
                                   size(gather_data,2),1,gather_data, &
                                   tmp_is_root,tmp_ishift,tmp_jshift)

  end subroutine mpp_gather_pelist_r32_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit real three-dimensional data.
  subroutine mpp_gather_pelist_r32_3D(is,ie,js,je,nk,pelist,array_seg, &
                                      gather_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_float,c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                 :: is
    integer(INT_KIND),intent(in)                 :: ie
    integer(INT_KIND),intent(in)                 :: js
    integer(INT_KIND),intent(in)                 :: je
    integer(INT_KIND),intent(in)                 :: nk
    integer(INT_KIND),dimension(:),intent(in)    :: pelist
    real(c_float),dimension(:,:,:),intent(in)    :: array_seg
    real(c_float),dimension(:,:,:),intent(inout) :: gather_data
    logical(INT_KIND),intent(in)                 :: is_root
    integer(INT_KIND),intent(in),optional        :: ishift
    integer(INT_KIND),intent(in),optional        :: jshift

   !<Local variables
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(array_seg)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_R32_3D: data is not contiguous')
    endif
    if (.not. is_contiguous(gather_data)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_R32_3D: data is not contiguous')
    endif

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

    call mpp_gather_pelist_c_style(is,ie,js,je,nk,MPP_REAL32,size(pelist), &
                                   pelist,array_seg,size(gather_data,1), &
                                   size(gather_data,2),size(gather_data,3), &
                                   gather_data,tmp_is_root,tmp_ishift, &
                                   tmp_jshift)

  end subroutine mpp_gather_pelist_r32_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real two-dimensional data.
  subroutine mpp_gather_pelist_r64_2D(is,ie,js,je,pelist,array_seg, &
                                      gather_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                :: is
    integer(INT_KIND),intent(in)                :: ie
    integer(INT_KIND),intent(in)                :: js
    integer(INT_KIND),intent(in)                :: je
    integer(INT_KIND),dimension(:),intent(in)   :: pelist
    real(c_double),dimension(:,:),intent(in)    :: array_seg
    real(c_double),dimension(:,:),intent(inout) :: gather_data
    logical(INT_KIND),intent(in)                :: is_root
    integer(INT_KIND),intent(in),optional       :: ishift
    integer(INT_KIND),intent(in),optional       :: jshift

   !<Local variables
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(array_seg)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_R64_2D: data is not contiguous')
    endif
    if (.not. is_contiguous(gather_data)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_R64_2D: data is not contiguous')
    endif

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

    call mpp_gather_pelist_c_style(is,ie,js,je,1,MPP_REAL64,size(pelist), &
                                   pelist,array_seg,size(gather_data,1), &
                                   size(gather_data,2),1,gather_data, &
                                   tmp_is_root,tmp_ishift,tmp_jshift)

  end subroutine mpp_gather_pelist_r64_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for 64-bit real three-dimensional data.
  subroutine mpp_gather_pelist_r64_3D(is,ie,js,je,nk,pelist,array_seg, &
                                      gather_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_double,c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                  :: is
    integer(INT_KIND),intent(in)                  :: ie
    integer(INT_KIND),intent(in)                  :: js
    integer(INT_KIND),intent(in)                  :: je
    integer(INT_KIND),intent(in)                  :: nk
    integer(INT_KIND),dimension(:),intent(in)     :: pelist
    real(c_double),dimension(:,:,:),intent(in)    :: array_seg
    real(c_double),dimension(:,:,:),intent(inout) :: gather_data
    logical(INT_KIND),intent(in)                  :: is_root
    integer(INT_KIND),intent(in),optional         :: ishift
    integer(INT_KIND),intent(in),optional         :: jshift

   !<Local variables
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(array_seg)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_R64_3D: data is not contiguous')
    endif
    if (.not. is_contiguous(gather_data)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_R64_3D: data is not contiguous')
    endif

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

    call mpp_gather_pelist_c_style(is,ie,js,je,nk,MPP_REAL64,size(pelist), &
                                   pelist,array_seg,size(gather_data,1), &
                                   size(gather_data,2),size(gather_data,3), &
                                   gather_data,tmp_is_root,tmp_ishift, &
                                   tmp_jshift)

  end subroutine mpp_gather_pelist_r64_3D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit logical two-dimensional data.
  subroutine mpp_gather_pelist_l32_2D(is,ie,js,je,pelist,array_seg, &
                                      gather_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                   :: is
    integer(INT_KIND),intent(in)                   :: ie
    integer(INT_KIND),intent(in)                   :: js
    integer(INT_KIND),intent(in)                   :: je
    integer(INT_KIND),dimension(:),intent(in)      :: pelist
    logical(INT_KIND),dimension(:,:),intent(in)    :: array_seg
    logical(INT_KIND),dimension(:,:),intent(inout) :: gather_data
    logical(INT_KIND),intent(in)                   :: is_root
    integer(INT_KIND),intent(in),optional          :: ishift
    integer(INT_KIND),intent(in),optional          :: jshift

   !<Local variables
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(array_seg)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_L32_2D: data is not contiguous')
    endif
    if (.not. is_contiguous(gather_data)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_L32_2D: data is not contiguous')
    endif

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

    call mpp_gather_pelist_c_style(is,ie,js,je,1,MPP_INT32,size(pelist), &
                                   pelist,array_seg,size(gather_data,1), &
                                   size(gather_data,2),1,gather_data, &
                                   tmp_is_root,tmp_ishift,tmp_jshift)

  end subroutine mpp_gather_pelist_l32_2D

 !-----------------------------------------------------------------------------
 !>Wrapper for 32-bit logical three-dimensional data.
  subroutine mpp_gather_pelist_l32_3D(is,ie,js,je,nk,pelist,array_seg, &
                                      gather_data,is_root,ishift,jshift)
    use iso_c_binding, only: c_int32_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                     :: is
    integer(INT_KIND),intent(in)                     :: ie
    integer(INT_KIND),intent(in)                     :: js
    integer(INT_KIND),intent(in)                     :: je
    integer(INT_KIND),intent(in)                     :: nk
    integer(INT_KIND),dimension(:),intent(in)        :: pelist
    logical(INT_KIND),dimension(:,:,:),intent(in)    :: array_seg
    logical(INT_KIND),dimension(:,:,:),intent(inout) :: gather_data
    logical(INT_KIND),intent(in)                     :: is_root
    integer(INT_KIND),intent(in),optional            :: ishift
    integer(INT_KIND),intent(in),optional            :: jshift

   !<Local variables
    integer(c_int32_t) :: tmp_is_root
    integer(c_int32_t) :: tmp_ishift
    integer(c_int32_t) :: tmp_jshift

   !<Make sure that the inputted data is contiguous.
    if (.not. is_contiguous(array_seg)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_L32_3D: data is not contiguous')
    endif
    if (.not. is_contiguous(gather_data)) then
        call mpp_error(FATAL,'MPP_GATHER_PELIST_L32_3D: data is not contiguous')
    endif

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

    call mpp_gather_pelist_c_style(is,ie,js,je,nk,MPP_INT32,size(pelist), &
                                   pelist,array_seg,size(gather_data,1), &
                                   size(gather_data,2),size(gather_data,3), &
                                   gather_data,tmp_is_root,tmp_ishift, &
                                   tmp_jshift)

  end subroutine mpp_gather_pelist_l32_3D

 !-----------------------------------------------------------------------------
 !>Gather data across all ranks on a pelist.
  subroutine mpp_gather_pelist_c_style(is,ie,js,je,nk,mpp_type,n,pelist, &
                                       array_seg,gd1,gd2,gd3,gather_data, &
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
    type(*),dimension(..),intent(in)          :: array_seg
    integer(INT_KIND),intent(in)              :: gd1
    integer(INT_KIND),intent(in)              :: gd2
    integer(INT_KIND),intent(in)              :: gd3
    type(*),dimension(..),intent(inout)       :: gather_data
    integer(c_int32_t),intent(in)             :: is_root
    integer(INT_KIND),intent(in)              :: ishift
    integer(INT_KIND),intent(in)              :: jshift

    call iso_c_mpp_gather_pelist(gather_data,int(gd3,kind=c_size_t), &
                                 int(gd2,kind=c_size_t), &
                                 int(gd1,kind=c_size_t),mpp_type,array_seg, &
                                 0,nk-1,js-1,je-1,is-1,ie-1,pelist, &
                                 int(n,kind=c_size_t),is_root,0,jshift, &
                                 ishift,mpp_context)

  end subroutine mpp_gather_pelist_c_style

 !-----------------------------------------------------------------------------
#endif

 !*****************************************************************************
