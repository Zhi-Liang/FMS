 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_scatter_pelist_c_style_i32a(is,ie,js,je,nk,mpp_type,n,pelist, &
                                             array_seg,gd1,gd2,gd3,scatter_data, &
                                             is_root,ishift,jshift)
    use iso_c_binding, only: c_int32_t,c_loc

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                         :: is
    integer(INT_KIND),intent(in)                         :: ie
    integer(INT_KIND),intent(in)                         :: js
    integer(INT_KIND),intent(in)                         :: je
    integer(INT_KIND),intent(in)                         :: nk
    integer(c_int32_t),intent(in)                        :: mpp_type
    integer(INT_KIND),intent(in)                         :: n
    integer(c_int32_t),dimension(n),intent(in)           :: pelist
    integer(c_int32_t),dimension(*),target,intent(in)    :: array_seg
    integer(INT_KIND),intent(in)                         :: gd1
    integer(INT_KIND),intent(in)                         :: gd2
    integer(INT_KIND),intent(in)                         :: gd3
    integer(c_int32_t),dimension(*),target,intent(inout) :: scatter_data
    integer(c_int32_t),intent(in)                        :: is_root
    integer(INT_KIND),intent(in)                         :: ishift
    integer(INT_KIND),intent(in)                         :: jshift

    call mpp_scatter_pelist_c_style(is,ie,js,je,nk,mpp_type,n,pelist, &
                                    c_loc(array_seg),gd1,gd2,gd3, &
                                    c_loc(scatter_data), &
                                    is_root,ishift,jshift)

  end subroutine mpp_scatter_pelist_c_style_i32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_scatter_pelist_c_style_r32a(is,ie,js,je,nk,mpp_type,n,pelist, &
                                             array_seg,gd1,gd2,gd3,scatter_data, &
                                             is_root,ishift,jshift)
    use iso_c_binding, only: c_float,c_int32_t,c_loc

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                    :: is
    integer(INT_KIND),intent(in)                    :: ie
    integer(INT_KIND),intent(in)                    :: js
    integer(INT_KIND),intent(in)                    :: je
    integer(INT_KIND),intent(in)                    :: nk
    integer(c_int32_t),intent(in)                   :: mpp_type
    integer(INT_KIND),intent(in)                    :: n
    integer(c_int32_t),dimension(n),intent(in)      :: pelist
    real(c_float),dimension(*),target,intent(in)    :: array_seg
    integer(INT_KIND),intent(in)                    :: gd1
    integer(INT_KIND),intent(in)                    :: gd2
    integer(INT_KIND),intent(in)                    :: gd3
    real(c_float),dimension(*),target,intent(inout) :: scatter_data
    integer(c_int32_t),intent(in)                   :: is_root
    integer(INT_KIND),intent(in)                    :: ishift
    integer(INT_KIND),intent(in)                    :: jshift

    call mpp_scatter_pelist_c_style(is,ie,js,je,nk,mpp_type,n,pelist, &
                                    c_loc(array_seg),gd1,gd2,gd3, &
                                    c_loc(scatter_data), &
                                    is_root,ishift,jshift)

  end subroutine mpp_scatter_pelist_c_style_r32a

 !-----------------------------------------------------------------------------
 !>Intermediate wrapper.
  subroutine mpp_scatter_pelist_c_style_r64a(is,ie,js,je,nk,mpp_type,n,pelist, &
                                             array_seg,gd1,gd2,gd3,scatter_data, &
                                             is_root,ishift,jshift)
    use iso_c_binding, only: c_double,c_int32_t,c_loc

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)                     :: is
    integer(INT_KIND),intent(in)                     :: ie
    integer(INT_KIND),intent(in)                     :: js
    integer(INT_KIND),intent(in)                     :: je
    integer(INT_KIND),intent(in)                     :: nk
    integer(c_int32_t),intent(in)                    :: mpp_type
    integer(INT_KIND),intent(in)                     :: n
    integer(c_int32_t),dimension(n),intent(in)       :: pelist
    real(c_double),dimension(*),target,intent(in)    :: array_seg
    integer(INT_KIND),intent(in)                     :: gd1
    integer(INT_KIND),intent(in)                     :: gd2
    integer(INT_KIND),intent(in)                     :: gd3
    real(c_double),dimension(*),target,intent(inout) :: scatter_data
    integer(c_int32_t),intent(in)                    :: is_root
    integer(INT_KIND),intent(in)                     :: ishift
    integer(INT_KIND),intent(in)                     :: jshift

    call mpp_scatter_pelist_c_style(is,ie,js,je,nk,mpp_type,n,pelist, &
                                    c_loc(array_seg),gd1,gd2,gd3, &
                                    c_loc(scatter_data), &
                                    is_root,ishift,jshift)

  end subroutine mpp_scatter_pelist_c_style_r64a

 !-----------------------------------------------------------------------------
 !>Gather data across all ranks on a pelist.
  subroutine mpp_scatter_pelist_c_style(is,ie,js,je,nk,mpp_type,n,pelist, &
                                        array_seg,gd1,gd2,gd3,scatter_data, &
                                        is_root,ishift,jshift)
    use iso_c_binding, only: c_ptr,c_int32_t,c_size_t

   !<Inputs/Outputs
    integer(INT_KIND),intent(in)               :: is
    integer(INT_KIND),intent(in)               :: ie
    integer(INT_KIND),intent(in)               :: js
    integer(INT_KIND),intent(in)               :: je
    integer(INT_KIND),intent(in)               :: nk
    integer(c_int32_t),intent(in)              :: mpp_type
    integer(INT_KIND),intent(in)               :: n
    integer(c_int32_t),dimension(n),intent(in) :: pelist
    type(c_ptr)                                :: array_seg
    integer(INT_KIND),intent(in)               :: gd1
    integer(INT_KIND),intent(in)               :: gd2
    integer(INT_KIND),intent(in)               :: gd3
    type(c_ptr)                                :: scatter_data
    integer(c_int32_t),intent(in)              :: is_root
    integer(INT_KIND),intent(in)               :: ishift
    integer(INT_KIND),intent(in)               :: jshift

    call iso_c_mpp_scatter_pelist(scatter_data,int(gd3,kind=c_size_t), &
                                  int(gd2,kind=c_size_t), &
                                  int(gd1,kind=c_size_t),mpp_type,array_seg, &
                                  0,nk-1,js-1,je-1,is-1,ie-1,pelist, &
                                  int(n,kind=c_size_t),is_root,0,jshift, &
                                  ishift,mpp_context)

  end subroutine mpp_scatter_pelist_c_style

 !-----------------------------------------------------------------------------

